# r

# merge post.r output files that differ only in "_<from>-<to>.nc" (e.g. after a run continued under another
# `pathin`) into 1 file with `cdo mergetime`; only if the time axes have no gap and no overlap
# `experiment_aliases`: [optional] list of sets of experiment names to treat as the same, e.g.
#   list(c("AWI-ESM3-CC-LR-CMIP7-Spinup", "AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230")); fout uses the 1st name
# `expout`: [optional; required for > 1 set] experiment name in fout instead
#
# usage: `Rscript merge_chunks.r dir=<dir1>[,<dir2>,...] [exp=<exp1>,<exp2>[,...]] [expout=<name>] [ignore_modes=<mode1>,...] [force=F] [dry=F] [clean=F] [cdo=/path/to/cdo]`
#   each `exp=` is 1 `experiment_aliases` set; variables set before sourcing win over arguments, e.g.
#   `Rscript -e 'ignore_modes <- c("select"); source("merge_chunks.r")' dir=/path/to/dir`

library(ncdf4)

# ---- time axis helpers (copied from plot.r for consistency) ----
known_time_dimnames <- c("time", "Time", "TIME", "mon")

# time dim values + units --> POSIXct if recognized; else unchanged
maybe_time_axis <- function(dimname, vals, units) {
    if (is.na(match(dimname, known_time_dimnames)) || !nzchar(units)) return(vals)
    if (grepl("^\\s*day as %Y%m%d", units)) {
        return(as.POSIXct(strptime(sprintf("%08d", as.integer(floor(vals))), "%Y%m%d", tz="UTC")) + (vals %% 1)*86400)
    }
    m <- regmatches(units, regexec("^\\s*([[:alpha:]]+)\\s+since\\s+(.+?)\\s*$", units))[[1]]
    if (length(m) != 3) return(vals)
    mult <- c(second=1, seconds=1, sec=1, secs=1, s=1, minute=60, minutes=60, min=60, mins=60,
              hour=3600, hours=3600, hr=3600, hrs=3600, h=3600, day=86400, days=86400, d=86400)[tolower(m[2])]
    if (is.na(mult)) return(vals)
    origin_str <- sub("T", " ", trimws(m[3]), fixed=T)
    if (grepl("^[0-9]+$", origin_str)) origin_str <- paste0(origin_str, "-01-01 00:00:00")
    else if (grepl("^[0-9]+-[0-9]{1,2}$", origin_str)) origin_str <- paste0(origin_str, "-01 00:00:00")
    else if (grepl("^[0-9]+-[0-9]{1,2}-[0-9]{1,2}$", origin_str)) origin_str <- paste0(origin_str, " 00:00:00")
    else if (grepl("^[0-9]+-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2}$", origin_str)) origin_str <- paste0(origin_str, ":00")
    origin <- as.POSIXct(strptime(origin_str, format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
    if (is.na(origin)) return(vals)
    origin + vals*mult
} # maybe_time_axis

# 1 file's time axis: list(ok, first, last, diffs (seconds, internal steps))
read_time_bounds <- function(f) {
    nc <- tryCatch(ncdf4::nc_open(f), error=function(e) NULL)
    if (is.null(nc)) return(list(ok=F))
    on.exit(ncdf4::nc_close(nc))
    tdim <- known_time_dimnames[!is.na(match(known_time_dimnames, names(nc$dim)))]
    if (length(tdim) == 0) return(list(ok=F))
    tdim <- tdim[1]
    traw <- nc$dim[[tdim]]$vals
    tunits <- if (is.null(nc$dim[[tdim]]$units)) "" else nc$dim[[tdim]]$units
    tv <- maybe_time_axis(tdim, traw, tunits)
    if (!inherits(tv, "POSIXct") || length(tv) == 0) return(list(ok=F))
    list(ok=T, first=tv[1], last=tv[length(tv)], diffs=if (length(tv) > 1) as.numeric(diff(tv), units="secs") else numeric(0))
} # read_time_bounds

user_set <- function(varname) exists(varname, where=globalenv(), inherits=F)

# ---- "key=value" arguments; a repeated key collects all values ----
cli_args <- commandArgs(trailingOnly=T)
cli <- list()
if (length(cli_args) > 0) {
    is_kv <- grepl("^[A-Za-z_][A-Za-z0-9_]*=", cli_args)
    if (any(!is_kv)) {
        stop("all arguments must be \"key=value\" -- got: ", paste(shQuote(cli_args[!is_kv]), collapse=", "), "; see the header comment for usage")
    }
    cli <- split(sub("^[^=]*=", "", cli_args), sub("=.*$", "", cli_args)) # named list, 1 character vector per distinct key
}
cli_get <- function(key) cli[[key, exact=T]] # exact: `cli$exp` would partially match "expout"
cli_csv <- function(key) if (!is.null(cli_get(key))) unlist(strsplit(cli_get(key), ",", fixed=T)) else NULL
cli_logical <- function(key) {
    v <- cli_get(key)
    if (is.null(v)) return(NULL)
    v <- tolower(v[length(v)])
    if (v %in% c("t", "true", "1")) return(T)
    if (v %in% c("f", "false", "0")) return(F)
    stop("could not parse `", key, "=", v, "` as logical (T/F/TRUE/FALSE/1/0)")
}

# ---- config (override any of these before sourcing, or via `key=value` CLI args above) ----
if (!user_set("dry")) dry <- if (!is.null(cli_logical("dry"))) cli_logical("dry") else F # if T: only print what would be run
if (!user_set("force")) force <- if (!is.null(cli_logical("force"))) cli_logical("force") else F # if T: redo even if the merged output file already exists
if (!user_set("clean")) clean <- if (!is.null(cli_logical("clean"))) cli_logical("clean") else F # if T: remove the chunk files after a successful merge
if (!user_set("cdo")) cdo <- if (!is.null(cli_get("cdo"))) cli_get("cdo")[length(cli_get("cdo"))] else suppressWarnings(system("which cdo", intern=T, ignore.stderr=T))
if (!user_set("ignore_modes")) ignore_modes <- if (!is.null(cli_csv("ignore_modes"))) cli_csv("ignore_modes") else character(0) # skip files with these modes, e.g. c("select")
if (!user_set("experiment_aliases")) { # see header
    experiment_aliases <- if (!is.null(cli_get("exp"))) lapply(cli_get("exp"), function(x) strsplit(x, ",", fixed=T)[[1]]) else list()
}
if (!user_set("expout")) expout <- if (!is.null(cli_get("expout"))) cli_get("expout") else character(0) # see header
if (length(experiment_aliases) > 1 && length(expout) == 0) {
    stop("`experiment_aliases` has ", length(experiment_aliases), " sets (", length(experiment_aliases), " `exp=` occurrences) but no `expout` was given -- ",
         "with more than 1 set, each would otherwise independently fall back to its own 1st member's name (see header comment); give `expout=` ",
         "(1 value applied to every set, or exactly 1 value per set) to make the rename explicit")
}
if (length(expout) > 0) {
    if (length(experiment_aliases) == 0) stop("`expout` given but `experiment_aliases` is empty -- nothing to rename (see header comment)")
    if (length(expout) == 1) expout <- rep(expout, length(experiment_aliases))
    if (length(expout) != length(experiment_aliases)) {
        stop("`expout` has ", length(expout), " value(s) but `experiment_aliases` has ", length(experiment_aliases),
             " set(s) -- give either 1 `expout` value (applied to every set) or exactly 1 per set")
    }
}
if (!user_set("dirs")) dirs <- if (!is.null(cli_get("dir"))) unlist(strsplit(cli_get("dir"), ",", fixed=T)) else character(0)
if (length(dirs) == 0) {
    stop("no directories given -- usage: `Rscript merge_chunks.r dir=<dir1>[,<dir2>,...] [exp=<exp1>,<exp2>[,...]] [expout=<name>] ",
         "[ignore_modes=<mode1>,...] [force=F] [dry=F] [clean=F] [cdo=/path/to/cdo]`; e.g. to also merge across ",
         "differently-named `experiment`s (e.g. after a model version bump mid-run): ",
         "`Rscript merge_chunks.r dir=<dir> exp=exp1,exp1_continued`")
}
missing_dirs <- dirs[!dir.exists(dirs)]
if (length(missing_dirs) > 0) stop("director", if (length(missing_dirs) == 1) "y" else "ies", " not found: ", paste(missing_dirs, collapse=", "))

# -----------------------------------------------------------------------------------------------

run_cmd <- function(cmd) {
    message("run `", cmd, "` ...")
    if (!dry) {
        check <- system(cmd)
        if (check != 0) stop("command failed with exit status ", check)
    }
}

message("search ", length(dirs), " director", if (length(dirs) == 1) "y" else "ies", " recursively for \"*_<from>-<to>.nc\" files ...")
files <- unlist(lapply(dirs, function(d) list.files(d, recursive=T, full.names=T, pattern="_[0-9]+-[0-9]+\\.nc$")))
files <- normalizePath(files, mustWork=F)
message("--> found ", length(files), " file(s)")
if (length(files) == 0) quit(save="no", status=0)

re <- "^(.*)_([0-9]+)-([0-9]+)\\.nc$"
m <- regmatches(basename(files), regexec(re, basename(files)))
ok <- lengths(m) == 4
if (any(!ok)) {
    message("--> ", sum(!ok), " file(s) found but their name does not end in \"_<from>-<to>.nc\" (unexpected) --> ignore:\n",
            paste(files[!ok], collapse="\n"))
}
files <- files[ok]
m <- m[ok]
base <- vapply(m, `[`, character(1), 2)
from <- as.integer(vapply(m, `[`, character(1), 3))
to <- as.integer(vapply(m, `[`, character(1), 4))
dirs_of <- dirname(files)

# group key: `base` with every alias set collapsed to its 1st name (longest first)
group_key <- base
for (seti in seq_along(experiment_aliases)) {
    set <- experiment_aliases[[seti]]
    if (length(set) < 2) next
    canonical <- if (length(expout) > 0) expout[seti] else set[1]
    for (member in set[order(-nchar(set))]) group_key <- gsub(member, canonical, group_key, fixed=T)
}

if (length(ignore_modes) > 0) {
    escape <- function(x) gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x)
    ignore_re <- paste0("(^|_)(", paste(vapply(ignore_modes, escape, character(1)), collapse="|"), ")(_|$)")
    skip <- grepl(ignore_re, base, perl=T)
    if (any(skip)) {
        message("--> ", sum(skip), " file(s) match `ignore_modes` = ", paste(shQuote(ignore_modes), collapse=", "), " --> ignore:\n",
                paste(files[skip], collapse="\n"))
        files <- files[!skip]; base <- base[!skip]; from <- from[!skip]; to <- to[!skip]; dirs_of <- dirs_of[!skip]; group_key <- group_key[!skip]
    }
}
if (length(files) == 0) quit(save="no", status=0)

groups <- split(seq_along(files), paste(dirs_of, group_key, sep="/"))
message("\n--> ", length(groups), " group(s) (same name except \"<from>-<to>\"", if (length(experiment_aliases) > 0) " and configured `experiment_aliases`" else "", ")")

nmerged <- 0L
for (groupi in seq_along(groups)) {
    idx <- groups[[groupi]]
    if (length(idx) < 2) next # nothing to merge
    ord <- order(from[idx])
    idx <- idx[ord]
    gfiles <- files[idx]; gfrom <- from[idx]; gto <- to[idx]; gdir <- dirs_of[idx][1]; gbase <- group_key[idx][1]

    message("\n", "group ", groupi, "/", length(groups), ": ", gbase, " (", length(idx), " chunk(s)) ...")
    print(data.frame(from=gfrom, to=gto, file=basename(gfiles)))

    tb <- lapply(gfiles, read_time_bounds)
    tb_ok <- all(vapply(tb, `[[`, logical(1), "ok"))

    if (!tb_ok) {
        message("--> could not read a recognized time axis from every chunk -- falling back to the (weaker) \"<from>-<to>\" filename check")
        overlap <- gfrom[-1] <= gto[-length(gto)]
        if (any(overlap)) {
            message("--> chunk(s) overlap (e.g. ", gto[which(overlap)[1]], " >= ", gfrom[which(overlap)[1] + 1],
                    ") -- possibly already merged before, or genuinely overlapping -- skip this group")
            next
        }
        gap <- gfrom[-1] > gto[-length(gto)] + 1L
        if (any(gap)) {
            message("--> gap(s) in coverage (e.g. between ", gto[which(gap)[1]], " and ", gfrom[which(gap)[1] + 1], ") -- merging anyway")
        }
    } else {
        # order by the first time step
        tord <- order(vapply(tb, `[[`, double(1), "first"))
        gfiles <- gfiles[tord]; gfrom <- gfrom[tord]; gto <- gto[tord]; tb <- tb[tord]

        typical_dt <- stats::median(unlist(lapply(tb, `[[`, "diffs")))
        if (is.na(typical_dt)) {
            # 1 time step per file: typical step from the gaps between files
            firsts <- vapply(tb, `[[`, double(1), "first")
            typical_dt <- stats::median(diff(firsts))
        }
        firsts <- vapply(tb, `[[`, double(1), "first")
        lasts <- vapply(tb, `[[`, double(1), "last")
        boundary_gap <- firsts[-1] - lasts[-length(lasts)] # seconds, next file's first minus previous file's last

        too_small <- boundary_gap <= typical_dt*0.5 # includes negative/0: overlap
        too_large <- boundary_gap >= typical_dt*1.5 # a whole step or more missing
        if (any(too_small)) {
            i <- which(too_small)[1]
            message("--> actual time steps overlap between \"", basename(gfiles[i]), "\" (ends ", format(as.POSIXct(lasts[i], origin="1970-01-01", tz="UTC")),
                    ") and \"", basename(gfiles[i+1]), "\" (starts ", format(as.POSIXct(firsts[i+1], origin="1970-01-01", tz="UTC")),
                    ") -- possibly already merged before, or genuinely overlapping -- skip this group")
            next
        }
        if (any(too_large)) {
            i <- which(too_large)[1]
            message("--> gap in actual time steps between \"", basename(gfiles[i]), "\" (ends ", format(as.POSIXct(lasts[i], origin="1970-01-01", tz="UTC")),
                    ") and \"", basename(gfiles[i+1]), "\" (starts ", format(as.POSIXct(firsts[i+1], origin="1970-01-01", tz="UTC")),
                    "; typical step ~", round(typical_dt/86400, 1), " day(s)) -- skip this group")
            next
        }
    }

    fout <- paste0(gdir, "/", gbase, "_", gfrom[1], "-", gto[length(gto)], ".nc")
    if (fout %in% gfiles) stop("group \"", gbase, "\": computed `fout` = \"", fout, "\" is 1 of its own input files -- refusing to overwrite it")
    if (file.exists(fout) && !force) {
        message("--> fout = \"", fout, "\" already exists and `force` = F --> skip")
        next
    }
    message("--> fout = \"", fout, "\"")

    run_cmd(paste0(cdo, " -O -s mergetime ", paste(gfiles, collapse=" "), " ", fout))
    if (!dry && !file.exists(fout)) stop("group \"", gbase, "\": `fout` = \"", fout, "\" was not created")
    nmerged <- nmerged + 1L

    if (clean) {
        if (dry) {
            message("--> `clean` = T: would remove the ", length(gfiles), " individual chunk file(s) above (skipped: `dry` = T)")
        } else {
            nt_in <- suppressWarnings(as.integer(system(paste0(cdo, " -s ntime \"", paste(gfiles, collapse="\" \""), "\""), intern=T)))
            nt_out <- suppressWarnings(as.integer(system(paste0(cdo, " -s ntime ", fout), intern=T)))
            if (length(nt_in) == 1 && length(nt_out) == 1 && !is.na(nt_in) && !is.na(nt_out) && nt_in == nt_out) {
                message("--> `clean` = T: ", nt_out, " time step(s) verified in `fout` --> remove the ", length(gfiles), " individual chunk file(s)")
                file.remove(gfiles)
            } else {
                warning("group \"", gbase, "\": `clean` = T but time step count could not be verified (input ", nt_in, " vs. output ", nt_out,
                         ") --> NOT removing the individual chunk files, check `fout` manually", call.=F, immediate.=T)
            }
        }
    }
}

message("\n", nmerged, " group(s) merged", if (dry) " (`dry` = T: nothing actually written)" else "")
