# r

# Merge post.r output files that are "the same thing" (same variable, dtin,
# mode, depth, setup, model, experiment, variant, grid -- i.e. identical filename
# except for the trailing "<from>-<to>" chunk), covering different,
# non-overlapping time chunks, into 1 file spanning their combined range.
# Needed e.g. after an experiment's `outdata` got split across several
# `pathin` roots (a model version bump, a restart under a new run name, ...)
# and post.r was run once per root, leaving 1 output file per root per
# variable instead of 1 continuous file.
#
# Uses `cdo mergetime`, not `cdo cat`: `cat` concatenates files in the exact
# order given on the command line, so a file-discovery bug that hands it
# files out of order silently produces a wrongly-ordered time axis. Confirmed
# by testing: `cdo cat` and `cdo mergetime` on the discovered files is
# equivalent here (no overlaps, nothing to interleave) but with the 2 chunks
# given in reverse, `cat` just concatenates reverse order, while `mergetime`
# sorts by the actual time axis regardless -- cheap insurance for free.
#
# Matching/grouping: a file's "base" is its full name with the trailing
# "_<from>-<to>.nc" stripped off (both integers). Files under the same
# directory with an identical `base` are 1 group to merge; `from`/`to`
# themselves are *not* required to match anything -- that's the whole point.
# Note this means a chunk whose `experiment` segment itself differs (e.g.
# deliberately renamed to "..._v351_from2230" to tell 2 `pathin` roots apart
# in a namelist) will NOT be grouped with the others by default -- grouping
# is purely string-equality on `base`, nothing here understands that 2
# different `experiment` strings might really be 1 continued run, unless
# told so via `experiment_aliases` (see below).
#
# `experiment_aliases`: [optional] a list of character vectors, each 1 set
# of `experiment` name(s) to treat as equivalent for grouping, e.g.
# `list(c("AWI-ESM3-CC-LR-CMIP7-Spinup", "AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230"))`
# -- a file's `base` has every name in a set *after* the 1st literally
# replaced by the set's 1st name (longest name replaced first, so a name
# that is itself a literal prefix of another, as above, cannot partially
# match inside it) before grouping, so otherwise-identical chunks whose
# `experiment` segment differs only by one of these configured aliases are
# grouped (and merged) together, with the resolved `fout` using the set's
# 1st name throughout -- unless overridden by `expout` (below).
#
# `expout`: [optional unless more than 1 `experiment_aliases` set is given,
# see below] a replacement `experiment` name for `fout`, used *instead of*
# each `experiment_aliases` set's own 1st name -- e.g. with `exp=A,B
# expout=C`, a group matched via that set gets `fout` carrying "C", not "A".
# Requires `experiment_aliases` to be set (nothing to anchor the substitution
# on otherwise); either 1 value (applied to every set) or exactly 1 value per
# `experiment_aliases` set (paired in order). With more than 1
# `experiment_aliases` set (i.e. `exp=` given more than once), `expout` is
# *required* -- each set would otherwise independently fall back to its own
# 1st member's name, and with several sets in play at once that default is
# easy to lose track of, so it must be spelled out explicitly.
#
# A group is only ever merged if its chunks have no gap and no overlap,
# checked against the *actual* `time` dim values read from each file (not
# the filename's "<from>-<to>" years, which are only used for grouping and
# initial ordering/labeling) -- a chunk file's name could in principle cover
# a year range without actually containing every time step in it. Per group:
# every file's `time` dim is read (via ncdf4) and converted to a real time
# axis (`maybe_time_axis()`, copied from plot.r); the typical step is the
# median of all *within-file* gaps, pooled across the whole group (robust
# against a short file with too few steps to tell on its own); each
# *between-file* boundary gap is then compared against that: too small
# (<= half the typical step, including negative/overlapping) or too large
# (>= 1.5x the typical step) --> skip the whole group, do not merge. If any
# file's time dim can't be read/converted (e.g. no recognized time dim, or
# unparseable units), this falls back to the old, weaker check based purely
# on the filename's "<from>-<to>" years (overlap --> skip, gap --> merge
# anyway with a warning), with a message explaining why.
#
# Usage: `Rscript merge_chunks.r dir=<dir1>[,<dir2>,...] [exp=<exp1>,<exp2>[,...]] [expout=<name>] [ignore_modes=<mode1>,...] [force=F] [dry=F] [clean=F] [cdo=/path/to/cdo]`
# -- every argument is "key=value", comma-separated lists where noted; each
# `<dir>` is searched *recursively* for chunk files. `exp=` sets 1
# `experiment_aliases` set (see above); give `exp=` more than once for more
# than 1 set. E.g., plain:
#   `Rscript merge_chunks.r dir=/path/to/dir`
# merging across differently-named `experiment`s too (e.g. after a model
# version bump mid-run), with the merged output renamed to a 3rd name:
#   `Rscript merge_chunks.r dir=/path/to/dir exp=AWI-ESM3-CC-LR-CMIP7-Spinup,AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230 expout=AWI-ESM3-CC-LR-CMIP7`
# Any config var can also still be set the usual way before sourcing
# (pre-set variables win over same-named "key=value" args), e.g.:
#   `Rscript -e 'ignore_modes <- c("select"); source("merge_chunks.r")' dir=/path/to/dir`

library(ncdf4)

# ---- time axis helpers (copied from plot.r for consistency) ----
known_time_dimnames <- c("time", "Time", "TIME", "mon")

# raw dim values + their "units" attribute --> POSIXct, if recognized
# ("<unit> since <origin>", or cdo's "day as %Y%m%d"); else unchanged
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

# ---- parse "key=value" CLI args (comma-separated lists where noted); a key
# given more than once collects every occurrence (used by `exp=` for more
# than 1 `experiment_aliases` set) ----
cli_args <- commandArgs(trailingOnly=T)
cli <- list()
if (length(cli_args) > 0) {
    is_kv <- grepl("^[A-Za-z_][A-Za-z0-9_]*=", cli_args)
    if (any(!is_kv)) {
        stop("all arguments must be \"key=value\" -- got: ", paste(shQuote(cli_args[!is_kv]), collapse=", "), "; see the header comment for usage")
    }
    cli <- split(sub("^[^=]*=", "", cli_args), sub("=.*$", "", cli_args)) # named list, 1 character vector per distinct key
}
cli_get <- function(key) cli[[key, exact=T]] # plain `cli[[key]]`/`cli$key` do *partial* (prefix) matching on list names, e.g. `cli$exp` would wrongly match an existing "expout" entry if there's no exact "exp" -- confirmed by testing
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
if (!user_set("clean")) clean <- if (!is.null(cli_logical("clean"))) cli_logical("clean") else F # if T: remove the individual chunk files after a verified-successful merge
if (!user_set("cdo")) cdo <- if (!is.null(cli_get("cdo"))) cli_get("cdo")[length(cli_get("cdo"))] else suppressWarnings(system("which cdo", intern=T, ignore.stderr=T))
if (!user_set("ignore_modes")) ignore_modes <- if (!is.null(cli_csv("ignore_modes"))) cli_csv("ignore_modes") else character(0) # e.g. c("select") -- files whose `mode` filename segment matches 1 of these (as a whole "_<mode>_"/"_<mode>.nc" token) are skipped entirely, as if they didn't exist
if (!user_set("experiment_aliases")) { # e.g. list(c("exp1", "exp1_continued")) -- see header comment; `exp=` CLI arg, 1 set per occurrence
    experiment_aliases <- if (!is.null(cli_get("exp"))) lapply(cli_get("exp"), function(x) strsplit(x, ",", fixed=T)[[1]]) else list()
}
if (!user_set("expout")) expout <- if (!is.null(cli_get("expout"))) cli_get("expout") else character(0) # [optional unless >1 `experiment_aliases` set] see header comment; `expout=` CLI arg, 1 value or 1 per `experiment_aliases` set
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

# group key: `base`, with every `experiment_aliases` set collapsed to its
# own 1st name (longest member first, so no name can partially match inside
# a longer one that contains it, e.g. as a prefix)
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
        # order by actual first time step, not the filename -- then re-derive
        # gfrom/gto/gfiles to match, so `fout`'s name and everything below
        # stays consistent with however the real data turned out to be sorted
        tord <- order(vapply(tb, `[[`, double(1), "first"))
        gfiles <- gfiles[tord]; gfrom <- gfrom[tord]; gto <- gto[tord]; tb <- tb[tord]

        typical_dt <- stats::median(unlist(lapply(tb, `[[`, "diffs")))
        if (is.na(typical_dt)) {
            # every file has just 1 time step -- estimate the typical step from
            # the gaps between consecutive files' single timestamps instead
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
