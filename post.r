# r

# generic post-processing of model output with cdo: select a variable (mode = "select") or apply a cdo
# statistic (mode = "fldmean", "fldint", "zonmean", "yearmean", ...)
# usage: set `jobs` (and optional config) in a namelist.post.r, then source this file
#
# job fields ([o]: optional):
#   pathin     input dir, searched recursively; brace alternatives allowed, e.g. ".../outdata/{echam,jsbach}"
#   fpattern   input file pattern with "<year>", "<year>"+"<mon>" or "<from>"+"<to>"; "*" allowed;
#              may include trailing dir levels, e.g. "<year>0101-<year>1231/cLand_yearly.nc"
#   from, to   [o] year range; default: range of the found files
#   varnamein  [o] default: 1st variable of the 1st input file
#   varnameout [o] default: varnamein
#   mode       "select" or a cdo statistic, e.g. "fldmean"
#   mode_name  [o] name of `mode` in fout/pathout, e.g. "fldmean" for mode = "fldsum" with weights; default: mode
#   mode_args  [o] e.g. mode = "zonmean", mode_args = "zonal_1" --> "-zonmean,zonal_1"
#   dtin       [o] <dtin> in fout; default: inferred from the input; set if cdo_pre changes the frequency
#   level      [o] level(s) to select, e.g. "0,10,50"
#   code       [o] cdo code table (`cdo -t`), or a pattern like fpattern
#   cdo_pre    [o] cdo operators before `mode`, e.g. "-remapycon,r180x90 -setgrid,mesh.nc"
#   cdo_post   [o] cdo operators after `mode`, e.g. "-setunit,ppm -mulc,2"; run per chunk if the files are split
#   setup      model setup, e.g. "AWI-ESM-1-REcoM"
#   model      [o] model component, e.g. "recom"
#   experiment e.g. "esm-ssp585"
#   variant    [o] default: "r1i1p1f1"
#   grid       [o] default: "gn"
#   pathout    [o] default: pathin; "/<setup>/<mode_name>/<varnameout>" is appended
#   fout       [o] default: <varnameout>_<dtin>_<mode_name>[_<depth>]_<setup>[_<model>]_<experiment>_<variant>_<grid>_<from>-<to>.nc
#   do         [o] F: skip the job; default: T
# if `save_fout_an`, the annual mean of a sub-annual time series fout is also saved (<dtin> = "year")

message("\n****************** post.r start ******************")

# ---- config (override in the namelist) ----
# search only globalenv: `force` is also a base function
user_set <- function(varname) exists(varname, where=globalenv(), inherits=F)

# no partial matching of list names (e.g. pathin/pathout); NULL = not given
job_get <- function(job, field, default=NULL) if (field %in% names(job) && !is.null(job[[field]])) job[[field]] else default
if (!user_set("dry")) dry <- F # if T: only print what would be run
if (!user_set("force")) force <- F # if T: redo even if fout already exists
if (!user_set("clean")) clean <- T # if T: remove temporary chunk/select files
if (!user_set("save_fout_an")) save_fout_an <- T # if T: also save the annual mean (`fout_an`) of a sub-annual time series `fout`
if (!user_set("cdo")) {
    cdo <- suppressWarnings(system("which cdo", intern=T, ignore.stderr=T))
    if (length(cdo) != 1 || !nzchar(cdo)) stop("could not find `cdo` on PATH; set `cdo <- \"/path/to/cdo\"` in the namelist")
}
if (!user_set("cdo_nthreads")) cdo_nthreads <- max(1, trunc(0.75*as.integer(system("nproc", intern=T))))
if (!user_set("jobs")) stop("must provide a `jobs` list before sourcing post.r; see namelist.post_fesom_recom.r for an example")

# every job a named list of known fields
known_fields <- c("pathin", "fpattern", "from", "to", "varnamein", "varnameout",
                  "mode", "mode_name", "mode_args", "dtin", "level", "code", "cdo_pre", "cdo_post",
                  "setup", "model", "experiment", "variant", "grid", "pathout", "fout", "do")
if (!is.list(jobs) || length(jobs) == 0) stop("`jobs` must be a non-empty list of jobs")
for (jobi in seq_along(jobs)) {
    if (!is.list(jobs[[jobi]]) || is.null(names(jobs[[jobi]])) || any(!nzchar(names(jobs[[jobi]])))) {
        stop("job ", jobi, " is not a named list of fields (got ", class(jobs[[jobi]])[1], "); ",
             "append jobs as `jobs <- c(jobs, list(list(...)))` or `jobs[[length(jobs)+1]] <- list(...)`")
    }
    unknown <- setdiff(names(jobs[[jobi]]), known_fields)
    if (length(unknown) > 0) stop("job ", jobi, " has unknown field(s): ", paste(unknown, collapse=", "),
                                  "; known: ", paste(known_fields, collapse=", "))
    do <- job_get(jobs[[jobi]], "do", T)
    if (!is.logical(do) || length(do) != 1 || is.na(do)) stop("job ", jobi, "'s `do` must be T or F")
}

# ---- cdo version (need to know if selection+statistic can be chained: cdo >= 1.9.4) ----
cdo_version_str <- system(paste0(cdo, " --version"), intern=T, ignore.stderr=T)[1] # e.g. "Climate Data Operators version 2.6.0 (...)"
cdo_version <- regmatches(cdo_version_str, regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", cdo_version_str))
if (length(cdo_version) != 1) stop("could not parse a version number (X.Y.Z) from `cdo --version` output: \"", cdo_version_str, "\"")
cdo_version <- base::numeric_version(cdo_version)
cdo_can_chain <- cdo_version >= base::numeric_version("1.9.4")
message("cdo = ", cdo, " (version ", cdo_version, ", ", if (!cdo_can_chain) "cannot" else "can", " chain selection+statistic)")

# max command length: system() runs `sh -c "<cmd>"`, 1 argument limited to MAX_ARG_STRLEN = 131072
cdo_nchar_max_arglist <- trunc(0.9*min(as.integer(system("getconf ARG_MAX", intern=T)), 131072)) # 10% safety margin

##################################################################################

# run 1 cdo command, respecting `dry`; stop on nonzero exit
# `files`: input files in `cmd`, printed as only the first and last 2
run_cmd <- function(cmd, files=NULL) {
    msg <- cmd
    if (length(files) > 4) {
        short <- paste(c(files[1:2], paste0("... (", length(files) - 4, " more files) ..."), utils::tail(files, 2)), collapse=" ")
        msg <- sub(paste(files, collapse=" "), short, msg, fixed=T)
    }
    message("run `", msg, "` ...")
    if (!dry) {
        check <- system(cmd)
        if (check != 0) stop("command failed with exit status ", check)
    }
}

# cdo statistic operator, e.g. "-zonmean,zonal_1"; "" for "select"
mode_to_calc <- function(mode, mode_args) {
    if (mode == "select") "" else paste0("-", mode, if (nzchar(mode_args)) paste0(",", mode_args) else "")
}
# year filter only for "<from>"/"<to>" chunk files; with "<year>" files it can crash cdo when chunked
sel_op <- function(varnamein, level, from, to, needs_year_filter) {
    paste0("-select,name=", varnamein, if (nzchar(level)) paste0(",level=", level) else "",
           if (needs_year_filter) paste0(",year=", from, "/", to) else "")
}

# temporal frequency of the input: median time step of the 1st file, else ntime*nfiles/nyears
get_dt_from_file <- function(files, from, to) {
    steps_per_year <- NA_real_

    ts <- suppressWarnings(system(paste0(cdo, " -s showtimestamp ", files[1]), intern=T))
    ts <- trimws(strsplit(paste(ts, collapse=" "), "[[:space:]]+")[[1]])
    ts <- ts[nzchar(ts)]
    ts <- tryCatch(as.POSIXct(ts, tz="UTC"), error=function(e) NULL)
    if (!is.null(ts) && length(ts) >= 2 && !anyNA(ts)) {
        step_days <- stats::median(diff(as.numeric(ts)))/86400
        if (step_days > 0) steps_per_year <- 365.25/step_days
    }

    if (is.na(steps_per_year)) { # tier 2: 1st file's ntime * nfiles, over the requested years
        ntime1 <- suppressWarnings(as.integer(system(paste0(cdo, " -s ntime ", files[1]), intern=T)))
        n_years <- to - from + 1
        if (length(ntime1) == 1 && !is.na(ntime1) && ntime1 > 0 && n_years > 0) {
            steps_per_year <- ntime1*length(files)/n_years
        }
    }
    if (is.na(steps_per_year) || steps_per_year <= 0) return("unknown")

    known <- c(year=1, mon=12, day=365.25, "6hr"=4*365.25, "3hr"=8*365.25, hr=24*365.25)
    ratios <- abs(log(steps_per_year/known))
    besti <- which.min(ratios)
    if (ratios[besti] <= log(1.15)) names(known)[besti] else paste0(round(steps_per_year), "ts_yr-1")
} # get_dt_from_file

# TRUE if `f` is a time series (1 grid point and 1 level for every variable)
# with more time steps than years, i.e. finer than annual
is_subannual_timeseries <- function(f) {
    cdo_int <- function(op) suppressWarnings(as.integer(system(paste0(cdo, " -s ", op, " ", f), intern=T)))
    ngp <- cdo_int("ngridpoints")
    nlev <- cdo_int("nlevel")
    ntime <- cdo_int("ntime")
    years <- suppressWarnings(system(paste0(cdo, " -s showyear ", f), intern=T))
    years <- unique(strsplit(trimws(paste(years, collapse=" ")), "[[:space:]]+")[[1]])
    length(ngp) > 0 && !anyNA(ngp) && all(ngp == 1) &&
        length(nlev) > 0 && !anyNA(nlev) && all(nlev == 1) &&
        length(ntime) == 1 && !is.na(ntime) && ntime > length(years)
} # is_subannual_timeseries

# escape regex metacharacters
regex_escape <- function(x) {
    for (s in c("\\", ".", "^", "$", "|", "(", ")", "[", "]", "{", "}", "*", "+", "?")) x <- gsub(s, paste0("\\", s), x, fixed=T)
    x
}

# "a/{x,y}/b" --> c("a/x/b", "a/y/b")
brace_expand <- function(x) {
    out <- c()
    for (p in x) {
        m <- regexpr("\\{[^{}]*\\}", p)
        if (m == -1) {
            out <- c(out, p)
        } else {
            alts <- strsplit(substr(p, m + 1, m + attr(m, "match.length") - 2), ",", fixed=T)[[1]]
            out <- c(out, brace_expand(paste0(substr(p, 1, m - 1), alts, substr(p, m + attr(m, "match.length"), nchar(p)))))
        }
    }
    out
} # brace_expand

# ".../outdata/{echam,jsbach}" --> ".../outdata"
pathin_base <- function(pathin) sub("/*$", "", sub("\\{.*$", "", pathin))

# `code` file or pattern --> the code table closest in name to the 1st input
# file (no stat/read of all matches: slow on cold lustre)
codes_resolve <- function(codes, files, jobi) {
    if (file.exists(codes) && !dir.exists(codes)) return(codes)
    dir <- if (grepl("/", codes, fixed=T)) dirname(codes) else dirname(files[1])
    re <- paste0("^", regex_escape(basename(codes)), "$")
    re <- gsub("<mon>", "[0-9]{2}", re, fixed=T)
    re <- gsub("<[^<>]*>", "[0-9]+", re) # <year>, <from>, <to> (and any other tag)
    re <- gsub("\\*", ".*", re, fixed=T)
    message("--> run base::list.files(\"", dir, "\", full.names=T, pattern=\"", re, "\") ...")
    cands <- base::list.files(dir, full.names=T, pattern=re)
    if (length(cands) == 0) stop("job ", jobi, "'s `code` = \"", codes, "\" is not an existing file, and as a pattern (regex \"", re,
                                 "\") it matches no file in \"", dir, "\"")
    target <- basename(files[1])
    common_prefix <- sapply(basename(cands), function(x) {
        n <- min(nchar(x), nchar(target)); i <- 0
        while (i < n && substr(x, i + 1, i + 1) == substr(target, i + 1, i + 1)) i <- i + 1
        i
    })
    chosen <- cands[which.max(common_prefix)]
    message("--> `code` = \"", codes, "\" --> ", length(cands), " matching code table(s) in \"", dir, "\" --> use \"", chosen, "\"")
    chosen
} # codes_resolve

# print the first/last `n` file names
print_head_tail <- function(files, n=13) {
    nf <- length(files)
    if (nf <= 2*n) {
        message(paste(files, collapse="\n"))
    } else {
        message(paste(utils::head(files, n), collapse="\n"))
        message("... (", nf - 2*n, " more) ...")
        message(paste(utils::tail(files, n), collapse="\n"))
    }
} # print_head_tail

# input files of 1 job; error if ambiguous or incomplete
find_input_files <- function(pathin, fpattern, from, to, jobi) {
    # with "/" in `fpattern`, match it against the last `nparts` path components
    nparts <- length(strsplit(fpattern, "/", fixed=T)[[1]])
    path_tail <- function(f) vapply(strsplit(f, "/", fixed=T), function(p) paste(utils::tail(p, nparts), collapse="/"), character(1))
    is_year <- grepl("<year>", fpattern, fixed=T)
    has_mon <- grepl("<mon>", fpattern, fixed=T)
    is_range <- grepl("<from>", fpattern, fixed=T) && grepl("<to>", fpattern, fixed=T)
    if (!is_year && !is_range) {
        stop("job ", jobi, "'s `fpattern` = \"", fpattern, "\" has neither a \"<year>\" placeholder nor both \"<from>\" and \"<to>\" placeholders")
    }
    if (has_mon && !is_year) {
        stop("job ", jobi, "'s `fpattern` = \"", fpattern, "\" has a \"<mon>\" placeholder without a \"<year>\" placeholder; ",
             "\"<mon>\" is only supported combined with \"<year>\"")
    }

# regex from fpattern: <year>, <from>, <to> --> [0-9]+; <mon> --> [0-9]{2}
    if (is_year && has_mon) {
# capture group order of year and month
        year_before_mon <- regexpr("<year>", fpattern, fixed=T) < regexpr("<mon>", fpattern, fixed=T)
        year_group <- if (year_before_mon) 2 else 3
        mon_group <- if (year_before_mon) 3 else 2
        re <- paste0("^", regex_escape(fpattern), "$")
        re <- sub("<year>", "([0-9]+)", re, fixed=T)
        re <- sub("<mon>", "([0-9]{2})", re, fixed=T)
    } else if (is_year) {
        re <- paste0("^", sub("<year>", "([0-9]+)", regex_escape(fpattern), fixed=T), "$")
    } else {
        re <- paste0("^", regex_escape(fpattern), "$")
        re <- sub("<from>", "([0-9]+)", re, fixed=T)
        re <- sub("<to>", "([0-9]+)", re, fixed=T)
    }
# other <...> tags: wildcard
    re <- gsub("<[^<>]*>", ".*", re)

# "*" before a placeholder must not end in a digit; non-capturing, so perl=T below
    re <- gsub("\\\\\\*(?=\\(\\[0-9\\])", "(?:.*[^0-9])?", re, perl=T)
    re <- gsub("\\*", ".*", re, fixed=T)

    # list.files() only takes extended (TRE) regexes, which lack "(?:": list
    # with a capturing copy (same matches), then keep the exact perl matches
    re_tre <- gsub("(?:", "(", re, fixed=T)
    if (nparts > 1) re_tre <- paste0("^", sub(".*/", "", re_tre)) # list.files() matches the basename only
    pathins <- brace_expand(pathin)
    missing_dirs <- pathins[!dir.exists(pathins)]
    if (length(missing_dirs) > 0) message("--> `pathin` director", if (length(missing_dirs) == 1) "y" else "ies",
                                          " not found --> skip:\n", paste(missing_dirs, collapse="\n"))
    pathins <- pathins[dir.exists(pathins)]
# `find` instead of list.files(recursive=T), which is very slow on lustre
    message("--> run `find pathins -mindepth 1 ! -type d` and keep basenames matching \"", re_tre, "\" in `pathin` director",
            if (length(pathins) == 1) "y" else "ies", " (specify a clearer `pathin` if this takes too long). pathins:\n",
            paste(pathins, collapse="\n"))
    all_files <- if (length(pathins) > 0) {
        system2("find", c(shQuote(pathins), "-mindepth", "1", "!", "-type", "d"), stdout=T)
    } else {
        character(0)
    }
    all_files <- sort(all_files[grepl(re_tre, basename(all_files))])
    all_files <- all_files[grepl(re, path_tail(all_files), perl=T)]
    if (length(all_files) == 0) { # NULL --> caller skips this job
        warning("job ", jobi, ": no files matching \"", re, "\" under \"", pathin, "\" --> skip job", call.=F, immediate.=T)
        return(NULL)
    }
    dups <- unique(path_tail(all_files)[duplicated(path_tail(all_files))])
    if (length(dups) > 0) {
        stop("job ", jobi, ": ", length(dups), " file name(s) found more than once under `pathin` = \"", pathin, "\", e.g.:\n",
             paste(all_files[path_tail(all_files) == dups[1]], collapse="\n"))
    }
    message("--> found ", length(all_files), " file(s)")
    print_head_tail(all_files)
    all_basenames <- path_tail(all_files) # = basename(all_files) without "/" in `fpattern`

    from_given <- !is.null(from)
    to_given <- !is.null(to)

    if (is_year && has_mon) {
        if (!from_given || !to_given) {
# derive from/to from the found files; same year+month twice: ambiguous
            mm <- regmatches(all_basenames, regexec(re, all_basenames, perl=T))
            file_years <- as.integer(vapply(mm, `[`, character(1), year_group))
            file_mons <- as.integer(vapply(mm, `[`, character(1), mon_group))
            key <- paste(file_years, file_mons)
            dup <- key %in% key[duplicated(key)]
            if (any(dup)) {
                stop("job ", jobi, ": ambiguous -- more than 1 file found recursively under \"", pathin, "\" for the same year+month, e.g.:\n",
                     paste(all_files[dup], collapse="\n"))
            }
            if (!from_given) from <- min(file_years)
            if (!to_given) to <- max(file_years)
            message("--> `from`/`to` not (both) given --> derived from found \"<year>\"+\"<mon>\" files: ", from, "-", to)
        }

        years <- from:to
        combos <- expand.grid(mon=1:12, year=years)
        files <- rep(NA_character_, nrow(combos))
        missing <- character(0)
# match by the year/month parsed from the names
        mm <- regmatches(all_basenames, regexec(re, all_basenames, perl=T))
        all_years <- as.integer(vapply(mm, `[`, character(1), year_group))
        all_mons <- as.integer(vapply(mm, `[`, character(1), mon_group))
        for (i in seq_len(nrow(combos))) {
            target <- gsub("<year>", combos$year[i], fpattern, fixed=T)
            target <- gsub("<mon>", sprintf("%02d", combos$mon[i]), target, fixed=T)
            hits <- all_files[all_years == combos$year[i] & all_mons == combos$mon[i]]
            if (length(hits) == 0) {
                missing <- c(missing, paste0(combos$year[i], "-", sprintf("%02d", combos$mon[i])))
            } else if (length(hits) > 1) {
                stop("job ", jobi, ": ambiguous -- ", length(hits), " files matching \"", target, "\" found recursively under \"", pathin,
                     "\":\n", paste(hits, collapse="\n"))
            } else {
                files[i] <- hits
            }
        } # for i combos
        if (length(missing) > 0) {
            stop("job ", jobi, ": ", length(missing), "/", nrow(combos), " expected input file(s) not found (searched recursively under \"",
                 pathin, "\"), e.g. missing year-month(s): ", paste(head(missing, 5), collapse=", "))
        }
        ord <- order(combos$year, combos$mon)
        files <- files[ord]
        message("--> found all ", length(files), " expected input file(s) via \"<year>\"+\"<mon>\" (", years[1], "-01 to ", years[length(years)], "-12)")
        print_head_tail(files)
        return(list(files=files, from=from, to=to, needs_year_filter=F)) # each file already exactly matches its own year+month
    }

    if (is_year) {
        if (!from_given || !to_given) {
# derive from/to from the found files
            file_years <- as.integer(vapply(regmatches(all_basenames, regexec(re, all_basenames, perl=T)), `[`, character(1), 2))
            dup <- file_years %in% file_years[duplicated(file_years)]
            if (any(dup)) {
                stop("job ", jobi, ": ambiguous -- more than 1 file found recursively under \"", pathin, "\" for the same year, e.g.:\n",
                     paste(all_files[dup], collapse="\n"))
            }
            if (!from_given) from <- min(file_years)
            if (!to_given) to <- max(file_years)
            message("--> `from`/`to` not (both) given --> derived from found \"<year>\" files: ", from, "-", to)
        }

        years <- from:to
        files <- rep(NA_character_, length(years))
        missing_years <- integer(0)
        # match by the year read from each name (see the "<mon>" case above)
        all_years <- as.integer(vapply(regmatches(all_basenames, regexec(re, all_basenames, perl=T)), `[`, character(1), 2))
        for (i in seq_along(years)) {
            target <- gsub("<year>", years[i], fpattern, fixed=T)
            hits <- all_files[all_years == years[i]]
            if (length(hits) == 0) {
                missing_years <- c(missing_years, years[i])
            } else if (length(hits) > 1) {
                stop("job ", jobi, ": ambiguous -- ", length(hits), " files matching \"", target, "\" found recursively under \"", pathin,
                     "\":\n", paste(hits, collapse="\n"))
            } else {
                files[i] <- hits
            }
        } # for i years
        if (length(missing_years) > 0) {
            stop("job ", jobi, ": ", length(missing_years), "/", length(years), " expected input file(s) not found (searched recursively under \"",
                 pathin, "\"), e.g. missing year(s): ", paste(head(missing_years, 5), collapse=", "))
        }
        message("--> found all ", length(files), " expected input file(s) via \"<year>\" (", years[1], " to ", years[length(years)], ")")
        print_head_tail(files)
        return(list(files=files, from=from, to=to, needs_year_filter=F)) # each file already exactly matches its own year
    }

    if (is_range) {
        m <- regmatches(all_basenames, regexec(re, all_basenames, perl=T))
        file_from <- as.integer(vapply(m, `[`, character(1), 2))
        file_to <- as.integer(vapply(m, `[`, character(1), 3))

# same chunk twice: ambiguous
        key <- paste(file_from, file_to)
        dup <- key %in% key[duplicated(key)]
        if (any(dup)) {
            stop("job ", jobi, ": ambiguous -- more than 1 file found recursively under \"", pathin, "\" for the same year range, e.g.:\n",
                 paste(all_files[dup], collapse="\n"))
        }

        if (!from_given || !to_given) {
# derive missing bound(s) from the found chunks
            if (!from_given) from <- min(file_from)
            if (!to_given) to <- max(file_to)
            message("--> `from`/`to` not (both) given --> derived from found \"<from>\"/\"<to>\" files: ", from, "-", to)
        }
        keep <- file_to >= from & file_from <= to # overlaps the (given or derived) requested range
        if (!any(keep)) {
            stop("job ", jobi, ": found ", length(all_files), " file(s) via \"<from>\"/\"<to>\", but none overlap the requested range ",
                 from, "-", to, "; found range(s): ", paste(paste0(file_from, "-", file_to), collapse=", "))
        }
        ord <- order(file_from[keep])
        files <- all_files[keep][ord]
        file_from <- file_from[keep][ord]
        file_to <- file_to[keep][ord]

        covered <- unique(unlist(Map(":", file_from, file_to)))
        missing_years <- setdiff(from:to, covered)
        if (length(missing_years) > 0) {
            stop("job ", jobi, ": file(s) found via \"<from>\"/\"<to>\" do not fully cover the ", if (from_given && to_given) "requested" else "derived",
                 " range ", from, "-", to, "; missing year(s) ", min(missing_years), "-", max(missing_years),
                 " (found file range(s): ", paste(paste0(file_from, "-", file_to), collapse=", "), ")")
        }

        message("--> found ", length(files), " file(s) via \"<from>\"/\"<to>\" covering ", from, "-", to,
                " (file range(s): ", paste(paste0(file_from, "-", file_to), collapse=", "), ")")
        print_head_tail(files)
        return(list(files=files, from=from, to=to, needs_year_filter=T)) # a chunk file may cover *more* years than requested
    }
} # find_input_files

# global options; `-t` must precede the operators, `-f nc` to write code-table input as netcdf
cdo_prefix <- function(codes) {
    paste0(cdo, " --pedantic -P ", cdo_nthreads, if (nzchar(codes)) paste0(" -t ", codes, " -f nc") else "")
}

# 1 cdo call: select, cdo_pre, mode, cdo_post, setname; 2 calls for cdo < 1.9.4
cdo_select_calc_1cmd <- function(files, varnamein, varnameout, mode, mode_args, level, from, to, needs_year_filter, codes, cdo_pre, cdo_post, fout_i) {
    calc <- mode_to_calc(mode, mode_args)
    sel <- sel_op(varnamein, level, from, to, needs_year_filter)
    filestr <- paste(files, collapse=" ")

    if (cdo_can_chain) {
        cmd <- paste0(cdo_prefix(codes), " ", cdo_post, " -setname,", varnameout, " ", calc, " ", cdo_pre, " ", sel, " ", filestr, " ", fout_i)
        cmd <- gsub("[[:space:]]+", " ", cmd) # tidy up in case any piece was ""
        run_cmd(cmd, files)
    } else if (mode == "select" && !nzchar(cdo_post)) {
        # 1 call suffices
        run_cmd(gsub("[[:space:]]+", " ", paste0(cdo_prefix(codes), " -setname,", varnameout, " ", cdo_pre, " ", sel, " ", filestr, " ", fout_i)), files)
    } else {
        tmpfile <- paste0(tools::file_path_sans_ext(fout_i), "_tmp_select_", Sys.getpid(), ".nc")
        run_cmd(gsub("[[:space:]]+", " ", paste0(cdo_prefix(codes), " ", cdo_pre, " ", sel, " ", filestr, " ", tmpfile)), files)
        run_cmd(gsub("[[:space:]]+", " ", paste0(cdo, " --pedantic -P ", cdo_nthreads, " ", cdo_post, " -setname,", varnameout, " ", calc, " ", tmpfile, " ", fout_i)))
        if (clean) run_cmd(paste0("rm ", tmpfile))
    }
} # cdo_select_calc_1cmd

# as cdo_select_calc_1cmd(), split into chunks if the command is too long, then `cdo cat`
cdo_select_calc <- function(files, varnamein, varnameout, mode, mode_args, level, from, to, needs_year_filter, codes, cdo_pre, cdo_post, fout) {
    nchar_fixed <- nchar(paste0(cdo_prefix(codes), " ", cdo_post, " -setname,", varnameout, " ", mode_to_calc(mode, mode_args), " ", cdo_pre,
                                " ", sel_op(varnamein, level, from, to, needs_year_filter), " ", fout))
    nchar_per_file <- nchar(files) + 1L # +1 for the separating space
    nchar_total <- nchar_fixed + sum(nchar_per_file)

    if (nchar_total <= cdo_nchar_max_arglist) { # fits in 1 command -- the common case
        cdo_select_calc_1cmd(files, varnamein, varnameout, mode, mode_args, level, from, to, needs_year_filter, codes, cdo_pre, cdo_post, fout)
        return(invisible())
    }

    nfiles_per_chunk <- max(1, floor((cdo_nchar_max_arglist - nchar_fixed) / max(nchar_per_file)))
    chunk_starts <- seq(1, length(files), by=nfiles_per_chunk)
    nchunks <- length(chunk_starts)
    message("\ninput file list is ", nchar_total, " characters, above the ", cdo_nchar_max_arglist,
            " character limit --> split into ", nchunks, " chunks of up to ", nfiles_per_chunk, " files each")

    fout_chunks <- paste0(tools::file_path_sans_ext(fout), "_chunk_", seq_len(nchunks), "_of_", nchunks, ".nc")
    for (chunki in seq_len(nchunks)) {
        message("\nchunk ", chunki, "/", nchunks, " ...")
        i_from <- chunk_starts[chunki]
        i_to <- min(i_from + nfiles_per_chunk - 1, length(files))
        cdo_select_calc_1cmd(files[i_from:i_to], varnamein, varnameout, mode, mode_args, level, from, to, needs_year_filter, codes, cdo_pre, cdo_post, fout_chunks[chunki])
    }

    message("\ncat ", nchunks, " chunks together ...")
    run_cmd(paste0(cdo, " --pedantic cat ", paste(fout_chunks, collapse=" "), " ", fout), fout_chunks)
    if (clean) run_cmd(paste0("rm ", paste(fout_chunks, collapse=" ")))

} # cdo_select_calc

##################################################################################

message("\nrun ", length(jobs), " job", if (length(jobs) != 1) "s", " ...")
skipped <- c() # jobs without input files
not_done <- c() # jobs with `do` = F
for (jobi in seq_along(jobs)) {

    job <- jobs[[jobi]]
    message("\n************************************************************")
    message("job ", jobi, "/", length(jobs), ": ", if (is.null(job_get(job, "varnamein"))) "<varnamein not given>" else job_get(job, "varnamein"),
            " (mode = \"", job_get(job, "mode"), "\") from \"", job_get(job, "pathin"), "\" ...")
    if (!job_get(job, "do", T)) {
        message("--> `do` = F --> skip")
        not_done <- c(not_done, jobi)
        next
    }

# required fields
    required <- c("pathin", "fpattern", "mode", "setup", "experiment")
    missing <- required[!sapply(required, function(f) !is.null(job_get(job, f)))]
    if (length(missing) > 0) stop("job ", jobi, " is missing required field(s): ", paste(missing, collapse=", "))

    mode <- job_get(job, "mode")
    mode_args <- job_get(job, "mode_args", "")
    mode_name <- job_get(job, "mode_name", mode)
    level <- job_get(job, "level", "")
    codes <- job_get(job, "code", "") # resolved below
    cdo_pre <- job_get(job, "cdo_pre", "")
    cdo_post <- job_get(job, "cdo_post", "")
    variant <- job_get(job, "variant", "r1i1p1f1")
    grid <- job_get(job, "grid", "gn")

# input files, rediscovered for every job
    message("run `find_input_files()` ...")
    found <- find_input_files(job_get(job, "pathin"), job_get(job, "fpattern"), job_get(job, "from"), job_get(job, "to"), jobi)
    if (is.null(found)) { # no input files
        skipped <- c(skipped, jobi)
        next
    }
    files <- found$files
    from <- found$from
    to <- found$to
    needs_year_filter <- found$needs_year_filter
    if (nzchar(codes)) codes <- codes_resolve(codes, files, jobi)

    varnamein <- job_get(job, "varnamein")
    if (is.null(varnamein)) {
        cmd <- paste0(cdo, " -s ", if (nzchar(codes)) paste0("-t ", codes, " -f nc ") else "", "showname ", files[1])
        varnamein <- trimws(system(cmd, intern=T))
        varnamein <- strsplit(varnamein, "[[:space:]]+")[[1]][1] # take the 1st in case several
        if (is.na(varnamein) || !nzchar(varnamein)) stop("job ", jobi, ": could not determine `varnamein` via `", cmd, "`; set it explicitly")
        message("--> `varnamein` not given --> `", cmd, "` --> \"", varnamein, "\"")
    }
    varnameout <- job_get(job, "varnameout", varnamein)

# append "/<setup>/<mode_name>/<varnameout>" unless already there
    pathout <- job_get(job, "pathout", pathin_base(job_get(job, "pathin")))
    pathout_parts <- strsplit(sub("/+$", "", pathout), "/")[[1]]
    required_tail <- c(job_get(job, "setup"), mode_name, varnameout)
    if (!identical(utils::tail(pathout_parts, 3), required_tail)) {
        pathout <- paste0(sub("/+$", "", pathout), "/", paste(required_tail, collapse="/"))
    }
    if (!dir.exists(pathout)) {
        message("`pathout` = \"", pathout, "\" does not exist --> create")
# dir.create() only warns on failure
        if (!dry && !dir.create(pathout, recursive=T) && !dir.exists(pathout)) {
            stop("job ", jobi, ": failed to create `pathout` = \"", pathout, "\" (see the warning above for the reason, ",
                 "e.g. a permission problem on one of its parent directories)")
        }
    }

    fout_given <- job_get(job, "fout")
    if (is.null(fout_given)) {
        dtin <- job_get(job, "dtin", get_dt_from_file(files, from, to))
        depth <- if (nzchar(level)) gsub(",", "-", level, fixed=T) else ""
        segs <- c(varnameout, dtin, mode_name, depth, job_get(job, "setup"), job_get(job, "model", ""), job_get(job, "experiment"), variant, grid)
        fout <- paste0(pathout, "/", paste(segs[nzchar(segs)], collapse="_"), "_", from, "-", to, ".nc")
    } else {
        fout <- paste0(pathout, "/", fout_given)
    }
    message("--> fout = \"", fout, "\"")

    if (file.exists(fout) && !force) {
        message("--> already exists and `force` = F --> skip")
    } else {
        cdo_select_calc(files, varnamein, varnameout, mode, mode_args, level, from, to, needs_year_filter, codes, cdo_pre, cdo_post, fout)
        if (!dry && !file.exists(fout)) stop("job ", jobi, ": `fout` = \"", fout, "\" was not created")
        message("--> done: \"", fout, "\"")
    }

    # annual mean of a sub-annual time series: same name with <dtin> = "year"
    # (or "_an" appended to a given `fout`)
    if (save_fout_an) {
        message("--> `save_fout_an` = T: check if data is 1) time series and 2) sub-annual ...")
        fout_an <- if (is.null(fout_given)) {
            segs[2] <- "year" # <dtin>
            paste0(pathout, "/", paste(segs[nzchar(segs)], collapse="_"), "_", from, "-", to, ".nc")
        } else {
            sub("(\\.nc)?$", "_an.nc", fout)
        }
        if (dry) {
            message("--> `dry` = T: `fout` not created, so not checked for `fout_an` = \"", fout_an, "\"")
        } else if (fout_an != fout && is_subannual_timeseries(fout)) {
            if (file.exists(fout_an) && !force) {
                message("--> `fout_an` = \"", fout_an, "\" already exists and `force` = F --> skip")
            } else {
                # monthly: weight each month by its number of days; else all steps equally
                op_an <- if (get_dt_from_file(fout, from, to) == "mon") "yearmonmean" else "yearmean"
                message("--> annual mean with `cdo ", op_an, "`")
                run_cmd(paste0(cdo_prefix(""), " ", op_an, " ", fout, " ", fout_an))
                if (!file.exists(fout_an)) stop("job ", jobi, ": `fout_an` = \"", fout_an, "\" was not created")
                message("--> done: \"", fout_an, "\"")
            }
        }
    }

} # for jobi

if (length(not_done) > 0) {
    message("\n", length(not_done), "/", length(jobs), " job(s) skipped because of `do` = F: ", paste(not_done, collapse=", "))
}
if (length(skipped) > 0) {
    warning(length(skipped), "/", length(jobs), " job(s) skipped because no input files were found: ",
            paste(skipped, collapse=", "), call.=F, immediate.=T)
}

message("\n****************** post.r finish ******************")
