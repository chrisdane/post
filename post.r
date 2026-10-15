# r

# Generic post-processing of gridded model output (any model, not just echam):
# construct and run cdo (and, if needed, nco) commands that either select a
# variable from a set of input files (mode = "select") or reduce it with a
# cdo statistic (mode = "fldmean", "fldint", "fldsum", "zonmean", "yearmean",
# "timmean", ...).
#
# Condensed rewrite of ~/scripts/r/echam/post_echam.r, keeping only the two
# mechanisms actually named as needing special treatment there, generalized:
#   1. many input files can make the cdo command line exceed the OS's
#      ARG_MAX --> split the file list into chunks that fit, run one cdo
#      command per chunk, then `cdo cat` the chunk outputs back together.
#   2. cdo versions before 1.9.4 cannot chain a selection and a statistic
#      into one command (`-fldmean -select,name=... <files> <fout>`) -->
#      fall back to two separate commands (select to a temp file, then the
#      statistic on that file) for old cdo.
# Everything else in post_echam.r (level/season/mask/area selection, custom
# per-variable cdo_known_cmds, new time axes, wiso delta variables, ...) is
# intentionally left out here; add it back in the namelist as a per-job
# `mode_args`/pre- or post-command if/when actually needed.
#
# Usage: a namelist script sets up config (optional, see defaults below) and
# a `jobs` list, then `source()`s this file. See namelist.post.r for an
# example. `jobs` is a list of jobs, each a named list of the fields below.
# Jobs sharing fields are easiest built there with `expand_jobs(common,
# settings)`: one job per entry of `settings` (pathin, fpattern, experiment,
# ...), each merged into `common` via `modifyList()`.
#
# Each element of `jobs` is a list with:
#   pathin     - directory to search *recursively* for the input files (any
#                depth below it, e.g. an ESGF/CMIP6-archive-style
#                "<pathin>/<table>/<var>/<grid>/<version>/<file>") -- `fpattern`
#                below is a filename pattern only (no "/"), matched against
#                every file's basename wherever it is found under `pathin`.
#                Brace alternatives allowed, e.g. ".../outdata/{echam,jsbach}"
#   fpattern   - input filename pattern, with one of:
#                  - a "<year>" placeholder for the year (any number of
#                    digits -- not assumed to be 4, e.g. a 5-digit
#                    paleoclimate year works too), e.g.
#                    "co2_burden_echam_<year>0101.nc" -- 1 file per year
#                    assumed (this is the same "<year>" convention used
#                    throughout the rest of this repo, e.g. in the cmor and
#                    cdotrend scripts); or
#                  - a "<year>" *and* a "<mon>" placeholder (the latter
#                    always exactly 2 digits, "01".."12" -- needed to
#                    disambiguate it from the variable-length "<year>" next
#                    to it), e.g. "PI-CTRL6_echam6_co2_<year><mon>.grb" -- 1
#                    file per (year, month) assumed, i.e. 12 files/year; or
#                  - both a "<from>" and a "<to>" placeholder, e.g.
#                    "tas_day_MPI-ESM1-2-LR_historical_r1i1p1f1_gn_<from>0101-<to>1231.nc"
#                    -- for archives (e.g. CMIP6) where 1 file covers a whole
#                    chunk of years, with that chunk's own actual from/to
#                    years encoded in the name. The chunk boundaries are not
#                    assumed known up front: candidate files are found via a
#                    regex derived from `fpattern` itself (a capture group
#                    per placeholder), their actual per-file year range is
#                    parsed back out of the matched filenames, and only the
#                    file(s) whose range overlaps [from, to] below
#                    are kept and concatenated -- so a request may span, and
#                    correctly pull in, more than one chunk file.
#                Any other "<...>"-style tag left in `fpattern` (e.g. an
#                unsupported "<mon_from>") is wildcarded away when searching
#                for candidate files, but is not otherwise interpreted.
#                `fpattern` may also contain "/" to include the last
#                directory level(s) of the path, e.g.
#                "<year>0101-<year>1231/cLand_yearly_remapcon_r360x180.nc" for
#                output with the year only in the directory name (a 2nd
#                "<year>" is wildcarded, see below); it is then matched
#                against that many trailing path components instead of the
#                basename alone.
#                A "*" matches any text, as in a shell glob, e.g.
#                "pCO2s_fesom_*<year>0101.nc" for both
#                "pCO2s_fesom_mon_20150101.nc" and "pCO2s_fesom_20150101.nc".
#                Directly before a placeholder it never ends in a digit, so
#                the year/month digits are not taken up by it.
#                Either way (except the derive-only ambiguity check the
#                "<year>"+"<mon>" case skips, see below), more than 1 file
#                matching the very same year (or year+month, or year range)
#                anywhere under `pathin` is treated as an error (ambiguous --
#                which one is the real one?), not silently resolved by
#                picking one.
#   from,
#   to         - [optional] inclusive year range to look for (with "<year>":
#                one file per year in this range is assumed to exist; with
#                "<from>"/"<to>": the found chunk file(s) must
#                fully cover this range, individual chunks need not align
#                with it). Default (either or both left unset): the
#                min/max year actually found under `pathin` -- i.e. every
#                "<year>" file found, or the full span of every
#                "<from>"/"<to>" chunk file found. Either way,
#                the same "no gaps" check still applies over the resulting
#                (given or derived) range. For "<from>"/"<to>",
#                also applied as a "-select,...,year=<from>/<to>"
#                filter (see `sel_op()`), trimming the data itself to
#                exactly this range -- needed there since a whole matching
#                chunk file may cover *more* years than requested. Not
#                applied at all for "<year>"/"<year>"+"<mon>" (the found
#                file(s) there already exactly match the range by
#                construction, so it would be a no-op -- and, unlike
#                `level`, *not* a safe one to always include regardless:
#                confirmed by testing that this combined with `--pedantic`
#                actually crashes cdo if ARG_MAX chunking ever splits the
#                job's files such that a given chunk's own file(s) don't
#                individually contain every requested year).
#   varnamein  - [optional] variable name to select from the input files;
#                default: `cdo showname` of the 1st found input file (with
#                `-t <code> -f nc` too, if `code` is set -- otherwise a raw
#                code-table file's numeric codes would not resolve to a name
#                at all)
#   varnameout - [optional, default = varnamein] variable name in the output
#                (applied via `cdo -setname`)
#   mode       - "select" (no reduction, just pick the variable), or any cdo
#                statistic operator name, e.g. "fldmean", "fldint", "fldsum",
#                "fldstd", "zonmean", "yearmean", "timmean"
#   mode_args  - [optional] arguments appended to `mode`, e.g. mode="zonmean",
#                mode_args="zonal_1" --> "-zonmean,zonal_1"
#   level      - [optional] a single height/depth level or a comma-separated
#                list of levels to select, in the units of the input's own
#                vertical axis, e.g. level="50" or level="0,10,50". Applied
#                as part of the variable selection itself
#                (`-select,name=...,level=...`), so this is where cdo does
#                the actual subsetting -- not a separate operator.
#   code       - [optional] path to a cdo code table / partab file (`cdo -t
#                <code>`), needed to resolve numeric GRIB codes to variable
#                names before `varnamein` can be selected by name at all --
#                the classic case being raw echam GRIB output, e.g.
#                ".../historical2_186110.01_co2.codes" (1 per model output
#                stream/month). Not specific to any one model: leave unset
#                for input that is already proper (self-describing) netCDF.
#                Or a pattern like `fpattern`, e.g. "*_<year><mon>.01_co2.codes":
#                the table of the 1st input file is used for all files.
#   cdo_pre    - [optional] 1 or more cdo operator(s) (with their own
#                leading "-", e.g. "-remapycon,r180x90" or
#                "-setmisstonn -remapycon,r180x90" for several chained
#                together), applied *before* the `mode` statistic -- but
#                after selection/`level`, so e.g.
#                `cdo_pre = "-remapycon,r180x90 -setgrid,/path/to/mesh.nc"`
#                is the classic "setgrid + remapycon" combination: attach
#                the correct source grid (needed whenever the input's own
#                grid is not correctly/fully described in the file itself --
#                the classic case being an unstructured mesh, e.g. native
#                FESOM output), then regrid to a regular one before e.g. a
#                `zonmean` (which needs a regular grid) can be computed on it.
#   cdo_post   - [optional] 1 or more cdo operator(s) (same "-op,args"
#                syntax as `cdo_pre`), applied *after* the `mode` statistic
#                -- chained into the *same* cdo command/call as everything
#                else above, not a separate `cdo` invocation afterward --
#                e.g. a unit conversion, or any further reduction/operator
#                not already covered by `mode`/`mode_args`. If `files` has
#                to be split into chunks (see `cdo_nchar_max_arglist`
#                below), `cdo_post` runs once per chunk, *before* the
#                chunks are concatenated, not once on the final whole
#                result -- fine for e.g. a further spatial or
#                unit-conversion operator, but not equivalent to a single
#                run-once-at-the-end call for an operator that is itself
#                sensitive to that split (e.g. a further temporal one).
#   model,
#   experiment - used in the default `fout` name/`pathout` dirs below
#                (CMIP6/CMOR-like); no generic default, e.g.
#                model="AWI-ESM-1-REcoM", experiment="ssp126"
#   variant    - [optional, default = "r1i1p1f1"] used in the default `fout`
#                name below
#   grid       - [optional, default = "gn"] used in the default `fout` name
#                below (e.g. "gn" native, "gr1" regridded)
#   pathout    - [optional, default = pathin up to any "{"] output directory. Whatever is
#                given (or defaulted to `pathin`) has "/<model>/<mode>/<varnameout>"
#                appended, unless it already ends in exactly that (so a
#                `pathout` already constructed that way by the caller is
#                left alone, not doubled up).
#   do         - [optional, default = T] if F, the job is skipped (e.g. to
#                switch off 1 dataset of a group without commenting it out)
#   fout       - [optional] output filename; default, CMIP6/CMOR-like:
#                "<pathout>/<varnameout>_<dtin>_<mode>_<depth>_<model>_<experiment>_<variant>_<grid>_<from>-<to>.nc"
#                e.g. "spco2_mon_fldmean_AWI-ESM-1-REcoM_ssp126_r1i1p1f1_gr1_2015-2100.nc".
#                <depth> is `level` above (any "," replaced by "-"), omitted
#                entirely (along with its surrounding "_") when `level` is
#                unset, as for a surface variable. <dtin> is the *input*
#                data's own temporal frequency, e.g. "day"/"mon"/"year" --
#                always inferred from the input files themselves (see
#                `get_dt_from_file()`), regardless of `mode`, including its fallback
#                for the case where even 1 whole input file holds only a
#                single usable time step, where no within-file step can be
#                diffed at all. <mode> is `mode` verbatim (e.g. "select",
#                "fldmean", "yearmean") -- shown separately from <dtin> since
#                a temporal-reduction `mode` (e.g. "yearmean") changes the
#                output's actual resolution without changing <dtin>, which
#                always describes the input.
#                If `save_fout_an` (config, default T) and `fout` is a time
#                series (1 grid point and 1 level per variable) finer than
#                annual, its `cdo yearmean` (`cdo yearmonmean` for monthly
#                data, i.e. weighted by days per month) is also saved as `fout_an`: the
#                same name with <dtin> = "year" ("_an" appended to a given
#                `fout`).

message("\n****************** post.r start ******************")

# ---- config (override any of these in the namelist before sourcing) ----
# `exists("force")` (default `exists()` args) would search the *whole* search
# path, including base R -- and `force` is itself a base R function
# (base::force, for lazy-eval forcing), so a plain `exists("force")` is
# always TRUE regardless of whether the namelist set one, silently leaving
# `force` bound to that function instead of a boolean. Restrict the search to
# the global environment (where a namelist's top-level assignments land) for
# every one of these checks, to avoid that class of bug entirely.
user_set <- function(varname) exists(varname, where=globalenv(), inherits=F)

# same class of bug as `user_set()` above, but for 1 job's fields: both
# `job$field` and `job[[field]]` (its default `exact=NA`) do *partial*
# matching on list names, so e.g. `job$pathin` could silently return
# `job$pathout`'s value whenever `pathin` itself is unset but `pathout` is
# (confirmed by testing) -- some of this script's own job field names
# collide exactly this way ("pathin"/"pathout", "mode"/"mode_args").
# `%in% names(job)` is a plain string equality check with no such fallback,
# so gate every job field access through this.
# explicit NULL counts as "not given" (falls through to `default`), not a
# present-but-NULL value overriding it
job_get <- function(job, field, default=NULL) if (field %in% names(job) && !is.null(job[[field]])) job[[field]] else default
if (!user_set("dry")) dry <- F # if T: only print what would be run
if (!user_set("force")) force <- F # if T: redo even if fout already exists
if (!user_set("clean")) clean <- T # if T: remove temporary chunk/select files
if (!user_set("save_fout_an")) save_fout_an <- T # if T: also save the annual mean (`fout_an`) of a sub-annual time series `fout`
if (!user_set("cdo")) {
    # `attributes(...)$status` is only set on a *failed* system() call (NULL
    # on success), so check the result string itself rather than that attribute
    cdo <- suppressWarnings(system("which cdo", intern=T, ignore.stderr=T))
    if (length(cdo) != 1 || !nzchar(cdo)) stop("could not find `cdo` on PATH; set `cdo <- \"/path/to/cdo\"` in the namelist")
}
if (!user_set("cdo_nthreads")) cdo_nthreads <- max(1, trunc(0.75*as.integer(system("nproc", intern=T))))
if (!user_set("jobs")) stop("must provide a `jobs` list before sourcing post.r; see namelist.post.r for an example")

# every job must be a named list, and every field one post.r knows: a job
# appended as `c(jobs, list(pathin=...))` instead of `c(jobs, list(list(...)))`
# spreads its fields as separate "jobs", and job_get() would silently ignore a
# misspelt field (e.g. "fpatern"), which a shared `common` list passes on to
# every job built from it
known_fields <- c("pathin", "fpattern", "from", "to", "varnamein", "varnameout",
                  "mode", "mode_args", "level", "code", "cdo_pre", "cdo_post",
                  "model", "experiment", "variant", "grid", "pathout", "fout", "do")
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

# ---- max cdo/nco command-line length, to detect "Argument list too long" up front ----
cdo_nchar_max_arglist <- trunc(0.9*as.integer(system("getconf ARG_MAX", intern=T))) # 10% safety margin

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

# the cdo statistic operator string for a given `mode`, e.g. "fldmean" -->
# "-fldmean", "zonmean"+"zonal_1" --> "-zonmean,zonal_1", "select" --> "" (no
# reduction, `-select,name=` alone is enough)
# the "-select,..." operator string, folding in an optional height/depth
# `level` filter and a `from`/`to` range (cdo's own
# "-select,name=...,level=...,year=<from>/<to>" syntax -- `select`'s own
# "year" key, rather than a separate "-selyear" operator, so this is where
# cdo does the actual year-trimming too, same as `level`) -- but *only*
# when `needs_year_filter` (from `find_input_files()`) says it's actually
# needed, i.e. only for the "<from>"/"<to>" convention, where a
# whole matching chunk file may cover *more* years than requested (chunk
# boundaries need not align with the request, see its own docs above) --
# without this, the untrimmed chunk's full range would silently be used
# instead of just [from, to]. The "<year>" and "<year>"+"<mon>"
# conventions never need it (each found file already exactly matches its
# own single year(-month) by construction) -- and, unlike `level`, it is
# NOT safe to just always include it as a harmless no-op there: confirmed
# by testing that "-select,...,year=X/Y" on a file that only contains some
# (not all) of the requested years, together with the `--pedantic` and
# `-P <n>` (parallel) options this script always uses, actually *crashes*
# cdo itself ("terminate called recursively") rather than just warning --
# hit for real by the "<year>" convention once ARG_MAX chunking splits the
# job's full year range across separate per-file chunks, each of which by
# itself then only ever has 1 of the requested years.
# Shared by `cdo_select_calc()`'s command-length estimate and
# `cdo_select_calc_1cmd()`'s actual command, so the two can never drift apart
mode_to_calc <- function(mode, mode_args) {
    if (mode == "select") "" else paste0("-", mode, if (nzchar(mode_args)) paste0(",", mode_args) else "")
}
sel_op <- function(varnamein, level, from, to, needs_year_filter) {
    paste0("-select,name=", varnamein, if (nzchar(level)) paste0(",level=", level) else "",
           if (needs_year_filter) paste0(",year=", from, "/", to) else "")
}

# infer the temporal frequency of the *input* `files` from the data itself
# (used for the default `fout` name's "<dtin>" segment, always -- regardless
# of `mode`; `mode` gets its own separate "<mode>" segment there instead,
# see post.r's header comment). Tiered, most-to-least direct:
#   1. the actual timestamps of the first file: if it holds >= 2 distinct
#      time steps, the median step between them gives the frequency
#      directly.
#   2. a file with only 1 (or 0 usable) time step cannot be diffed on its
#      own -- this is the case a single-timestep input needs special
#      handling for: instead, spread the *total* number of time steps found
#      (this file's own ntime, times the number of input files, assuming a
#      uniform count per file) over the *requested* number of years. For
#      the "<year>" 1-file-per-year convention this is exact (nfiles ==
#      nyears by construction, so it reduces to exactly that file's own
#      ntime -- e.g. 1 file/year with 12 monthly steps each --> 12
#      steps/year, or 1 file/year with 1 (already annual) step each --> 1
#      step/year); for the "<from>"/"<to>" chunk convention it is
#      an approximation that only matters when even a single chunk file
#      holds just 1 time step -- rare, since tier 1 above already handles
#      the normal, many-steps-per-chunk case directly.
# Either way, classify the resulting steps/year against known frequencies
# within a tolerance; anything else (irregular data, or genuinely just 1
# time step in 1 file for 1 requested year, where nothing at all -- not
# even this tier-2 ratio -- can distinguish "the 1 value is an annual mean"
# from "it's some other single snapshot") falls back to a literal
# "<n>ts_yr-1" label rather than silently guessing wrong.
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

# escape all regex metacharacters in `x` (1 literal character at a time via
# `fixed=T`, rather than 1 combined character-class pattern -- easy to get
# subtly wrong across `[`/`]`/`-`/`^` position rules within a class)
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

# find the input files for 1 job: either the "<year>" convention (1 file per
# year, substituted directly), or the "<from>"+"<to>" convention (a
# file covers a -- possibly multi-year -- chunk, with that chunk's own actual
# from/to years encoded in the name, e.g. CMIP6-archive-style
# "..._<from>0101-<to>1231.nc"). Chunk boundaries are not assumed
# known up front for the latter: parse each candidate's actual year range
# back out via a regex derived from `fpattern` itself, then keep only the
# file(s) that overlap [from, to] and error out if they do not
# fully cover it.
# Searches `pathin` *recursively*, in 1 `list.files()` listing (rather than a
# filesystem lookup per year/chunk) -- `pathin` may be an archive root several
# directories above where the files actually live (e.g. ESGF/CMIP6-style
# "<pathin>/<table>/<var>/<grid>/<version>/<file>"), so a plain, non-recursive
# lookup would miss them entirely. That listing is itself restricted, via
# `list.files()`'s own `pattern` argument, to a regex derived from
# `fpattern` (matched against each file's basename only, even though the
# listing is recursive) -- so only files that could possibly match
# `fpattern` are ever listed, not every file anywhere under `pathin`. More
# than 1 match for the same year (or year range) anywhere under `pathin` is
# an error (ambiguous), never silently resolved by picking one.
# print the first/last `n` of a (possibly long) file list, so a sanity
# check doesn't require scrolling through hundreds/thousands of names;
# printed in full instead if there's no gap to collapse (<= 2n files)
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

    # the regex derived from `fpattern` is built up front and passed
    # straight to `list.files()` as its own `pattern` (matched against
    # basename even with recursive=T, confirmed by testing) -- so only
    # files that could possibly match `fpattern` are ever listed in the
    # first place, not every file anywhere under `pathin`. "<year>" (and
    # "<from>"/"<to>") become "[0-9]+" -- 1 or more digits, not
    # assumed to be 4 -- while "<mon>" is fixed at exactly 2 ("[0-9]{2}"):
    # needed to disambiguate it from a variable-length "<year>" immediately
    # next to it (e.g. "<year><mon>" with no separator at all) -- with an
    # unanchored "[0-9]+" on *both* sides there would be no unique split.
    if (is_year && has_mon) {
        # sub()'s literal-text replacement leaves the 2 capture groups in
        # the same left-to-right order the 2 placeholders were in
        # `fpattern` to begin with, regardless of which is substituted
        # first -- so track that original order here to know which capture
        # group (2 or 3) is the year vs the month later on
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
    # any other "<...>" tag left in `fpattern` (this script only recognizes
    # "<year>"/"<mon>"/"<from>"/"<to>", but a namelist might carry
    # over an unrelated tag from this repo's other, richer placeholder
    # conventions, e.g. "<mon_from>") is not a literal string to match --
    # wildcard it out too (as ".*", the regex equivalent of a "*" glob
    # wildcard; a literal "*" would instead be read as "zero or more of the
    # previous character") rather than requiring the literal "<...>" text
    # to appear in a filename
    re <- gsub("<[^<>]*>", ".*", re)

    # glob "*" (escaped to "\*" above): directly before a placeholder's
    # "([0-9]" it must not end in a digit, or a greedy ".*" takes the year's
    # leading digits; as a non-capturing group, so the placeholders' capture
    # group numbers stay the same (needs perl=T for every use of `re` below)
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
    message("--> run base::list.files(pathins, recursive=T, full.names=T, pattern=\"", re_tre, "\") in `pathin` director",
            if (length(pathins) == 1) "y" else "ies", " (specify a clearer `pathin` if this takes too long). pathins:\n",
            paste(pathins, collapse="\n"))
    all_files <- if (length(pathins) > 0) base::list.files(pathins, recursive=T, full.names=T, pattern=re_tre) else character(0)
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
            # neither/either given --> derive from every "<year>"+"<mon>"
            # file actually found, rather than a caller-specified range.
            # Same (year, month) matched by >1 file is ambiguous -- unlike
            # the plain "<year>" case below, the same *year* alone
            # legitimately recurs (once per month), so only the (year,
            # month) pair, not the year alone, is checked here
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
        # match by the year/month read from each name, not the exact name,
        # which a "*" in `fpattern` would not give
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
            # neither/either given --> derive from every "<year>" file
            # actually found, rather than a caller-specified range
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

        # same (from, to) chunk matched by >1 file (e.g. duplicated across
        # subdirectories/versions) is ambiguous -- do not silently pick one
        key <- paste(file_from, file_to)
        dup <- key %in% key[duplicated(key)]
        if (any(dup)) {
            stop("job ", jobi, ": ambiguous -- more than 1 file found recursively under \"", pathin, "\" for the same year range, e.g.:\n",
                 paste(all_files[dup], collapse="\n"))
        }

        if (!from_given || !to_given) {
            # neither/either given --> use every chunk file found, spanning
            # its own full min/max range, rather than a caller-specified one
            keep <- rep(T, length(all_files))
            if (!from_given) from <- min(file_from)
            if (!to_given) to <- max(file_to)
            message("--> `from`/`to` not (both) given --> derived from found \"<from>\"/\"<to>\" files: ", from, "-", to)
        } else {
            keep <- file_to >= from & file_from <= to # overlaps the requested range
            if (!any(keep)) {
                stop("job ", jobi, ": found ", length(all_files), " file(s) via \"<from>\"/\"<to>\", but none overlap the requested range ",
                     from, "-", to, "; found range(s): ", paste(paste0(file_from, "-", file_to), collapse=", "))
            }
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

# cdo's `-P <n>` etc. up front, plus `-t <codes> -f nc` if a code table is
# given. Unlike `-setgrid`/`-select`/`-fldmean` etc., `-t` is NOT a chainable
# operator (`cdo -select,... -t <codes> ...` errors with "Operator >t<
# not found!") -- it is a *global option* and must precede the whole
# operator chain, right after `cdo` itself. `-f nc` is bundled alongside it
# because raw model output using a code table (e.g. echam's own binary
# "service"/GRIB-like format) otherwise stays in its native format even when
# written to a ".nc"-named file (cdo does not infer output format from the
# file extension) -- without it, `varnamein` would not even carry a resolved
# name in the result, defeating the point of `-t` in the first place.
cdo_prefix <- function(codes) {
    paste0(cdo, " --pedantic -P ", cdo_nthreads, if (nzchar(codes)) paste0(" -t ", codes, " -f nc") else "")
}

# 1 cdo call: select `varnamein` (renamed to `varnameout`, optionally
# restricted to `level`) from `files`, apply the `mode` statistic (unless
# mode=="select"), write to `fout_i`. Chains selection+statistic into 1
# command if the cdo version allows it (see `cdo_can_chain` above);
# otherwise selects to a temp file first, then runs the statistic on that
# temp file as a 2nd command (`code`/`-f nc` only apply to that 1st,
# file-reading command -- the 2nd, on the by-then-proper-netCDF temp file,
# needs neither).
# `cdo_pre` (may be "" for none): 1 or more cdo operator(s) (with their own
# leading "-", e.g. "-remapycon,r180x90 -setgrid,/path/to/mesh.nc"), applied
# after selection but before the `mode` statistic -- e.g. the classic
# "setgrid + remapycon" combination, regridding an unstructured mesh to a
# regular grid before e.g. computing a `zonmean` on it (which needs one).
# `cdo_post` (may be "" for none): 1 or more cdo operator(s), applied right
# after the `mode` statistic -- chained into this *same* cdo command/call
# (not a separate `cdo` invocation afterward). Note this means it runs
# once per chunk when `cdo_select_calc()` below has to split `files` up
# (i.e. *before* the chunks are `cdo cat`-ed together), not once on the
# final whole result -- fine for e.g. a further spatial or unit-conversion
# operator, but not equivalent to a single run-once-at-the-end call for an
# operator that is itself sensitive to that split (e.g. a further temporal
# one).
cdo_select_calc_1cmd <- function(files, varnamein, varnameout, mode, mode_args, level, from, to, needs_year_filter, codes, cdo_pre, cdo_post, fout_i) {
    calc <- mode_to_calc(mode, mode_args)
    sel <- paste0("-setname,", varnameout, " ", sel_op(varnamein, level, from, to, needs_year_filter))
    filestr <- paste(files, collapse=" ")

    if (cdo_can_chain) {
        cmd <- paste0(cdo_prefix(codes), " ", cdo_post, " ", calc, " ", cdo_pre, " ", sel, " ", filestr, " ", fout_i)
        cmd <- gsub("[[:space:]]+", " ", cmd) # tidy up in case any piece was ""
        run_cmd(cmd, files)
    } else {
        tmpfile <- paste0(tools::file_path_sans_ext(fout_i), "_tmp_select_", Sys.getpid(), ".nc")
        run_cmd(gsub("[[:space:]]+", " ", paste0(cdo_prefix(codes), " ", cdo_pre, " ", sel, " ", filestr, " ", tmpfile)), files)
        if (mode == "select" && !nzchar(cdo_post)) {
            run_cmd(paste0(if (clean) "mv " else "cp ", tmpfile, " ", fout_i))
        } else {
            run_cmd(gsub("[[:space:]]+", " ", paste0(cdo, " --pedantic -P ", cdo_nthreads, " ", cdo_post, " ", calc, " ", tmpfile, " ", fout_i)))
            if (clean) run_cmd(paste0("rm ", tmpfile))
        }
    }
} # cdo_select_calc_1cmd

# same as `cdo_select_calc_1cmd()`, but splits `files` into as many chunks
# as needed to keep every individual cdo command under `cdo_nchar_max_arglist`
# characters, running 1 chunk at a time and `cdo cat`-ing the chunk results
# back together into 1 final `fout` (chunk count 1 is the common case: just
# runs directly, no cat needed).
cdo_select_calc <- function(files, varnamein, varnameout, mode, mode_args, level, from, to, needs_year_filter, codes, cdo_pre, cdo_post, fout) {
    nchar_fixed <- nchar(paste0(cdo_prefix(codes), " ", cdo_post, " ", mode_to_calc(mode, mode_args), " ", cdo_pre,
                                " -setname,", varnameout, " ", sel_op(varnamein, level, from, to, needs_year_filter), " ", fout))
    nchar_per_file <- nchar(files) + 1L # +1 for the separating space
    nchar_total <- nchar_fixed + sum(nchar_per_file)

    if (nchar_total <= cdo_nchar_max_arglist) { # fits in 1 command -- the common case
        cdo_select_calc_1cmd(files, varnamein, varnameout, mode, mode_args, level, from, to, needs_year_filter, codes, cdo_pre, cdo_post, fout)
        return(invisible())
    }

    # doesn't fit --> split into chunks of as many files as fit under the
    # limit each, so no individual cdo invocation ever hits "Argument list too long"
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

    # check required fields ("varnamein"/"from"/"to" are not among them -- see below)
    required <- c("pathin", "fpattern", "mode", "model", "experiment")
    missing <- required[!sapply(required, function(f) !is.null(job_get(job, f)))]
    if (length(missing) > 0) stop("job ", jobi, " is missing required field(s): ", paste(missing, collapse=", "))

    mode <- job_get(job, "mode")
    mode_args <- job_get(job, "mode_args", "")
    level <- job_get(job, "level", "")
    codes <- job_get(job, "code", "") # resolved below
    cdo_pre <- job_get(job, "cdo_pre", "")
    cdo_post <- job_get(job, "cdo_post", "")
    variant <- job_get(job, "variant", "r1i1p1f1")
    grid <- job_get(job, "grid", "gn")

    # input files are needed up front now regardless of `job$fout`: both for
    # the default (CMOR-like) `fout`'s "dtin" segment (see `get_dt_from_file()` below)
    # and, here, for defaulting `varnamein` itself off the 1st one found.
    # Always (re-)discovered here, every iteration, rather than only when
    # needed: a stale `files` left over from a *previous* job in this loop
    # would otherwise silently carry over and be reused for the wrong job.
    # `from`/`to` come back resolved too (unchanged if `job` gave
    # both; derived from the found files' own min/max otherwise).
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

    # `pathout` (given, or defaulted to `pathin`) always ends in
    # "/<model>/<mode>/<varnameout>" -- appended unless it's already exactly
    # that (compared component-by-component on the *trailing* path parts,
    # not a plain string suffix match, so e.g. a `pathout` that merely ends
    # in "...xspco2" is not mistaken for already ending in ".../spco2")
    pathout <- job_get(job, "pathout", pathin_base(job_get(job, "pathin")))
    pathout_parts <- strsplit(sub("/+$", "", pathout), "/")[[1]]
    required_tail <- c(job_get(job, "model"), mode, varnameout)
    if (!identical(utils::tail(pathout_parts, 3), required_tail)) {
        pathout <- paste0(sub("/+$", "", pathout), "/", paste(required_tail, collapse="/"))
    }
    if (!dir.exists(pathout)) {
        message("`pathout` = \"", pathout, "\" does not exist --> create")
        # `dir.create()` does NOT stop/error on failure (e.g. no permission
        # on a parent dir) -- it just emits a warning and returns FALSE,
        # which is easy to miss among this script's other `message()`
        # output, silently leaving `pathout` never actually created and
        # every write into it failing later (or, if `dry`, "succeeding"
        # without ever writing anything at all) -- verify explicitly instead
        if (!dry && !dir.create(pathout, recursive=T) && !dir.exists(pathout)) {
            stop("job ", jobi, ": failed to create `pathout` = \"", pathout, "\" (see the warning above for the reason, ",
                 "e.g. a permission problem on one of its parent directories)")
        }
    }

    fout_given <- job_get(job, "fout")
    if (is.null(fout_given)) {
        dtin <- get_dt_from_file(files, from, to)
        depth <- if (nzchar(level)) gsub(",", "-", level, fixed=T) else ""
        segs <- c(varnameout, dtin, mode, depth, job_get(job, "model"), job_get(job, "experiment"), variant, grid)
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
