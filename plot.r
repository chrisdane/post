# r

# plot netcdf time series/fields (e.g. post.r output); the plot type follows the data shape:
#   1 dim  --> line plot vs that dim
#   2 datasets with 1 dim + mode="pairwise" --> data1 (x) vs data2 (y)
#   2 dims --> contour plot (image.plot.nxm), 1 panel per dataset
# surplus dims are removed via the data's `index`, e.g. index=list(depth=2)
#
# usage: set `plots` (and optional config below) in a namelist and source this file
#
# each element of `plots` is a list with:
#   fout    - output file(s), ".png" and/or ".pdf". May contain "<plot_from>"/
#             "<plot_to>" placeholders, substituted with this plot's own
#             `plot_from`/`plot_to` (below) if set, else the actual calendar-year
#             range found in its data (so the name reflects whatever time range
#             was actually plotted even if `plot_from`/`plot_to` were never
#             set at all). Only substituted if present; `fout` is left as is
#             otherwise. A job whose `fout` uses either placeholder has its
#             data read before `fout` can be resolved (needed to derive the
#             fallback range), so it loses the usual before-reading
#             `force`/already-exists skip -- only the plotting itself is
#             skipped in that case, not the (cheap, if `plot_from`/`plot_to`
#             restrict it) read. Errors only if a placeholder is present and
#             no range -- explicit or data-derived -- could be found at all.
#   pathout - [optional] output dir, default: dirname(fout)
#   mode    - [optional] "auto" (default) or "pairwise"
#   datas   - list of 1 or more datasets to plot together, each a list with:
#     fin      - netcdf file; a bare file name is searched in `pathpost`
#     varname  - [optional] default: 1st variable of `fin`
#     label    - [optional] default: varname
#     index    - [optional] list(dimname=index) to slice away dims; size-1 dims are dropped anyway
#     fac      - [optional] factor the data is multiplied with, e.g. -1 to flip the sign; default: 1
#     plot_from, plot_to - [optional] overrides the plot-level `plot_from`/`plot_to` (see below) for just this dataset
#     col, lty, lwd - [optional] line plots only
#     ma       - [optional] line plots only; list(nyrs=, align=) overriding,
#                for just this dataset, the plot-level `ma_opts` (see below),
#                itself overriding config `ma_nyrs`/"center", used for the
#                `plot_ma` running-mean output; align: "center" (default,
#                centered window) or "right" (trailing window, filling the
#                start via an expanding window instead of leaving it
#                unplotted, like `zoo::rollapply(align="right", partial=T)`)
#   plot_from,
#   plot_to - [optional] inclusive calendar-year range to actually plot, e.g.
#             plot_from=1900, plot_to=2000 -- restricts a dataset's time dim
#             to just this range, read directly via `ncvar_get(..., start=,
#             count=)` rather than reading every time step and subsetting in
#             R afterward (matters for big files). May be set per plot (here,
#             applies to every dataset in `datas`), per dataset (its own
#             `plot_from`/`plot_to`, overriding the plot-level one for just
#             that dataset), and/or globally for every plot in this whole run
#             (config `plot_from`/`plot_to` below, overridden by a plot's/
#             dataset's own). Ignored (with a message) for a dataset
#             whose variable has no single recognized time dim, or whose
#             time dim's units could not be parsed into an actual time axis.
#   width, height, res - [optional] png px and ppi (pdf: width/res, height/res inch); default: plot_sizes()
#   ylab    - [optional] line plots only; default: label and units of the 1st dataset
#   legend_pos - [optional] line plots only; "my_maxempty" (default; "topleft" if it fails),
#             a `legend()` keyword or c(x, y)
#   legend_ncol, legend_cex - [optional] default: 2 and 0.8 if more than 5 datasets, else 1 and 1
#   legend_x_intersp - [optional] space between line and text in character widths; default: 0.5
#   plot_ma - [optional] if T, also save a running-mean version of this plot
#             next to the normal one (same `fout`(s), with "_ma<nyrs>" inserted
#             before the extension, <nyrs> = the plot's/config's `ma_opts`/
#             `ma_nyrs` -- a dataset's own differing `ma$nyrs` isn't reflected
#             in the name) -- analogous to post.r's `save_fout_an` saving an
#             additional annual-mean `fout_an` next to `fout`. Only applies to
#             a non-pairwise line plot with an actual time axis (ignored
#             otherwise, same as any dataset's `ma`, above). default: config
#             `plot_ma` (below), itself default F
#   ma_opts - [optional] list(nyrs=, align=) default running-mean settings for
#             every dataset in `datas` that doesn't set its own `ma` (above);
#             itself overrides config `ma_nyrs`/"center"
#   zlim, nlevels, n, m - [optional] contour plots only; passed to image.plot.pre()/image.plot.nxm()

message("\n****************** plot.r start ******************")

message("\nload ncdf4, data.table ...")
library(ncdf4)
library(data.table) # frollmean(), for `plot_ma`
message("source myfunctions.r, image.plot.pre.r, image.plot.nxm.r ...")
source("~/scripts/r/functions/myfunctions.r")
source("~/scripts/r/functions/image.plot.pre.r")
source("~/scripts/r/functions/image.plot.nxm.r")

# config; may be set in the namelist
user_set <- function(varname) exists(varname, where=globalenv(), inherits=F)

# not `job$x`: partial matching would return e.g. job$mode for job$m
# explicit NULL counts as "not given" (falls through to `default`) --
# otherwise `list(plot_from=exp$plot_from, ...)` with an absent `exp$plot_from`
# would store a present-but-NULL "plot_from" key, winning over `default` when
# it shouldn't
job_get <- function(job, field, default=NULL) if (field %in% names(job) && !is.null(job[[field]])) job[[field]] else default
if (!user_set("dry")) dry <- F # if T: do not plot
if (!user_set("force")) force <- F # if T: redo even if fout already exists
if (!user_set("cdo")) {
    cdo <- suppressWarnings(system("which cdo", intern=T, ignore.stderr=T))
    if (length(cdo) != 1 || !nzchar(cdo)) stop("could not find `cdo` on PATH; set `cdo <- \"/path/to/cdo\"` in the namelist")
}
if (!user_set("plots")) stop("must provide a `plots` list before sourcing plot.r; see namelist.plot.r for an example")
if (!user_set("default_cols")) default_cols <- c("black", "red", "blue", "darkgreen", "orange", "magenta", "cyan", "brown")
if (!user_set("pp")) pp <- plot_sizes()
if (!user_set("line_width")) line_width <- pp$png_width_px # px
if (!user_set("line_height")) line_height <- pp$png_height_px
if (!user_set("contour_width")) contour_width <- 10*pp$png_ppi
if (!user_set("contour_height")) contour_height <- 6*pp$png_ppi
if (!user_set("res")) res <- pp$png_ppi
if (!user_set("family")) family <- "Graphik Regular Trial" # fallback: png "Droid Sans", pdf "sans"
if (!user_set("mgp")) mgp <- c(1.35, 0.5, 0) # lines of axis title (contour plots only), tick labels, axis (default: c(3, 1, 0))
if (!user_set("tcl")) tcl <- -0.3 # tick length (default: -0.5)
if (!user_set("pathpost")) pathpost <- "" # where to search a `fin` given as bare file name
if (!user_set("plot_from")) plot_from <- NULL # [optional] restrict ALL plots to just this calendar-year range by default (reads only the needed time steps); overridden by a plot's/dataset's own `plot_from`/`plot_to`
if (!user_set("plot_to")) plot_to <- NULL
if (!user_set("plot_ma")) plot_ma <- F # if T (or a plot's own `plot_ma`): also save a running-mean version of each eligible (time series) plot; see header
if (!user_set("ma_nyrs")) ma_nyrs <- 21 # [default] running-mean window length in years; overridable per-dataset via that dataset's own `ma$nyrs`

##################################################################################

# time dim names; time goes on the x axis of contour plots
known_time_dimnames <- c("time", "Time", "TIME", "mon")

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

# `fin` as is if it exists, else its basename searched for under `pathpost`
resolve_fin <- function(fin, ploti, datai) {
    if (is.null(fin)) stop("plot ", ploti, ", data ", datai, " has no `fin`")
    if (file.exists(fin)) return(fin)
    dirs <- brace_expand(pathpost)
    dirs <- dirs[nzchar(dirs) & dir.exists(dirs)]
    # fast: `fin` relative to `pathpost`, e.g. "echam/fldmean/co2_burden/<file>"
    hits <- file.path(dirs, fin)
    hits <- hits[file.exists(hits)]
    if (length(hits) == 1) return(hits)
    if (length(hits) > 1) stop("plot ", ploti, ", data ", datai, ": `fin` = \"", fin, "\" found more than once under `pathpost`:\n", paste(hits, collapse="\n"))
    # slow: recursive search for its basename
    re <- paste0("^", gsub("([.+^$()|{}\\[\\]\\\\])", "\\\\\\1", basename(fin)), "$")
    if (length(dirs) > 0) message("--> run list.files(recursive=T, full.names=T, pattern=\"", re, "\") in:\n", paste(dirs, collapse="\n"))
    hits <- if (length(dirs) > 0) list.files(dirs, recursive=T, full.names=T, pattern=re) else c()
    if (length(hits) == 0) stop("plot ", ploti, ", data ", datai, ": `fin` = \"", fin, "\" not found (also not under `pathpost` = \"", paste(pathpost, collapse="\", \""), "\")")
    if (length(hits) > 1) stop("plot ", ploti, ", data ", datai, ": `fin` = \"", fin, "\" found more than once under `pathpost`:\n", paste(hits, collapse="\n"))
    hits
} # resolve_fin

# read 1 dataset; `plot_from`/`plot_to` (may be NULL): this plot's default
# calendar-year range to restrict the time dim to, overridden by `d`'s own
# `plot_from`/`plot_to` if it has them
read_data <- function(d, ploti, datai, plot_from=NULL, plot_to=NULL) {
    fin <- resolve_fin(job_get(d, "fin"), ploti, datai)
    nc <- ncdf4::nc_open(fin)
    varname <- job_get(d, "varname")
    if (is.null(varname)) {
        cmd <- paste0(cdo, " -s showname ", fin)
        varname <- trimws(system(cmd, intern=T))
        varname <- strsplit(varname, "[[:space:]]+")[[1]][1]
        if (is.na(varname) || !nzchar(varname)) stop("plot ", ploti, ", data ", datai, ": could not determine a variable name via `", cmd, "`")
        message("--> data ", datai, ": `varname` not given --> `", cmd, "` --> \"", varname, "\"")
    }
    if (is.null(nc$var[[varname]])) {
        stop("plot ", ploti, ", data ", datai, ": variable \"", varname, "\" not found in \"", fin, "\". available: ",
             paste(names(nc$var), collapse=", "))
    }
    vardims <- sapply(nc$var[[varname]]$dim, function(dd) dd$name)
    time_dim_ind <- which(!is.na(match(vardims, known_time_dimnames)))

    # `plot_from`/`plot_to`: restrict reading to just the needed time steps via
    # `ncvar_get(..., start=, count=)`, instead of reading the whole file and
    # subsetting in R afterward -- matters for big files
    plot_from_d <- job_get(d, "plot_from", plot_from)
    plot_to_d <- job_get(d, "plot_to", plot_to)
    if (!is.null(plot_from_d) && !is.null(plot_to_d) && plot_from_d > plot_to_d) {
        stop("plot ", ploti, ", data ", datai, ": `plot_from` = ", plot_from_d, " > `plot_to` = ", plot_to_d)
    }
    start <- count <- i_from <- i_to <- NULL
    if (!is.null(plot_from_d) || !is.null(plot_to_d)) {
        if (length(time_dim_ind) != 1) {
            message("--> data ", datai, ": `plot_from`/`plot_to` given but found ", length(time_dim_ind),
                    " recognized time dim(s) among ", paste(vardims, collapse=", "), " --> ignore `plot_from`/`plot_to`")
        } else {
            tdim <- vardims[time_dim_ind]
            traw <- nc$dim[[tdim]]$vals
            tunits <- if (is.null(nc$dim[[tdim]]$units)) "" else nc$dim[[tdim]]$units
            tvals <- maybe_time_axis(tdim, traw, tunits)
            if (!inherits(tvals, "POSIXct")) {
                message("--> data ", datai, ": `plot_from`/`plot_to` given but dim \"", tdim, "\"'s units = \"", tunits,
                        "\" could not be converted to a time axis --> ignore `plot_from`/`plot_to`")
            } else {
                from_bound <- if (is.null(plot_from_d)) -Inf else as.POSIXct(paste0(plot_from_d, "-01-01 00:00:00"), tz="UTC")
                to_bound <- if (is.null(plot_to_d)) Inf else as.POSIXct(paste0(plot_to_d, "-12-31 23:59:59"), tz="UTC")
                keep <- which(tvals >= from_bound & tvals <= to_bound)
                if (length(keep) == 0) {
                    stop("plot ", ploti, ", data ", datai, ": `plot_from` = ", if (is.null(plot_from_d)) "NULL" else plot_from_d,
                         ", `plot_to` = ", if (is.null(plot_to_d)) "NULL" else plot_to_d, " match no time step of dim \"", tdim,
                         "\" in \"", fin, "\" (", length(tvals), " steps, ", format(tvals[1]), " to ", format(tvals[length(tvals)]), ")")
                }
                if (!all(diff(keep) == 1)) {
                    stop("plot ", ploti, ", data ", datai, ": time steps of dim \"", tdim, "\" matching `plot_from`/`plot_to` ",
                         "are not contiguous -- unexpected, check the file's time axis")
                }
                i_from <- min(keep); i_to <- max(keep)
                start <- rep(1, length(vardims)); count <- rep(-1, length(vardims))
                start[time_dim_ind] <- i_from
                count[time_dim_ind] <- i_to - i_from + 1
                message("--> data ", datai, ": `plot_from`/`plot_to` --> read only time steps ", i_from, "-", i_to,
                        " of ", length(tvals), " (", format(tvals[i_from]), " to ", format(tvals[i_to]), ")")
            }
        }
    }

    arr <- if (is.null(start)) {
        ncdf4::ncvar_get(nc, varname, collapse_degen=F)
    } else {
        ncdf4::ncvar_get(nc, varname, start=start, count=count, collapse_degen=F)
    }
    units_att <- suppressWarnings(ncdf4::ncatt_get(nc, varname, "units"))
    units <- if (units_att$hasatt) units_att$value else ""
    dimvals <- lapply(seq_along(vardims), function(i) {
        vals <- nc$dim[[vardims[i]]]$vals
        if (!is.null(start) && i == time_dim_ind) vals <- vals[i_from:i_to]
        vals
    })
    names(dimvals) <- vardims
    dimunits <- vapply(vardims, function(nm) if (is.null(nc$dim[[nm]]$units)) "" else nc$dim[[nm]]$units, character(1))
    ncdf4::nc_close(nc)

    # this dataset's own actual (post-restriction, if any) time range, as plain
    # calendar years -- used as the "<plot_from>"/"<plot_to>" fallback in
    # `resolve_fout()` for a plot with no `plot_from`/`plot_to` of its own
    year_from <- year_to <- NA_integer_
    if (length(time_dim_ind) == 1) {
        tv <- maybe_time_axis(vardims[time_dim_ind], dimvals[[time_dim_ind]], dimunits[time_dim_ind])
        if (inherits(tv, "POSIXct") && length(tv) > 0) {
            year_from <- as.integer(format(tv[1], "%Y"))
            year_to <- as.integer(format(tv[length(tv)], "%Y"))
        }
    }

    # apply `index` and drop size-1 dims in 1 `[` call
    orig_dim <- dim(arr)
    index <- job_get(d, "index", list())
    for (nm in names(index)) {
        if (!(nm %in% vardims)) stop("plot ", ploti, ", data ", datai, ": `index` names \"", nm, "\", not among this data's dims: ",
                                      paste(vardims, collapse=", "))
        ind <- index[[nm]]
        di <- which(vardims == nm)
        if (ind < 1 || ind > orig_dim[di]) stop("plot ", ploti, ", data ", datai, ": `index[[\"", nm, "\"]]` = ", ind, " out of range 1..", orig_dim[di])
    }
    idx_args <- rep(list(quote(expr=)), length(vardims))
    for (nm in names(index)) idx_args[[which(vardims == nm)]] <- index[[nm]]
    arr <- do.call(`[`, c(list(arr), idx_args, list(drop=T)))
    survives <- !(vardims %in% names(index)) & (orig_dim > 1)
    vardims <- vardims[survives]

    arr <- arr*job_get(d, "fac", 1)
    label <- job_get(d, "label", varname)
    message("--> data ", datai, ": \"", label, "\" (\"", varname, "\" from \"", fin, "\") --> ", length(vardims), " remaining dim(s): ",
            if (length(vardims) == 0) "(none, scalar)" else paste(vardims, collapse=", "))
    list(data=arr, dims=vardims, dimvals=dimvals[vardims], dimunits=dimunits[vardims], varname=varname, units=units, label=label,
         year_from=year_from, year_to=year_to)
} # read_data

axis_label <- function(dimname, dimunits) if (nzchar(dimunits)) paste0(dimname, " [", dimunits, "]") else dimname
data_label <- function(d) if (nzchar(d$units)) paste0(d$label, " [", d$units, "]") else d$label

# time dim with "<unit> since <origin>" (s/min/h/d) or cdo's "day as %Y%m%d" units --> POSIXct; else unchanged
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

# `year_from`/`year_to` (may be NULL): fallback for the "<plot_from>"/"<plot_to>"
# placeholders below, used only if this plot has no `plot_from`/`plot_to` of its
# own -- the main loop passes in the data's own actual (post-read) time range
# for this, once known, for a job whose `fout` needs it
resolve_fout <- function(job, ploti, year_from=NULL, year_to=NULL) {
    fout <- job_get(job, "fout")
    if (is.null(fout)) stop("plot ", ploti, " has no `fout`")
    if (!is.character(fout) || length(fout) == 0) stop("plot ", ploti, ": `fout` must be a non-empty character vector")
    # "<plot_from>"/"<plot_to>" placeholders (optional): substituted with this
    # plot's own `plot_from`/`plot_to` if set, else `year_from`/`year_to` (the
    # data's actual time range), so the output filename reflects whatever time
    # range was actually plotted
    plot_from <- job_get(job, "plot_from", year_from)
    plot_to <- job_get(job, "plot_to", year_to)
    if (grepl("<plot_from>", fout, fixed=T)) {
        if (is.null(plot_from)) {
            stop("plot ", ploti, ": `fout` contains \"<plot_from>\" but this plot has no `plot_from` set, and its data ",
                 "has no usable time dim to derive one from")
        }
        fout <- gsub("<plot_from>", plot_from, fout, fixed=T)
    }
    if (grepl("<plot_to>", fout, fixed=T)) {
        if (is.null(plot_to)) {
            stop("plot ", ploti, ": `fout` contains \"<plot_to>\" but this plot has no `plot_to` set, and its data ",
                 "has no usable time dim to derive one from")
        }
        fout <- gsub("<plot_to>", plot_to, fout, fixed=T)
    }
    abs <- grepl("^/", fout)
    pathout <- job_get(job, "pathout", dirname(fout))
    ifelse(abs, fout, paste0(pathout, "/", basename(fout)))
} # resolve_fout

# validate `fouts`' extensions, create their output dir(s) if needed, and (if
# `force` is F) drop any that already exist -- shared by both the up-front
# (no "<plot_from>"/"<plot_to>") and the deferred (needs the data first) path
# in the main loop below
check_fouts <- function(fouts, ploti) {
    exts <- tolower(tools::file_ext(fouts))
    if (!all(exts %in% c("png", "pdf"))) stop("plot ", ploti, ": every `fout` must end in \".png\" or \".pdf\"; got ", paste0("\"", fouts, "\"", collapse=", "))
    for (pathout in unique(dirname(fouts))) {
        if (!dir.exists(pathout)) {
            message("`pathout` = \"", pathout, "\" does not exist --> create")
            if (!dry) if (!dir.create(pathout, recursive=T) && !dir.exists(pathout)) stop("plot ", ploti, ": failed to create `pathout` = \"", pathout, "\"")
        }
    }
    if (!force) {
        exist <- file.exists(fouts)
        if (any(exist)) message("--> already exist and `force` = F --> skip: ", paste0("\"", fouts[exist], "\"", collapse=", "))
        fouts <- fouts[!exist]
    }
    fouts
} # check_fouts

# fall back if `family` is not installed
fc_families <- tryCatch(unique(trimws(unlist(strsplit(system("fc-list : family", intern=T, ignore.stderr=T), ",")))),
                        error=function(e) NULL, warning=function(w) NULL)
if (is.null(fc_families) || family %in% fc_families) {
    family_png <- family_pdf <- family
} else {
    family_png <- "Droid Sans"
    family_pdf <- "sans"
    message("font `family` = \"", family, "\" not installed --> fall back to png \"", family_png, "\", pdf \"", family_pdf, "\"")
}

##################################################################################

message("\nmake ", length(plots), " plot", if (length(plots) != 1) "s", " ...")
for (ploti in seq_along(plots)) {

    job <- plots[[ploti]]
    message("\n************************************************************")
    message("plot ", ploti, "/", length(plots), " ...")

    job_datas <- job_get(job, "datas")
    if (is.null(job_datas) || length(job_datas) == 0) stop("plot ", ploti, " has no `datas`")
    fout_raw <- job_get(job, "fout")
    if (is.null(fout_raw)) stop("plot ", ploti, " has no `fout`")
    # "<plot_from>"/"<plot_to>" in `fout` (see `resolve_fout()`) may need the
    # actual data's own time range as a fallback when this plot has no
    # `plot_from`/`plot_to` of its own -- that isn't known until the data is
    # read, so `fout` can only be resolved (and thus only then checked against
    # `force`) after reading in that case, unlike the normal up-front check
    # that lets a job be skipped before ever reading its data
    needs_defer <- any(grepl("<plot_from>", fout_raw, fixed=T)) || any(grepl("<plot_to>", fout_raw, fixed=T))

    if (!needs_defer) {
        fouts <- resolve_fout(job, ploti)
        message("--> fout = ", paste0("\"", fouts, "\"", collapse=", "))
        fouts <- check_fouts(fouts, ploti)
        if (length(fouts) == 0) next
    }

    job_plot_from <- job_get(job, "plot_from", plot_from)
    job_plot_to <- job_get(job, "plot_to", plot_to)
    datas <- vector("list", length(job_datas))
    for (datai in seq_along(job_datas)) datas[[datai]] <- read_data(job_datas[[datai]], ploti, datai, job_plot_from, job_plot_to)
    ndata <- length(datas)

    if (needs_defer) {
        year_froms <- sapply(datas, "[[", "year_from")
        year_tos <- sapply(datas, "[[", "year_to")
        year_from <- if (all(is.na(year_froms))) NULL else min(year_froms, na.rm=T)
        year_to <- if (all(is.na(year_tos))) NULL else max(year_tos, na.rm=T)
        fouts <- resolve_fout(job, ploti, year_from, year_to)
        message("--> fout = ", paste0("\"", fouts, "\"", collapse=", "))
        fouts <- check_fouts(fouts, ploti)
        if (length(fouts) == 0) next
    }
    ndims <- sapply(datas, function(x) length(x$dims))
    mode <- job_get(job, "mode", "auto")

    if (mode == "pairwise") {
        if (ndata != 2 || !all(ndims == 1)) {
            stop("plot ", ploti, ": `mode` = \"pairwise\" needs exactly 2 `datas`, each reduced to exactly 1 remaining dim; got ",
                 ndata, " datasets with ", paste(ndims, collapse=", "), " dim(s)")
        }
    } else if (mode != "auto") {
        stop("plot ", ploti, ": unknown `mode` = \"", mode, "\" (\"auto\" or \"pairwise\")")
    }

    if (dry) {
        message("--> `dry` = T --> skip actually plotting")
        next
    }

    for (fout in fouts) {
        message("--> plot \"", fout, "\" ...")

        ext <- tolower(tools::file_ext(fout))
        open_dev <- function(width, height, path=fout) {
            if (ext == "png") {
                grDevices::png(path, width=width, height=height, res=res, family=family_png)
            } else if (ext == "pdf") {
                grDevices::cairo_pdf(path, width=width/res, height=height/res, family=family_pdf)
            }
            par(mgp=mgp, tcl=tcl)
        }
        # axis titles just outside the tick labels
        xtitle <- function(xlab) title(xlab=xlab, line=mgp[2] + strheight("0", units="inches")/par("csi") + 0.25)
        # ticks within the drawn range (also if the data range is 0)
        usr_at <- function(side, n=10, time=F) {
            r <- par("usr")[if (side == 1) 1:2 else 3:4]
            at <- if (time) pretty(as.POSIXct(r, origin="1970-1-1", tz="UTC"), n=n) else pretty(r, n=n)
            at[at >= r[1] & at <= r[2]]
        }
        ylabs_width <- function(at) max(strwidth(format(at, trim=T), units="inches"))/par("csi") # lines
        yaxis <- function(at, ylab) {
            axis(2, at=at, labels=format(at, trim=T), las=2)
            title(ylab=ylab, line=mgp[2] + ylabs_width(at) + 0.5)
        }
        # left margin wide enough for the y tick labels and title; call before plot()
        set_ymar <- function(yr) {
            if (diff(yr) == 0) yr <- if (yr[1] == 0) c(-1, 1) else yr + c(-0.4, 0.4)*abs(yr[1]) # as R does
            r <- yr + c(-0.04, 0.04)*diff(yr) # yaxs="r"
            at <- pretty(r, n=10)
            at <- at[at >= r[1] & at <= r[2]]
            par(mar=c(par("mar")[1], mgp[2] + ylabs_width(at) + 0.5 + 1.3, par("mar")[3:4]))
        }

        if (mode == "pairwise" || (mode == "auto" && all(ndims == 1))) {

            # lengths may differ, the dim must be the same
            if (mode == "auto" && ndata > 1) {
                shared_dim <- datas[[1]]$dims[1]
                for (datai in 2:ndata) {
                    if (datas[[datai]]$dims[1] != shared_dim) {
                        stop("plot ", ploti, ": data 1 and data ", datai, " don't share the same remaining dim (\"",
                             shared_dim, "\" vs \"", datas[[datai]]$dims[1], "\") -- cannot overlay on 1 line plot")
                    }
                }
            }

            message("--> line plot (", if (mode == "pairwise") "pairwise data1 vs data2" else paste0(ndata, " datasets vs \"", datas[[1]]$dims[1], "\""), ") ...")
            open_dev(line_width, line_height)
            if (mode == "pairwise") {
                x <- as.vector(datas[[1]]$data); y <- as.vector(datas[[2]]$data)
                set_ymar(range(y, finite=T))
                graphics::plot(x, y, type="n", xlab="", ylab="", xaxt="n", yaxt="n")
                axis(1, at=usr_at(1))
                xtitle(data_label(datas[[1]]))
                yaxis(usr_at(2), data_label(datas[[2]]))
                abline(h=0, v=0, col="gray", lwd=0.5)
                lines(x, y, col=job_get(job_datas[[1]], "col", default_cols[1]), lwd=job_get(job_datas[[1]], "lwd", 1))
                message("run `my_dev.off(", fout, ") ...")
                my_dev.off(fout)
            } else {
                xvals_list <- lapply(datas, function(dm) maybe_time_axis(dm$dims[1], dm$dimvals[[1]], dm$dimunits[1]))
                is_time <- inherits(xvals_list[[1]], "POSIXct")

                # draws 1 version of this line-vs-time(/dim) plot into the
                # currently open device, using whatever `$data` is in
                # `datas_draw` -- shared between the normal (raw `datas`) and,
                # if `plot_ma`, the running-mean (`datas_ma`, below) render,
                # which otherwise differ only in that 1 field
                draw_ts <- function(datas_draw) {
                    yr <- range(sapply(datas_draw, function(x) range(x$data, finite=T, na.rm=T)))
                    xr <- range(if (is_time) do.call(c, xvals_list) else unlist(xvals_list), finite=T)
                    set_ymar(yr)
                    graphics::plot(0, type="n", xlim=xr, ylim=yr,
                         xlab="", ylab="", xaxt="n", yaxt="n")
                    if (is_time) {
                        axis.POSIXct(1, at=usr_at(1, time=T), format="%Y")
                        xtitle("year")
                    } else {
                        axis(1, at=usr_at(1))
                        xtitle(axis_label(datas_draw[[1]]$dims[1], datas_draw[[1]]$dimunits[1]))
                    }
                    yaxis(usr_at(2), job_get(job, "ylab", data_label(datas_draw[[1]])))
                    abline(h=0, col="gray", lwd=0.5)
                    for (datai in seq_len(ndata)) {
                        dj <- job_datas[[datai]]
                        col <- job_get(dj, "col", default_cols[(datai - 1) %% length(default_cols) + 1])
                        lty <- job_get(dj, "lty", 1)
                        lwd <- job_get(dj, "lwd", 1)
                        lines(xvals_list[[datai]], as.vector(datas_draw[[datai]]$data), col=col, lty=lty, lwd=lwd)
                    }
                    if (ndata > 1) {
                        pos <- job_get(job, "legend_pos", "my_maxempty")
                        if (identical(pos, "my_maxempty")) {
                            pos <- tryCatch(my_maxempty(lapply(xvals_list, as.numeric), lapply(datas_draw, function(dm) as.vector(dm$data))),
                                            error=function(e) {
                                                message("`my_maxempty()` failed (", conditionMessage(e), ") --> legend at \"topleft\"")
                                                "topleft"
                                            })
                        }
                        if (is.numeric(pos)) pos <- list(x=pos[1], y=pos[2])
                        lg_args <- list(legend=sapply(datas_draw, "[[", "label"),
                                        col=sapply(seq_len(ndata), function(i) job_get(job_datas[[i]], "col", default_cols[(i - 1) %% length(default_cols) + 1])),
                                        lty=sapply(job_datas, function(dj) job_get(dj, "lty", 1)),
                                        lwd=sapply(job_datas, function(dj) job_get(dj, "lwd", 1)), bty="n",
                                        ncol=job_get(job, "legend_ncol", if (ndata > 5) 2 else 1),
                                        cex=job_get(job, "legend_cex", if (ndata > 5) 0.8 else 1),
                                        x.intersp=job_get(job, "legend_x_intersp", 0.5))
                        if (is.list(pos)) { # keep the whole legend inside the plot
                            usr <- par("usr")
                            r <- do.call(legend, c(list(x=pos$x, y=pos$y, plot=F), lg_args))$rect
                            pos$x <- max(usr[1], min(pos$x, usr[2] - r$w))
                            pos$y <- min(usr[4], max(pos$y, usr[3] + r$h))
                        }
                        do.call(legend, c(list(x=pos), lg_args))
                    }
                } # draw_ts

                draw_ts(datas)
                message("run `my_dev.off(", fout, ") ...")
                my_dev.off(fout)

                # `plot_ma`: also save a running-mean version of this same
                # plot, analogous to post.r's `save_fout_an` -- only makes
                # sense for an actual time axis, not an arbitrary dim
                job_plot_ma <- job_get(job, "plot_ma", plot_ma)
                if (job_plot_ma && is_time) {
                    job_ma_opts <- job_get(job, "ma_opts", list()) # plot-level default, cascades to any dataset without its own `ma`
                    job_ma_nyrs <- job_get(job_ma_opts, "nyrs", ma_nyrs) # nominal window (years) for `fout_ma`'s name -- a dataset's own `ma$nyrs` (if any) can still differ, this is just the label
                    fout_ma <- sub("(\\.[^./]+)$", paste0("_ma", job_ma_nyrs, "\\1"), fout)
                    if (fout_ma == fout) stop("plot ", ploti, ": could not derive a \"_ma\" `fout` from \"", fout, "\"")
                    if (file.exists(fout_ma) && !force) {
                        message("--> `plot_ma` = T but \"", fout_ma, "\" already exists and `force` = F --> skip")
                    } else {
                        message("--> `plot_ma` = T: also plot a running-mean version --> \"", fout_ma, "\" ...")
                        datas_ma <- lapply(seq_len(ndata), function(datai) {
                            dm <- datas[[datai]]
                            ma_cfg <- job_get(job_datas[[datai]], "ma", list())
                            ma_opts <- list(nyrs=job_get(ma_cfg, "nyrs", job_ma_nyrs),
                                       method="data.table::frollmean", # fastest of the options considered; handles NA; fills start/end only for align="right" (`adaptive=T`, below)
                                       align=job_get(ma_cfg, "align", job_get(job_ma_opts, "align", "center")))
                            yvals <- as.vector(dm$data)
                            dt_secs <- stats::median(diff(as.numeric(xvals_list[[datai]])))
                            n <- if (is.na(dt_secs) || dt_secs <= 0) NA_integer_ else as.integer(max(1, round(ma_opts$nyrs*365.2425*86400/dt_secs)))
                            if (is.na(n) || n < 2) {
                                message("--> data ", datai, ": could not derive a running-mean window (dt = ", dt_secs, " s) --> leave unsmoothed")
                            } else if (ma_opts$align == "right") {
                                # fills the start via an expanding window instead of leaving it unplotted, like `zoo::rollapply(align="right", partial=T)`
                                dm$data <- data.table::frollmean(yvals, n=pmin(seq_along(yvals), n), align="right", na.rm=T, adaptive=T)
                            } else {
                                dm$data <- data.table::frollmean(yvals, n=n, align=ma_opts$align, na.rm=T)
                            }
                            dm
                        })
                        open_dev(line_width, line_height, path=fout_ma)
                        draw_ts(datas_ma)
                        message("run `my_dev.off(", fout_ma, ") ...")
                        my_dev.off(fout_ma)
                        if (!file.exists(fout_ma)) stop("plot ", ploti, ": `fout_ma` = \"", fout_ma, "\" was not created")
                        message("--> done: \"", fout_ma, "\"")
                    }
                }
            }

        } else if (all(ndims == 2)) {

            # time (if any) on the x axis
            x <- vector("list", ndata); y <- vector("list", ndata); z <- vector("list", ndata)
            dims_ordered <- vector("list", ndata)
            for (datai in seq_len(ndata)) {
                dm <- datas[[datai]]
                time_ind <- which(!is.na(match(dm$dims, known_time_dimnames)))
                ord <- if (length(time_ind) == 1) c(time_ind, setdiff(1:2, time_ind)) else 1:2
                x[[datai]] <- dm$dimvals[[ord[1]]]
                y[[datai]] <- dm$dimvals[[ord[2]]]
                z[[datai]] <- if (ord[1] == 1) dm$data else t(dm$data)
                dims_ordered[[datai]] <- list(names=dm$dims[ord], units=dm$dimunits[ord])
            }
            znames <- sapply(datas, "[[", "label")
            zlim <- job_get(job, "zlim", range(z, finite=T, na.rm=T))
            nlevels <- job_get(job, "nlevels", 11)
            zlab <- job_get(job, "zlab", data_label(datas[[1]]))

            message("--> contour plot (", ndata, " panel(s), zlim = [", round(zlim[1], 3), ", ", round(zlim[2], 3), "]) ...")
            ip <- image.plot.pre(zlim=zlim, nlevels=nlevels)
            open_dev(if (ndata > 1) contour_width else contour_width/1.3, contour_height)
            args <- list(x=x, y=y, z=z, ip=ip,
                         xlab=axis_label(dims_ordered[[1]]$names[1], dims_ordered[[1]]$units[1]),
                         ylab=axis_label(dims_ordered[[1]]$names[2], dims_ordered[[1]]$units[2]),
                         zlab=zlab, znames_labels=znames, plot_type="active")
            job_n <- job_get(job, "n"); if (!is.null(job_n)) args$n <- job_n
            job_m <- job_get(job, "m"); if (!is.null(job_m)) args$m <- job_m
            do.call(image.plot.nxm, args)
            message("run `my_dev.off(", fout, ") ...")
            my_dev.off(fout)

        } else {
            stop("plot ", ploti, ": unsupported/mismatched data shape(s) for `mode` = \"", mode, "\" -- ", ndata, " datasets with ",
                 paste(ndims, collapse=", "), " remaining dim(s) each (", paste(sapply(datas, function(x) paste(x$dims, collapse="+")), collapse=" | "), ")")
        }

        if (!file.exists(fout)) stop("plot ", ploti, ": `fout` = \"", fout, "\" was not created")
        message("--> done: \"", fout, "\"")

    } # for fout

} # for ploti

message("\n****************** plot.r finish ******************")
