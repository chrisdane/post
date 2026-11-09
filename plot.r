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
#   fout    - output file(s), ".png" and/or ".pdf"; "<plot_from>"/"<plot_to>" are replaced by the plotted years
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
#     ma       - [optional] line plots only; list(nyrs=, align="center"|"right") running mean, overrides `ma_opts`
#   plot_from,
#   plot_to - [optional] calendar-year range to plot; per plot, per dataset or global config
#   width, height, res - [optional] png px and ppi (pdf: width/res, height/res inch); default: plot_sizes()
#   ylab    - [optional] line plots only; default: label and units of the 1st dataset
#   legend_pos - [optional] line plots only; "my_maxempty" (default; "topleft" if it fails),
#             a `legend()` keyword or c(x, y)
#   legend_ncol, legend_cex - [optional] default: 2 and 0.8 if more than 5 datasets, else 1 and 1
#   legend_x_intersp - [optional] space between line and text in character widths; default: 0.5
#   plot_ma - [optional] also save a running-mean version ("_ma<nyrs>" before the extension); default: config `plot_ma`
#   ma_opts - [optional] list(nyrs=, align=) for datasets without own `ma`; overrides config `ma_nyrs`
#   zlim, nlevels, n, m - [optional] contour plots only; passed to image.plot.pre()/image.plot.nxm()
#   anom    - [optional] contour plots (2 dims) only; if T, plot every pairwise difference (data_j - data_i,
#             interpolated onto data_i's grid, for all i < j) instead of the datasets themselves
#   zlim_quantiles - [optional] contour plots only; e.g. c(0.1, 0.9): most colors between these quantiles of all panels,
#             with the min and max of the data at the ends of the colorbar
#   anom_colorbar - [optional] contour plots only; F: sequential colors (Spectral) also if zlim includes 0; default: automatic
#   anomaly - [optional] contour plots with 2 datasets only; if T: also save "data 2 minus data 1" ("_anom" before the extension);
#             data 2 is interpolated bilinearly to the axes of data 1 (`fields::interp.surface.grid`), NA if any of the 4 surrounding values is NA; no interpolation if the axes are identical; skipped if the units differ
#   anomaly_zlim - [optional] default: symmetric around 0, from the 99th percentile of the absolute anomaly
#   anomaly_zlab - [optional] colorbar label of the anomaly plot; default: "anomaly [<units of data 1>]"
#   same_units - [optional] if T: the units of both datasets are the same after `fac`, also if the `units` attributes differ
# units in labels are drawn with sub-/superscripts, e.g. "mmolC/m2/d" --> mmolC m^-2 d^-1 (see `fmt_label()`)

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

# not `job$x`: partial matching would return e.g. job$mode for job$m; NULL = not given
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
if (!user_set("plot_from")) plot_from <- NULL # calendar-year range of all plots; overridden per plot/dataset
if (!user_set("plot_to")) plot_to <- NULL
if (!user_set("plot_ma")) plot_ma <- F # if T: also save running-mean versions; see header
if (!user_set("ma_nyrs")) ma_nyrs <- 21 # running-mean window in years

##################################################################################

# time dim names; time goes on the x axis of contour plots
known_time_dimnames <- c("time", "Time", "TIME", "mon")
# depth/level dim names (varies by data source, e.g. glodap's mismatched "depth_surface", FESOM's "nz",
# buitenhuis_etal_2013's upper-case "DEPTH"); depth goes on the inverted y axis of contour plots
# (`mode` = "auto", 2 dims, no time) and is the sole axis of a `mode` = "profile" line plot (1 dim)
known_depth_dimnames <- c("depth", "Depth", "DEPTH", "depth_surface", "nz", "lev", "level", "Level", "LEVEL")
known_lat_dimnames <- c("lat", "Lat", "LAT", "latitude", "Latitude", "LATITUDE")

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
    # fast: `fin` relative to `pathpost`
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

# bilinear-style interpolation of a regular-grid `z_src` (dim length(x_src) x length(y_src)) onto
# `x_tgt`/`y_tgt`, as 2 separable 1-dim passes (`approx()` along x, then along y); used by `anom` (see
# header) to difference 2 datasets that are on different, but each individually regular, grids (e.g.
# obs vs FESOM's own zonmean/fldmean lat or depth bins). NA outside `x_src`/`y_src`'s range (no extrapolation).
# exception: a 12-step POSIXct x (a "_ymon" monthly climatology) is matched by *position*, not value --
# cdo's `ymonmean` gives each file its own, mutually non-comparable absolute reference date per month
# (see `month_ticks` below), so value-based interpolation would wrongly see disjoint ranges and return all-NA.
interp_grid <- function(x_src, y_src, z_src, x_tgt, y_tgt) {
    if (inherits(x_src, "POSIXct") && inherits(x_tgt, "POSIXct") && length(x_src) == length(x_tgt)) {
        tmp <- z_src
    } else {
        tmp <- matrix(NA_real_, nrow=length(x_tgt), ncol=length(y_src))
        for (j in seq_along(y_src)) {
            v <- z_src[, j]
            if (sum(!is.na(v)) >= 2) tmp[, j] <- approx(x_src, v, xout=x_tgt)$y # else: leave as NA (e.g. an all-masked row)
        }
    }
    out <- matrix(NA_real_, nrow=length(x_tgt), ncol=length(y_tgt))
    for (i in seq_len(nrow(tmp))) {
        v <- tmp[i, ]
        if (sum(!is.na(v)) >= 2) out[i, ] <- approx(y_src, v, xout=y_tgt)$y
    }
    out
} # interp_grid

# read 1 dataset; `plot_from`/`plot_to` (may be NULL) overridden by those of `d`
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
    time_dim_name <- if (length(time_dim_ind) == 1) vardims[time_dim_ind] else NA_character_

    # read only the needed time steps
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

    # time range in calendar years, for "<plot_from>"/"<plot_to>"
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
    dimvals_out <- dimvals[vardims]
    # `timep`: the time dim's values as POSIXct, alongside its raw values under its own name (`dims`/`dimunits`
    # stay as the actual netcdf dims, so this does not affect anything that counts/uses those, e.g. `length(x$dims)`)
    if (!is.na(time_dim_name) && time_dim_name %in% vardims && inherits(tv, "POSIXct")) dimvals_out$timep <- tv
    list(data=arr, dims=vardims, dimvals=dimvals_out, dimunits=dimunits[vardims], varname=varname, units=units, label=label,
         year_from=year_from, year_to=year_to)
} # read_data

# "mmolC/m2/d", "kg m-2 s-1", "micro-mol kg-1", ... --> data.frame(base, exp); NULL if dimensionless
unit_tokens <- function(u) {
    u <- trimws(u)
    if (!nzchar(u) || u == "1") return(NULL)
    aliases <- c("micro-mol"="umol", "micromol"="umol", "milligram"="mg", "milligrams"="mg", "degrees_celsius"="degC", "degree_celsius"="degC",
                 "degree_C"="degC", "degrees_C"="degC", "celsius"="degC", "Celsius"="degC", "meter"="m", "meters"="m", "metre"="m", "metres"="m")
    parts <- strsplit(u, "/", fixed=T)[[1]]
    out <- data.frame(base=character(), exp=integer())
    for (pi in seq_along(parts)) {
        for (tok in strsplit(trimws(parts[pi]), "[[:space:]*]+")[[1]]) {
            if (tok %in% names(aliases)) tok <- aliases[[tok]]
            m <- regmatches(tok, regexec("^([[:alpha:]]+)\\^?(-?[0-9]+)$", tok))[[1]]
            is_exp <- length(m) == 3 && !(m[2] %in% c("CO", "O", "N", "H", "NO", "PO")) # CO2 is no exponent
            base <- if (is_exp) m[2] else tok
            exp <- if (is_exp) as.integer(m[3]) else 1L
            out[nrow(out) + 1,] <- list(base, if (pi > 1) -exp else exp)
        }
    }
    out
} # unit_tokens

# plotmath string of units
unit_math <- function(u) {
    tok <- unit_tokens(u)
    if (is.null(tok)) return(NULL)
    terms <- mapply(function(b, e) {
        if (grepl("^(CO|O|N|H|NO|PO)[0-9]+$", b)) return(sub("^([[:alpha:]]+)([0-9]+)$", "\\1[\\2]", b))
        b <- if (b == "degC") "\u00b0C" else if (b %in% c("umol", "uatm", "um", "ug")) paste0("\u00b5", substring(b, 2)) else b
        q <- paste0("\"", gsub("\"", "\\\\\"", b), "\"")
        if (e == 1) q else paste0(q, "^{", e, "}")
    }, tok$base, tok$exp, USE.NAMES=F)
    paste(terms, collapse="~")
} # unit_math

# "flux [PgC/yr] (>0: into ocean)" --> expression with sub-/superscripts in the units; as is if not possible
fmt_label <- function(x) {
    if (!is.character(x) || length(x) != 1) return(x)
    m <- regmatches(x, regexec("^(.*)\\[([^]]*)\\](.*)$", x))[[1]]
    if (length(m) != 4) return(x)
    tryCatch({
        q <- function(s) paste0("\"", gsub("\"", "\\\\\"", gsub("\\", "\\\\", s, fixed=T)), "\"")
        um <- unit_math(m[3])
        if (is.null(um)) return(x)
        parse(text=paste0("paste(", q(paste0(m[2], "[")), ", ", um, ", ", q(paste0("]", m[4])), ")"))
    }, error=function(e) x)
} # fmt_label

# same units after normalizing the spelling
same_units <- function(u1, u2) {
    t1 <- unit_tokens(u1); t2 <- unit_tokens(u2)
    if (is.null(t1) || is.null(t2)) return(is.null(t1) && is.null(t2))
    identical(sort(paste(t1$base, t1$exp)), sort(paste(t2$base, t2$exp)))
} # same_units

axis_label <- function(dimname, dimunits) if (nzchar(dimunits)) paste0(dimname, " [", dimunits, "]") else dimname
data_label <- function(d) {
    units <- sub("^\\[(.*)\\]$", "\\1", d$units) # several FESOM/REcoM vars store units already bracketed, e.g. "[mmol/m3]"
    if (nzchar(units)) paste0(d$label, " [", units, "]") else d$label
}

# time dim with "<unit> since <origin>" (s/min/h/d), cdo's "day as %Y%m%d" or "month as %Y%m.%f" units
# --> POSIXct; else unchanged
maybe_time_axis <- function(dimname, vals, units) {
    if (is.na(match(dimname, known_time_dimnames)) || !nzchar(units)) return(vals)
    if (grepl("^\\s*day as %Y%m%d", units)) {
        return(as.POSIXct(strptime(sprintf("%08d", as.integer(floor(vals))), "%Y%m%d", tz="UTC")) + (vals %% 1)*86400)
    }
    if (grepl("^\\s*month as %Y%m", units)) { # value = year*100 + month (+ a %f fraction-of-month, ignored)
        yyyymm <- as.integer(floor(vals))
        return(as.POSIXct(strptime(sprintf("%06d01", yyyymm), "%Y%m%d", tz="UTC")))
    }
    m <- regmatches(units, regexec("^\\s*([[:alpha:]]+)\\s+since\\s+(.+?)\\s*$", units))[[1]]
    if (length(m) != 3) return(vals)
    unit <- tolower(m[2])
    mult <- c(second=1, seconds=1, sec=1, secs=1, s=1, minute=60, minutes=60, min=60, mins=60,
              hour=3600, hours=3600, hr=3600, hrs=3600, h=3600, day=86400, days=86400, d=86400)[unit]
    is_month <- unit %in% c("month", "months", "mon", "mons") # not a fixed duration -- handled via POSIXlt below
    if (is.na(mult) && !is_month) return(vals)
    origin_str <- sub("T", " ", trimws(m[3]), fixed=T)
    if (grepl("^[0-9]+$", origin_str)) origin_str <- paste0(origin_str, "-01-01 00:00:00")
    else if (grepl("^[0-9]+-[0-9]{1,2}$", origin_str)) origin_str <- paste0(origin_str, "-01 00:00:00")
    else if (grepl("^[0-9]+-[0-9]{1,2}-[0-9]{1,2}$", origin_str)) origin_str <- paste0(origin_str, " 00:00:00")
    else if (grepl("^[0-9]+-[0-9]{1,2}-[0-9]{1,2} [0-9]{1,2}:[0-9]{1,2}$", origin_str)) origin_str <- paste0(origin_str, ":00")
    origin <- as.POSIXct(strptime(origin_str, format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
    if (is.na(origin)) return(vals)
    if (is_month) {
        lt <- as.POSIXlt(rep(origin, length(vals)), tz="UTC")
        lt$mon <- lt$mon + vals
        return(as.POSIXct(lt))
    }
    origin + vals*mult
} # maybe_time_axis

# `year_from`/`year_to` (may be NULL): data time range, fallback for "<plot_from>"/"<plot_to>"
resolve_fout <- function(job, ploti, year_from=NULL, year_to=NULL) {
    fout <- job_get(job, "fout")
    if (is.null(fout)) stop("plot ", ploti, " has no `fout`")
    if (!is.character(fout) || length(fout) == 0) stop("plot ", ploti, ": `fout` must be a non-empty character vector")
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

# check `fouts` extensions, create dirs, drop existing ones unless `force`
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
    # fout with "<plot_from>"/"<plot_to>" and no plot_from/plot_to: resolved after reading the data
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
    } else if (mode == "fldmean") { # a vertical profile, as from post.r's `mode` = "fldmean" -- value on x, depth on inverted y
        if (!all(ndims == 1)) {
            stop("plot ", ploti, ": `mode` = \"fldmean\" needs every `data` reduced to exactly 1 remaining dim; got ",
                 paste(ndims, collapse=", "), " dim(s)")
        }
        if (any(is.na(match(sapply(datas, function(x) x$dims[1]), known_depth_dimnames)))) {
            stop("plot ", ploti, ": `mode` = \"fldmean\" needs every `data`'s remaining dim to be a recognized depth dim (",
                 paste(known_depth_dimnames, collapse=", "), "); got ", paste(sapply(datas, function(x) x$dims[1]), collapse=", "))
        }
    } else if (mode != "auto") {
        stop("plot ", ploti, ": unknown `mode` = \"", mode, "\" (\"auto\", \"pairwise\" or \"fldmean\")")
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

        if (mode == "pairwise" || mode == "fldmean" || (mode == "auto" && all(ndims == 1))) {

            # lengths may differ, the dim must be the same -- except for `mode` = "fldmean", where the
            # dim-validation above already confirms every dataset's remaining dim is depth-like (via
            # `known_depth_dimnames`); the exact dim *name* legitimately differs across datasets there
            # (e.g. "DEPTH" vs "nz"), that is the entire point of recognizing multiple aliases
            if (mode == "auto" && ndata > 1) {
                shared_dim <- datas[[1]]$dims[1]
                # the same kind of dim may have another name per data source, e.g. lat/latitude, depth/Depth
                same_kind <- function(d1, d2) d1 == d2 || (d1 %in% known_depth_dimnames && d2 %in% known_depth_dimnames) ||
                                              (d1 %in% known_lat_dimnames && d2 %in% known_lat_dimnames)
                for (datai in 2:ndata) {
                    if (!same_kind(datas[[datai]]$dims[1], shared_dim)) {
                        stop("plot ", ploti, ": data 1 and data ", datai, " don't share the same remaining dim (\"",
                             shared_dim, "\" vs \"", datas[[datai]]$dims[1], "\") -- cannot overlay on 1 line plot")
                    }
                }
            }

            message("--> line plot (", if (mode == "pairwise") "pairwise data1 vs data2" else if (mode == "fldmean")
                    paste0(ndata, " profile(s) vs \"", datas[[1]]$dims[1], "\"") else paste0(ndata, " datasets vs \"", datas[[1]]$dims[1], "\""), ") ...")
            open_dev(line_width, line_height)
            if (mode == "pairwise") {
                x <- as.vector(datas[[1]]$data); y <- as.vector(datas[[2]]$data)
                set_ymar(range(y, finite=T))
                graphics::plot(x, y, type="n", xlab="", ylab="", xaxt="n", yaxt="n")
                axis(1, at=usr_at(1))
                xtitle(fmt_label(data_label(datas[[1]])))
                yaxis(usr_at(2), fmt_label(data_label(datas[[2]])))
                abline(h=0, v=0, col="gray", lwd=0.5)
                lines(x, y, col=job_get(job_datas[[1]], "col", default_cols[1]), lwd=job_get(job_datas[[1]], "lwd", 1))
                message("run `my_dev.off(", fout, ") ...")
                my_dev.off(fout)
            } else if (mode == "fldmean") {
                # depth (inverted: shallow at top) on y, value on x -- the conventional oceanographic
                # vertical-profile orientation, unlike the regular dim-on-x line plot
                depth_list <- lapply(datas, function(dm) dm$dimvals[[1]])
                xr <- range(sapply(datas, function(x) range(x$data, finite=T, na.rm=T)))
                depth_range <- range(unlist(depth_list), finite=T)
                set_ymar(depth_range)
                graphics::plot(0, type="n", xlim=xr, ylim=rev(depth_range), xlab="", ylab="", xaxt="n", yaxt="n")
                axis(1, at=usr_at(1)); xtitle(fmt_label(job_get(job, "ylab", data_label(datas[[1]]))))
                yaxis(pretty(par("usr")[4:3], n=10), fmt_label(axis_label(datas[[1]]$dims[1], datas[[1]]$dimunits[1])))
                abline(v=0, col="gray", lwd=0.5)
                for (datai in seq_len(ndata)) {
                    dj <- job_datas[[datai]]
                    lines(as.vector(datas[[datai]]$data), depth_list[[datai]], col=job_get(dj, "col", default_cols[(datai - 1) %% length(default_cols) + 1]),
                          lty=job_get(dj, "lty", 1), lwd=job_get(dj, "lwd", 1))
                }
                if (ndata > 1) {
                    pos <- job_get(job, "legend_pos", "bottomright") # profiles are mostly in the upper part
                    if (is.numeric(pos)) pos <- list(x=pos[1], y=pos[2])
                    do.call(legend, c(list(x=pos), list(legend=sapply(datas, "[[", "label"),
                             col=sapply(seq_len(ndata), function(i) job_get(job_datas[[i]], "col", default_cols[(i - 1) %% length(default_cols) + 1])),
                             lty=sapply(job_datas, function(dj) job_get(dj, "lty", 1)), lwd=sapply(job_datas, function(dj) job_get(dj, "lwd", 1)), bty="n")))
                }
                message("run `my_dev.off(", fout, ") ...")
                my_dev.off(fout)
            } else {
                xvals_list <- lapply(datas, function(dm) maybe_time_axis(dm$dims[1], dm$dimvals[[1]], dm$dimunits[1]))
                is_time <- inherits(xvals_list[[1]], "POSIXct")

                # draw 1 version of the plot (raw or running mean)
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
                        xtitle(fmt_label(axis_label(datas_draw[[1]]$dims[1], datas_draw[[1]]$dimunits[1])))
                    }
                    yaxis(usr_at(2), fmt_label(job_get(job, "ylab", data_label(datas_draw[[1]]))))
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

                # `plot_ma`: running-mean version; only for a time axis
                job_plot_ma <- job_get(job, "plot_ma", plot_ma)
                if (job_plot_ma && is_time) {
                    job_ma_opts <- job_get(job, "ma_opts", list()) # plot-level default, cascades to any dataset without its own `ma`
                    job_ma_nyrs <- job_get(job_ma_opts, "nyrs", ma_nyrs) # window in `fout_ma`'s name
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
                                       method="data.table::frollmean", # handles NA
                                       align=job_get(ma_cfg, "align", job_get(job_ma_opts, "align", "center")))
                            yvals <- as.vector(dm$data)
                            dt_secs <- stats::median(diff(as.numeric(xvals_list[[datai]])))
                            n <- if (is.na(dt_secs) || dt_secs <= 0) NA_integer_ else as.integer(max(1, round(ma_opts$nyrs*365.2425*86400/dt_secs)))
                            if (is.na(n) || n < 2) {
                                message("--> data ", datai, ": could not derive a running-mean window (dt = ", dt_secs, " s) --> leave unsmoothed")
                            } else if (ma_opts$align == "right") {
                                # expanding window at the start
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

            # time (if any), else depth, on the x axis -- wait, on the y axis: depth is more naturally the
            # y axis (inverted: shallow at top), as for a zonal-mean section (lat on x, depth on y)
            x <- vector("list", ndata); y <- vector("list", ndata); z <- vector("list", ndata)
            dims_ordered <- vector("list", ndata)
            is_depth_y <- is_month_x <- is_month_y <- F
            for (datai in seq_len(ndata)) {
                dm <- datas[[datai]]
                time_ind <- which(!is.na(match(dm$dims, known_time_dimnames)))
                depth_ind <- which(!is.na(match(dm$dims, known_depth_dimnames)))
                ord <- if (length(time_ind) == 1) c(time_ind, setdiff(1:2, time_ind))
                       else if (length(depth_ind) == 1) c(setdiff(1:2, depth_ind), depth_ind)
                       else 1:2
                if (length(depth_ind) == 1 && ord[2] == depth_ind) is_depth_y <- T # depth ended up on y (e.g.
                    # fldmean_ymon's time-on-x, depth-on-y -- not just the no-time-dim case below)
                # convert a recognized time dim to POSIXct (image.plot.nxm formats it as calendar dates/
                # years via `axis.POSIXct` automatically); raw "<n> seconds/days since ..." values
                # otherwise, as for the 1-dim line-plot path above
                x[[datai]] <- maybe_time_axis(dm$dims[ord[1]], dm$dimvals[[ord[1]]], dm$dimunits[ord[1]])
                y[[datai]] <- maybe_time_axis(dm$dims[ord[2]], dm$dimvals[[ord[2]]], dm$dimunits[ord[2]])
                z[[datai]] <- if (ord[1] == 1) dm$data else t(dm$data)
                # a 12-step POSIXct axis is a monthly climatology (e.g. post.r's "..._ymon" modes): its dates have nothing
                # to do with the plotted years. cdo's `ymonmean` assigns each month's *own* mean timestamp, which can be in
                # different and non-chronological years per month, and obs and model are in different years, so that a common
                # time axis squeezes each panel into a sliver --> 1, ..., 12, sorted, for all panels
                if (inherits(x[[datai]], "POSIXct") && length(x[[datai]]) == 12) {
                    mons <- as.POSIXlt(x[[datai]], tz="UTC")$mon + 1
                    if (anyDuplicated(mons)) stop("plot ", ploti, ", data ", datai, ": 12 time steps but not 12 different months: ", paste(mons, collapse=", "))
                    z[[datai]] <- z[[datai]][order(mons), , drop=F]
                    x[[datai]] <- sort(mons)
                    is_month_x <- T
                }
                dims_ordered[[datai]] <- list(names=dm$dims[ord], units=dm$dimunits[ord])
            }
            znames <- sapply(datas, "[[", "label")

            if (job_get(job, "anom", F)) {
                if (ndata < 2) stop("plot ", ploti, ": `anom`=T needs >= 2 datasets, got ", ndata)
                combi <- combn(ndata, 2)
                anom_x <- vector("list", ncol(combi)); anom_y <- anom_x; anom_z <- anom_x
                anom_names <- character(ncol(combi)); anom_dims_ordered <- vector("list", ncol(combi))
                for (pi in seq_len(ncol(combi))) {
                    i <- combi[1, pi]; j <- combi[2, pi] # i: reference grid; j: interpolated onto i
                    anom_x[[pi]] <- x[[i]]; anom_y[[pi]] <- y[[i]]
                    anom_z[[pi]] <- interp_grid(x[[j]], y[[j]], z[[j]], x[[i]], y[[i]]) - z[[i]]
                    anom_names[pi] <- paste0(znames[j], " - ", znames[i])
                    anom_dims_ordered[[pi]] <- dims_ordered[[i]]
                }
                x <- anom_x; y <- anom_y; z <- anom_z; znames <- anom_names; dims_ordered <- anom_dims_ordered
                ndata <- length(x)
                message("--> `anom`=T --> ", ndata, " pairwise difference(s): ", paste(znames, collapse=", "))
            }
            # default: 10th/90th percentile, not the full min/max -- a handful of near-zero outliers
            # (e.g. obs/model noise at depth) can otherwise stretch `zlim` just enough to cross 0 and
            # spuriously trigger image.plot.pre()'s auto diverging ("anomaly") colorbar for an otherwise
            # all-positive field, and in general compresses the color scale's useful contrast; `anom`=T
            # plots are the difference itself, so keep their full range -- clipping it would hide exactly
            # the (potentially large) bias regions an anomaly plot exists to show
            # `zlim_quantiles` also needs the full range: its min and max are the ends of the colorbar
            zlim <- job_get(job, "zlim", if (job_get(job, "anom", F) || !is.null(job_get(job, "zlim_quantiles"))) range(z, finite=T, na.rm=T)
                                          else quantile(unlist(z), probs=c(0.1, 0.9), na.rm=T))
            nlevels <- job_get(job, "nlevels", 11)
            # default to the *last* dataset's label (by convention the model, e.g. "salt", vs. the 1st/obs
            # dataset's dataset-prefixed label, e.g. "woa salt") -- a comparison plot's colorbar should read
            # as the physical variable being compared, not one side's own dataset name
            zlab <- fmt_label(job_get(job, "zlab", data_label(datas[[length(datas)]])))

            message("--> contour plot (", ndata, " panel(s), zlim = [", round(zlim[1], 3), ", ", round(zlim[2], 3), "]) ...")
            ip_args <- list(zlim=zlim, nlevels=nlevels, anom_colorbar=job_get(job, "anom_colorbar"))
            zq <- job_get(job, "zlim_quantiles")
            if (!is.null(zq)) {
                zlevels <- as.numeric(stats::quantile(unlist(z), zq, na.rm=T))
                if (zlevels[1] < zlevels[2]) {
                    message("--> `zlim_quantiles` = ", paste(zq, collapse=", "), ": zoom colorbar to [", signif(zlevels[1], 3), ", ", signif(zlevels[2], 3), "]")
                    ip_args <- c(ip_args, list(zlevels=zlevels, axis.zoom=T))
                }
            }
            ip <- do.call(image.plot.pre, ip_args)
            open_dev(if (ndata > 1) contour_width else contour_width/1.3, contour_height)
            axlab <- function(i) if (is_month_x && i == 1) "month"
                                  else if (inherits(x[[1]], "POSIXct") && i == 1 || inherits(y[[1]], "POSIXct") && i == 2) "time"
                                  else fmt_label(axis_label(dims_ordered[[1]]$names[i], dims_ordered[[1]]$units[i]))
            args <- list(x=x, y=y, z=z, ip=ip, xlab=axlab(1), ylab=axlab(2),
                         zlab=zlab, znames_labels=znames, plot_type="active")
            if (is_depth_y) args$ylim <- job_get(job, "ylim", rev(range(unlist(y), finite=T)))
            if (is_month_x) { args$x_at <- 1:12; args$x_labels <- month.abb }
            # image.plot.nxm's default raster needs regular axes: it interpolates irregular ones to the same number of equidistant
            # values, which e.g. squeezes all upper levels of a depth axis into 1 band and extends the NA --> polygons at the real values
            is_irregular <- function(v) {
                d <- as.vector(diff(as.numeric(v)))
                length(d) > 1 && !isTRUE(all.equal(d, rep(d[1], length(d))))
            }
            if (any(sapply(c(x, y), is_irregular))) args$useRaster <- F
            job_n <- job_get(job, "n"); if (!is.null(job_n)) args$n <- job_n
            job_m <- job_get(job, "m"); if (!is.null(job_m)) args$m <- job_m
            do.call(image.plot.nxm, args)
            message("run `my_dev.off(", fout, ") ...")
            my_dev.off(fout)

            # data 2 minus data 1, on the axes of data 1
            if (isTRUE(job_get(job, "anomaly", F))) {
                fout_anom <- sub("(\\.[^./]+)$", "_anom\\1", fout)
                if (ndata != 2) {
                    message("--> `anomaly` = T needs 2 datasets, not ", ndata, " --> skip")
                } else if (!isTRUE(job_get(job, "same_units", F)) && !same_units(datas[[1]]$units, datas[[2]]$units)) {
                    message("--> `anomaly` = T but units differ (\"", datas[[1]]$units, "\" vs \"", datas[[2]]$units,
                            "\"): set `fac` and `same_units` = T --> skip \"", fout_anom, "\"")
                } else if (file.exists(fout_anom) && !force) {
                    message("--> \"", fout_anom, "\" already exists and `force` = F --> skip")
                } else {
                    srt <- function(v) order(as.numeric(v))
                    ox1 <- srt(x[[1]]); oy1 <- srt(y[[1]]); ox2 <- srt(x[[2]]); oy2 <- srt(y[[2]])
                    xa <- as.numeric(x[[1]])[ox1]; ya <- as.numeric(y[[1]])[oy1]
                    x2 <- as.numeric(x[[2]])[ox2]; y2 <- as.numeric(y[[2]])[oy2]; z2 <- z[[2]][ox2, oy2, drop=F]
                    # identical axes: no interpolation, since fields gives NA also for an NA neighbor with weight 0, which would grow the mask
                    if (!isTRUE(all.equal(x2, xa)) || !isTRUE(all.equal(y2, ya))) z2 <- fields::interp.surface.grid(list(x=x2, y=y2, z=z2), list(x=xa, y=ya))$z
                    anom <- z2 - z[[1]][ox1, oy1, drop=F]
                    if (!any(is.finite(anom))) {
                        message("--> no overlap of data 1 and data 2 --> skip \"", fout_anom, "\"")
                    } else {
                        zm <- as.numeric(stats::quantile(abs(anom), 0.99, na.rm=T))
                        if (zm == 0) zm <- max(abs(anom), na.rm=T)
                        if (zm == 0) zm <- 1
                        zlim_a <- job_get(job, "anomaly_zlim", c(-zm, zm))
                        anom[is.finite(anom)] <- pmax(pmin(anom[is.finite(anom)], zlim_a[2]), zlim_a[1]) # colors of the ends for outliers
                        # time axis as given by the 1st dataset
                        xa_plot <- if (inherits(x[[1]], "POSIXct")) x[[1]][ox1] else xa
                        ya_plot <- if (inherits(y[[1]], "POSIXct")) y[[1]][oy1] else ya
                        args_a <- args
                        args_a$x <- list(xa_plot); args_a$y <- list(ya_plot); args_a$z <- list(anom)
                        args_a$ip <- image.plot.pre(zlim=zlim_a, nlevels=nlevels, anom_colorbar=T)
                        args_a$zlab <- fmt_label(job_get(job, "anomaly_zlab", if (nzchar(datas[[1]]$units)) paste0("anomaly [", datas[[1]]$units, "]") else "anomaly"))
                        args_a$znames_labels <- paste0(datas[[2]]$label, " minus ", datas[[1]]$label)
                        if (is_depth_y) args_a$ylim <- rev(range(ya, finite=T))
                        message("--> anomaly plot \"", fout_anom, "\", zlim = [", signif(zlim_a[1], 3), ", ", signif(zlim_a[2], 3), "] ...")
                        open_dev(contour_width/1.3, contour_height, path=fout_anom)
                        do.call(image.plot.nxm, args_a)
                        message("run `my_dev.off(", fout_anom, ") ...")
                        my_dev.off(fout_anom)
                        if (!file.exists(fout_anom)) stop("plot ", ploti, ": `fout_anom` = \"", fout_anom, "\" was not created")
                    }
                }
            }

        } else {
            stop("plot ", ploti, ": unsupported/mismatched data shape(s) for `mode` = \"", mode, "\" -- ", ndata, " datasets with ",
                 paste(ndims, collapse=", "), " remaining dim(s) each (", paste(sapply(datas, function(x) paste(x$dims, collapse="+")), collapse=" | "), ")")
        }

        if (!file.exists(fout)) stop("plot ", ploti, ": `fout` = \"", fout, "\" was not created")
        message("--> done: \"", fout, "\"")

    } # for fout

} # for ploti

message("\n****************** plot.r finish ******************")
