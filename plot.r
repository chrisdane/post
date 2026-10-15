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
#   fout    - output file(s), ".png" and/or ".pdf"
#   pathout - [optional] output dir, default: dirname(fout)
#   mode    - [optional] "auto" (default) or "pairwise"
#   datas   - list of 1 or more datasets to plot together, each a list with:
#     fin      - netcdf file; a bare file name is searched in `pathpost`
#     varname  - [optional] default: 1st variable of `fin`
#     label    - [optional] default: varname
#     index    - [optional] list(dimname=index) to slice away dims; size-1 dims are dropped anyway
#     fac      - [optional] factor the data is multiplied with, e.g. -1 to flip the sign; default: 1
#     col, lty, lwd - [optional] line plots only
#   width, height, res - [optional] png px and ppi (pdf: width/res, height/res inch); default: plot_sizes()
#   ylab    - [optional] line plots only; default: label and units of the 1st dataset
#   legend_pos - [optional] line plots only; "my_maxempty" (default; "topleft" if it fails),
#             a `legend()` keyword or c(x, y)
#   legend_ncol, legend_cex - [optional] default: 2 and 0.8 if more than 5 datasets, else 1 and 1
#   legend_x_intersp - [optional] space between line and text in character widths; default: 0.5
#   zlim, nlevels, n, m - [optional] contour plots only; passed to image.plot.pre()/image.plot.nxm()

message("\n****************** plot.r start ******************")

message("\nload ncdf4 ...")
library(ncdf4)
message("source myfunctions.r, image.plot.pre.r, image.plot.nxm.r ...")
source("~/scripts/r/functions/myfunctions.r")
source("~/scripts/r/functions/image.plot.pre.r")
source("~/scripts/r/functions/image.plot.nxm.r")

# config; may be set in the namelist
user_set <- function(varname) exists(varname, where=globalenv(), inherits=F)

# not `job$x`: partial matching would return e.g. job$mode for job$m
job_get <- function(job, field, default=NULL) if (field %in% names(job)) job[[field]] else default
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

# read 1 dataset
read_data <- function(d, ploti, datai) {
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
    arr <- ncdf4::ncvar_get(nc, varname, collapse_degen=F)
    units_att <- suppressWarnings(ncdf4::ncatt_get(nc, varname, "units"))
    units <- if (units_att$hasatt) units_att$value else ""
    dimvals <- lapply(vardims, function(nm) nc$dim[[nm]]$vals)
    names(dimvals) <- vardims
    dimunits <- vapply(vardims, function(nm) if (is.null(nc$dim[[nm]]$units)) "" else nc$dim[[nm]]$units, character(1))
    ncdf4::nc_close(nc)

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
    list(data=arr, dims=vardims, dimvals=dimvals[vardims], dimunits=dimunits[vardims], varname=varname, units=units, label=label)
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

resolve_fout <- function(job, ploti) {
    fout <- job_get(job, "fout")
    if (is.null(fout)) stop("plot ", ploti, " has no `fout`")
    if (!is.character(fout) || length(fout) == 0) stop("plot ", ploti, ": `fout` must be a non-empty character vector")
    abs <- grepl("^/", fout)
    pathout <- job_get(job, "pathout", dirname(fout))
    ifelse(abs, fout, paste0(pathout, "/", basename(fout)))
} # resolve_fout

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
    fouts <- resolve_fout(job, ploti)
    message("--> fout = ", paste0("\"", fouts, "\"", collapse=", "))
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
        if (length(fouts) == 0) next
    }

    datas <- vector("list", length(job_datas))
    for (datai in seq_along(job_datas)) datas[[datai]] <- read_data(job_datas[[datai]], ploti, datai)
    ndata <- length(datas)
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
        open_dev <- function(width, height) {
            if (ext == "png") {
                grDevices::png(fout, width=width, height=height, res=res, family=family_png)
            } else if (ext == "pdf") {
                grDevices::cairo_pdf(fout, width=width/res, height=height/res, family=family_pdf)
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
            } else {
                xvals_list <- lapply(datas, function(dm) maybe_time_axis(dm$dims[1], dm$dimvals[[1]], dm$dimunits[1]))
                is_time <- inherits(xvals_list[[1]], "POSIXct")
                yr <- range(sapply(datas, function(x) range(x$data, finite=T, na.rm=T)))
                xr <- range(if (is_time) do.call(c, xvals_list) else unlist(xvals_list), finite=T)
                set_ymar(yr)
                graphics::plot(0, type="n", xlim=xr, ylim=yr,
                     xlab="", ylab="", xaxt="n", yaxt="n")
                if (is_time) {
                    axis.POSIXct(1, at=usr_at(1, time=T), format="%Y")
                    xtitle("year")
                } else {
                    axis(1, at=usr_at(1))
                    xtitle(axis_label(datas[[1]]$dims[1], datas[[1]]$dimunits[1]))
                }
                yaxis(usr_at(2), job_get(job, "ylab", data_label(datas[[1]])))
                abline(h=0, col="gray", lwd=0.5)
                for (datai in seq_len(ndata)) {
                    dj <- job_datas[[datai]]
                    col <- job_get(dj, "col", default_cols[(datai - 1) %% length(default_cols) + 1])
                    lty <- job_get(dj, "lty", 1)
                    lwd <- job_get(dj, "lwd", 1)
                    lines(xvals_list[[datai]], as.vector(datas[[datai]]$data), col=col, lty=lty, lwd=lwd)
                }
                if (ndata > 1) {
                    pos <- job_get(job, "legend_pos", "my_maxempty")
                    if (identical(pos, "my_maxempty")) {
                        pos <- tryCatch(my_maxempty(lapply(xvals_list, as.numeric), lapply(datas, function(dm) as.vector(dm$data))),
                                        error=function(e) {
                                            message("`my_maxempty()` failed (", conditionMessage(e), ") --> legend at \"topleft\"")
                                            "topleft"
                                        })
                    }
                    if (is.numeric(pos)) pos <- list(x=pos[1], y=pos[2])
                    lg_args <- list(legend=sapply(datas, "[[", "label"),
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
            }
            message("run `my_dev.off(", fout, ") ...")
            my_dev.off(fout)

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
