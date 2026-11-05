# r

# Plots of namelist.post_data_fesom_recom_clim.r's output, on its own (no FESOM/REcoM comparison -- see
# namelist.plot_data_fesom_recom_clim.r for that): maps (`timmean`, 1 per depth for a 3D variable --
# plot.r draws 1 or 2 dims, so a 3rd (depth) is sliced via `index`), zonal means (line plot vs latitude
# for 2D variables; lat-depth section for 3D ones) and vertical profiles (`mode` = "fldmean", 3D
# variables only; added to plot.r for this).
#
# Fully data-driven: every dataset/variable/mode combination that actually exists under `pathpost` is
# discovered and plotted (new datasets/variables added to the post namelist need no change here). Plain
# `ymonmean` (full lon-lat(-depth) fields, 1 per calendar month) is not plotted (would need the same
# per-depth and, for a 3D variable, also per-month slicing as `timmean` -- deferred, not asked for);
# `zonmean_ymon` of a 3D variable (3 remaining dims: lat, depth, month) is skipped too (plot.r draws 1 or
# 2 dims).

force <- F
library(ncdf4)

hostname <- Sys.info()["nodename"]
if (grepl("levante|^l[0-9]{5}", hostname)) {
    pathpost <- c("/work/ab1095/a270073/post", "/work/ba1103/a270073/post")
    pathplot <- "/work/ab1095/a270073/plots"
} else if (grepl("albedo|^prod-[0-9]+$", hostname)) {
    pathpost <- "/albedo/work/user/cdanek/post"
    pathplot <- "/albedo/work/user/cdanek/plots"
} else {
    stop("hostname \"", hostname, "\" unknown: set `pathpost` and `pathplot` for it")
}

datasets <- c("buitenhuis_etal_2013", "de_boyer_montegut_etal_2023", "glodap", "gregor_and_fay_2021",
              "huang_etal_2022", "keppler_etal_2020", "lewis_etal_2022", "nsidc", "cmems", "steele_etal_2001", "woa")
modes_want <- c("timmean", "zonmean", "zonmean_ymon", "fldmean", "fldmean_ymon")
known_depth_dimnames <- c("depth", "Depth", "DEPTH", "depth_surface", "nz", "lev", "level", "Level", "LEVEL") # as in plot.r

# `f`'s 1st (real) variable's dims (name, length, values) -- plot.r draws 1 (line/profile) or 2 (contour)
# remaining (length > 1) dims; anything else (e.g. a 3D variable's zonmean_ymon: lat+depth+month, or its
# timmean: lat+lon+depth) needs `index`-slicing (done below for timmean) or is skipped (for zonmean_ymon)
# rather than left for plot.r to error out on (it has no per-plot error isolation, unlike post.r)
nc_var_dims <- function(f) {
    varname <- trimws(strsplit(trimws(system(paste0("cdo -s showname ", f), intern=T)), "[[:space:]]+")[[1]][1])
    nc <- nc_open(f)
    on.exit(nc_close(nc))
    if (is.null(nc$var[[varname]])) stop("nc_var_dims(\"", f, "\"): variable \"", varname, "\" not found")
    dims <- nc$var[[varname]]$dim
    list(varname=varname, names=sapply(dims, "[[", "name"), lens=sapply(dims, "[[", "len"),
         vals=lapply(dims, "[[", "vals"), units=sapply(dims, function(d) if (is.null(d$units)) "" else d$units))
}

#####################################################################################################

plots <- list()
for (dataset in datasets) {
    for (mode_name in modes_want) {
        moddir <- file.path(pathpost, dataset, mode_name)
        if (!dir.exists(moddir)) next
        for (vardir in list.dirs(moddir, recursive=F)) {
            varname <- basename(vardir)
            for (f in list.files(vardir, pattern="\\.nc$", full.names=T)) {
                dd <- nc_var_dims(f)
                survive <- dd$lens > 1
                ndims <- sum(survive)
                label <- tools::file_path_sans_ext(basename(f))

                if (mode_name == "timmean" && ndims == 3) { # a 3D variable's map: lon+lat+depth -- 1 map per depth
                    depth_i <- which(survive & !is.na(match(dd$names, known_depth_dimnames)))
                    if (length(depth_i) != 1) {
                        message("skip \"", f, "\" (3 remaining dims, but none/more than 1 recognized as depth: ",
                                paste(dd$names[survive], collapse=", "), ")")
                        next
                    }
                    depth_vals <- dd$vals[[depth_i]]
                    nc <- nc_open(f)
                    for (k in seq_along(depth_vals)) {
                        start <- rep(1, length(dd$names)); count <- dd$lens; start[depth_i] <- k; count[depth_i] <- 1
                        slice <- ncvar_get(nc, dd$varname, start=start, count=count)
                        if (all(is.na(slice))) {
                            message("skip \"", f, "\" (", dd$names[depth_i], "=", depth_vals[k], ": all-NA slice)")
                            next
                        }
                        fout <- file.path(pathplot, dataset, mode_name, varname, paste0(label, "_", dd$names[depth_i], depth_vals[k], ".png"))
                        job <- list(datas=list(list(fin=f, label=varname, index=setNames(list(k), dd$names[depth_i]))), fout=fout)
                        plots[[length(plots)+1]] <- job
                    }
                    nc_close(nc)
                    next
                }

                if (!(ndims %in% 1:2)) {
                    message("skip \"", f, "\" (", ndims, " remaining dim(s), plot.r draws 1 or 2)")
                    next
                }
                fout <- file.path(pathplot, dataset, mode_name, varname, paste0(label, ".png"))
                job <- list(datas=list(list(fin=f, label=varname)), fout=fout)
                # `mode` = "fldmean" is only for the 1-dim (pure depth profile) case; "fldmean_ymon"'s
                # 2-dim depth+month case goes through the regular (depth-aware) "auto" contour path
                if (mode_name == "fldmean" && ndims == 1) job$mode <- "fldmean"
                plots[[length(plots)+1]] <- job
            }
        }
    }
}

message("--> ", length(plots), " plot(s) discovered under ", length(datasets), " dataset(s)")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")
