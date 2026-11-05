# r

# Side-by-side comparison of namelist.post_data_fesom_recom_clim.r's (obs) and
# namelist.post_fesom_recom_clim.r's (FESOM/REcoM) output, per the variable pairing established
# throughout both of those namelists' own comments (`pairs` below). For standalone (no-comparison) plots
# of just the obs side, see namelist.data_clim.r.
#
# - maps (`timmean`): obs on the left panel, FESOM/REcoM on the right, remapped (`cdo remapcon`) onto the
#   obs file's own grid -- FESOM's native output is on its own unstructured mesh, not directly
#   comparable/plottable the way a regular lon-lat grid is. 1 map per depth in `depths_want` for a 3D
#   variable (nearest-matched independently on each side, as elsewhere in this project), skipping an
#   all-missing depth on the obs side.
# - 1-dim reductions (`zonmean` of a 2D variable; `fldmean` of a 3D one, `mode` = "fldmean"): obs+model
#   overlaid on 1 line/profile plot.
# - 2-dim reductions (`zonmean` of a 3D variable: lat-depth section; `zonmean_ymon` of a 2D one:
#   lat-month; `fldmean_ymon` of a 3D one: depth-month): obs left panel, model right panel -- no remap
#   needed here, both sides are already reduced to the same kind of axes (lat/depth/month bins).
# - `zonmean_ymon` of a 3D variable (3 remaining dims: lat, depth, month) is skipped (plot.r draws 1 or 2).
#
# Datasets/variables with no established FESOM/REcoM counterpart are left out of `pairs` entirely (not
# silently skipped at plot time): nsidc (scalar hemispheric time series, not comparable to a_ice's
# spatial field without first reducing it the same way -- "left for later", see
# namelist.post_fesom_recom_clim.r); cmems's DIATO/MICRO/NANO/PICO and glodap's
# Cant/OmegaA/OmegaC/PI_TCO2/PO4/pHts25p0/pHtsinsitutp (no REcoM counterpart tracked here).

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

cdo <- "/albedo/soft/sw/spack-sw/cdo/2.6.0-7vr4w3i/bin/cdo"
fesom_mesh_griddes <- "/albedo/work/user/cdanek/mesh/fesom2/core3/griddes_core3_node.nc"
remap_dir <- file.path(pathpost, "AWI-ESM-3-REcoM_remapped") # cache dir for maps remapped onto an obs grid

# as in namelist.post_fesom_recom_clim.r
setup <- "AWI-ESM-3-REcoM"
experiment <- "AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230"
from <- 2510
to <- 2609
depths_want <- c(0, 50, 100, 200, 500, 1000) # m; same convention used throughout this project

known_depth_dimnames <- c("depth", "Depth", "DEPTH", "depth_surface", "nz", "lev", "level", "Level", "LEVEL") # as in plot.r

# 3D vars' zonmean/fldmean/timmean filenames embed the `level` string, which differs per variable and
# isn't reproduced here -- `*` in its place (2D vars have no such segment at all, which "*" also matches)
fin_fesom <- function(var, mode_name) {
    pattern <- file.path(pathpost, setup, mode_name, var,
                          paste0(var, "_mon_", mode_name, "_*", setup, "_recom_", experiment, "_r1i1p1f1_gn_", from, "-", to, ".nc"))
    hits <- Sys.glob(pattern)
    if (length(hits) != 1) {
        message("fin_fesom(\"", var, "\", \"", mode_name, "\"): expected 1 match for \"", pattern, "\", got ", length(hits))
        return(NULL)
    }
    hits
}

# `f`'s 1st (real) variable's dims (name, length, values)
nc_var_dims <- function(f) {
    varname <- trimws(strsplit(trimws(system(paste0(cdo, " -s showname ", f), intern=T)), "[[:space:]]+")[[1]][1])
    nc <- nc_open(f)
    on.exit(nc_close(nc))
    if (is.null(nc$var[[varname]])) stop("nc_var_dims(\"", f, "\"): variable \"", varname, "\" not found")
    dims <- nc$var[[varname]]$dim
    list(varname=varname, names=sapply(dims, "[[", "name"), lens=sapply(dims, "[[", "len"), vals=lapply(dims, "[[", "vals"))
}
nearest_idx <- function(vals, want) which.min(abs(vals - want))

# remap FESOM/REcoM's `var` timmean map onto `target_f`'s own grid -- at the depth nearest `depth_want`
# (3D variable; `target_f`'s own nearest depth is picked by the caller) or the whole field (2D variable,
# `depth_want` = NULL). cdo accepts a plain data file in place of a griddes to define the target grid.
# Cached under `remap_dir` by variable + depth + target file (so re-used across a target with >1 own file,
# e.g. lewis_etal_2022's 2 sensors).
remap_fesom_map <- function(var, target_f, depth_want=NULL) {
    fesom_f <- fin_fesom(var, "timmean")
    if (is.null(fesom_f)) return(NULL)
    dd <- nc_var_dims(fesom_f)
    survive <- dd$lens > 1
    depth_i <- which(survive & !is.na(match(dd$names, known_depth_dimnames)))
    if (length(depth_i) == 1) {
        if (is.null(depth_want)) stop("remap_fesom_map(\"", var, "\"): FESOM file has a depth dim but no `depth_want` given")
        k <- nearest_idx(dd$vals[[depth_i]], depth_want)
        sellev <- paste0("-sellevidx,", k)
        tag <- paste0(dd$names[depth_i], round(dd$vals[[depth_i]][k], 1))
    } else {
        sellev <- ""
        tag <- "2d"
    }
    outdir <- file.path(remap_dir, "timmean", var)
    dir.create(outdir, recursive=T, showWarnings=F)
    fout <- file.path(outdir, paste0(var, "_timmean_remap_", tag, "_onto_", tools::file_path_sans_ext(basename(target_f)), ".nc"))
    if (!file.exists(fout) || force) {
        cmd <- paste0(cdo, " -O -s remapcon,", target_f, " ", sellev, " -setgrid,", fesom_mesh_griddes, " ", fesom_f, " ", fout)
        message("run `", cmd, "` ...")
        if (system(cmd) != 0) {
            message("--> remap failed -- skip this map")
            return(NULL)
        }
    }
    fout
}

#####################################################################################################

pairs <- list(
    # buitenhuis_etal_2013 (PFT biomass) vs REcoM PFT carbon; Phaeocystis has no REcoM PFT counterpart
    list(dataset="buitenhuis_etal_2013", varname="BIOMASS", file_match="Diatoms",          fesom_var="DiaC"),
    list(dataset="buitenhuis_etal_2013", varname="BIOMASS", file_match="Coccolithophores", fesom_var="PhyCalc"),
    list(dataset="buitenhuis_etal_2013", varname="BIOMASS", file_match="Microzooplankton", fesom_var="HetC"),
    list(dataset="buitenhuis_etal_2013", varname="BIOMASS", file_match="Mesozooplankton",  fesom_var="Zoo2C"),
    list(dataset="buitenhuis_etal_2013", varname="BIOMASS", file_match="Macrozooplankton", fesom_var="Zoo3C"),
    list(dataset="de_boyer_montegut_etal_2023", varname="mld_dr003",   fesom_var="MLD3"),
    list(dataset="glodap", varname="TCO2",        fesom_var="DIC"),
    list(dataset="glodap", varname="TAlk",        fesom_var="Alk"),
    list(dataset="glodap", varname="NO3",         fesom_var="DIN"),
    list(dataset="glodap", varname="silicate",    fesom_var="DSi"),
    list(dataset="glodap", varname="oxygen",      fesom_var="O2"),
    list(dataset="glodap", varname="temperature", fesom_var="temp"),
    list(dataset="glodap", varname="salinity",    fesom_var="salt"),
    list(dataset="gregor_and_fay_2021", varname="fgco2", fesom_var="CO2f"),
    list(dataset="gregor_and_fay_2021", varname="pCO2s", fesom_var="pCO2s"),
    list(dataset="huang_etal_2022",     varname="dFe",   fesom_var="DFe"),
    list(dataset="keppler_etal_2020",   varname="DIC",   fesom_var="DIC"),
    list(dataset="lewis_etal_2022", varname="chlor_a", file_match="^A_", fesom_var="Chl"),
    list(dataset="lewis_etal_2022", varname="chlor_a", file_match="^S_", fesom_var="Chl"),
    list(dataset="cmems", varname="ARC_009_124_CHL", fesom_var="Chl"),
    list(dataset="cmems", varname="GLO_009_104_CHL", fesom_var="Chl"),
    list(dataset="cmems", varname="GLO_009_108_CHL", fesom_var="Chl"),
    list(dataset="cmems", varname="GLO_009_104_PP",  fesom_var="NPP"),
    list(dataset="steele_etal_2001", varname="temp", fesom_var="temp"),
    list(dataset="steele_etal_2001", varname="salt", fesom_var="salt"),
    list(dataset="woa", varname="temp", fesom_var="temp"),
    list(dataset="woa", varname="salt", fesom_var="salt"),
    list(dataset="woa", varname="DIN",  fesom_var="DIN"),
    list(dataset="woa", varname="DSi",  fesom_var="DSi"),
    list(dataset="woa", varname="O2",   fesom_var="O2")
)

plots <- list()
for (p in pairs) {

    # --- maps (timmean): obs left, FESOM/REcoM (remapped onto the obs grid) right ---
    ddir <- file.path(pathpost, p$dataset, "timmean", p$varname)
    if (dir.exists(ddir)) {
        dfiles <- list.files(ddir, pattern="\\.nc$", full.names=T)
        if (!is.null(p$file_match)) dfiles <- dfiles[grepl(p$file_match, basename(dfiles))]
        for (f in dfiles) {
            dd <- nc_var_dims(f)
            survive <- dd$lens > 1
            ndims <- sum(survive)
            label <- tools::file_path_sans_ext(basename(f))

            if (ndims == 2) {
                model_f <- remap_fesom_map(p$fesom_var, f)
                if (!is.null(model_f)) {
                    fout <- file.path(pathplot, "comparison", p$dataset, "timmean", p$varname, paste0(label, "_vs_", p$fesom_var, ".png"))
                    plots[[length(plots)+1]] <- list(datas=list(list(fin=f, label=paste(p$dataset, p$varname)),
                                                                 list(fin=model_f, label=p$fesom_var)), fout=fout)
                }
            } else if (ndims == 3) {
                depth_i <- which(survive & !is.na(match(dd$names, known_depth_dimnames)))
                if (length(depth_i) != 1) {
                    message("skip \"", f, "\" (3 remaining dims, but none/more than 1 recognized as depth)")
                } else {
                    nc <- nc_open(f)
                    for (dw in depths_want) {
                        k <- nearest_idx(dd$vals[[depth_i]], dw)
                        start <- rep(1, length(dd$names)); count <- dd$lens; start[depth_i] <- k; count[depth_i] <- 1
                        slice <- ncvar_get(nc, dd$varname, start=start, count=count)
                        if (all(is.na(slice))) {
                            message("skip \"", f, "\" (", dd$names[depth_i], "=", dd$vals[[depth_i]][k], ": all-NA slice)")
                            next
                        }
                        model_f <- remap_fesom_map(p$fesom_var, f, depth_want=dw)
                        if (is.null(model_f)) next
                        dtag <- paste0(dd$names[depth_i], round(dd$vals[[depth_i]][k]))
                        fout <- file.path(pathplot, "comparison", p$dataset, "timmean", p$varname, paste0(label, "_", dtag, "_vs_", p$fesom_var, ".png"))
                        plots[[length(plots)+1]] <- list(datas=list(
                            list(fin=f, label=paste(p$dataset, p$varname), index=setNames(list(k), dd$names[depth_i])),
                            list(fin=model_f, label=p$fesom_var)), fout=fout)
                    }
                    nc_close(nc)
                }
            }
        }
    }

    # --- zonmean/fldmean family: 1-dim overlaid, 2-dim side-by-side (obs left, model right) ---
    for (mode_name in c("zonmean", "zonmean_ymon", "fldmean", "fldmean_ymon")) {
        ddir2 <- file.path(pathpost, p$dataset, mode_name, p$varname)
        if (!dir.exists(ddir2)) next
        model_f <- fin_fesom(p$fesom_var, mode_name)
        if (is.null(model_f)) next
        dfiles2 <- list.files(ddir2, pattern="\\.nc$", full.names=T)
        if (!is.null(p$file_match)) dfiles2 <- dfiles2[grepl(p$file_match, basename(dfiles2))]
        for (f in dfiles2) {
            ndims <- sum(nc_var_dims(f)$lens > 1)
            if (!(ndims %in% 1:2)) {
                message("skip \"", f, "\" (", ndims, " remaining dim(s), plot.r draws 1 or 2)")
                next
            }
            label <- tools::file_path_sans_ext(basename(f))
            fout <- file.path(pathplot, "comparison", p$dataset, mode_name, p$varname, paste0(label, "_vs_", p$fesom_var, ".png"))
            job <- list(datas=list(list(fin=f, label=paste(p$dataset, p$varname)), list(fin=model_f, label=p$fesom_var)), fout=fout)
            if (mode_name == "fldmean" && ndims == 1) job$mode <- "fldmean"
            plots[[length(plots)+1]] <- job
        }
    }
}

message("--> ", length(plots), " comparison plot(s) built from ", length(pairs), " pairing(s)")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")
