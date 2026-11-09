# r

# Plots of namelist.post_fesom_recom_clim.r's output: zonal means (line plot vs latitude for 2D
# variables; lat-depth section for 3D ones) and vertical profiles (`mode` = "fldmean", 3D variables only;
# added to plot.r for this). Comparison with namelist.post_data_oce.r's obs output is a
# separate, later namelist (not done here) -- this is a sanity-check of the FESOM/REcoM side on its own.
#
# `timmean`/`ymonmean` (full lon-lat(-depth) fields on FESOM's unstructured mesh) and `zonmean_ymon` of 3D
# variables (3 remaining dims: lat, depth, month -- plot.r only draws 1 or 2) are not plotted here.

force <- F

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

#####################################################################################################

# as in namelist.post_fesom_recom_clim.r
setup <- "AWI-ESM-3-REcoM"
experiment <- "AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230"
from <- 2510
to <- 2609

# vars to plot: Chl/NPP (the derived sums) instead of their DiaChl+PhyChl/NPPd+NPPn components
vars <- list(
    list(v="DIC",     is3d=T),
    list(v="Alk",     is3d=T),
    list(v="DFe",     is3d=T),
    list(v="DiaC",    is3d=T),
    list(v="PhyCalc", is3d=T),
    list(v="HetC",    is3d=T),
    list(v="Zoo2C",   is3d=T),
    list(v="Zoo3C",   is3d=T),
    list(v="Chl",     is3d=T),
    list(v="temp",    is3d=T),
    list(v="salt",    is3d=T),
    list(v="DIN",     is3d=T),
    list(v="DSi",     is3d=T),
    list(v="O2",      is3d=T),
    list(v="MLD3",    is3d=F),
    list(v="CO2f",    is3d=F),
    list(v="pCO2s",   is3d=F),
    list(v="a_ice",   is3d=F),
    list(v="NPP",     is3d=F)
)

# 3D vars' zonmean/fldmean filenames embed the (nearest-depth / "all depths") `level` string, which
# differs per variable and isn't reproduced here -- `*` in its place (2D vars have no such segment at
# all, which "*" also matches, zero-length)
fin_fesom <- function(var, mode_name) {
    pattern <- file.path(pathpost, setup, mode_name, var,
                          paste0(var, "_mon_", mode_name, "_*", setup, "_recom_", experiment, "_r1i1p1f1_gn_", from, "-", to, ".nc"))
    hits <- Sys.glob(pattern)
    if (length(hits) != 1) stop("fin_fesom(\"", var, "\", \"", mode_name, "\"): expected 1 match for \"", pattern, "\", got ",
                                 length(hits), if (length(hits) > 0) paste0(": ", paste(hits, collapse=", ")) else "")
    hits
}
fout_fesom <- function(var, mode_name) {
    paste0(pathplot, "/", setup, "/", mode_name, "/", var, "/", var, "_", mode_name, "_", setup, "_recom_", experiment, "_", from, "-", to, ".png")
}

plots <- list()
for (x in vars) {
    v <- x$v
    # zonmean: line vs latitude (2D) or lat-depth section (3D, depth on inverted y -- plot.r's `known_depth_dimnames`)
    plots[[length(plots)+1]] <- list(datas=list(list(fin=fin_fesom(v, "zonmean"), label=v)), fout=fout_fesom(v, "zonmean"))
    if (!x$is3d) { # zonmean_ymon of a 2D var: lat-month section; of a 3D var it's 3-dim (lat+depth+month) -- not plotted
        plots[[length(plots)+1]] <- list(datas=list(list(fin=fin_fesom(v, "zonmean_ymon"), label=v)), fout=fout_fesom(v, "zonmean_ymon"))
    }
    if (x$is3d) { # vertical profile (`mode` = "fldmean") and its seasonal evolution (depth-month section)
        plots[[length(plots)+1]] <- list(mode="fldmean", datas=list(list(fin=fin_fesom(v, "fldmean"), label=v)), fout=fout_fesom(v, "fldmean"))
        plots[[length(plots)+1]] <- list(datas=list(list(fin=fin_fesom(v, "fldmean_ymon"), label=v)), fout=fout_fesom(v, "fldmean_ymon"))
    }
}

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")
