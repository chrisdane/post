# r

# Example namelist for plot.r: set up config (optional; plot.r has sensible
# defaults for everything below), activate whichever plots are wanted via
# `if (T)`/`if (F)`, then source plot.r.
# Run with: `Rscript namelist.plot.r`

# ---- config (all optional; shown here just to document the defaults) ----
#dry <- T # default: F; set T to only check/report what would be plotted
#force <- T # default: F; set T to redo even if fout already exists
#res <- 300 # default: 200; passed to png()/pdf()
#default_cols <- c("black", "red", "blue") # default line colors, cycled/indexed by dataset

force <- T # default: F; set T to redo even if fout already exists

hostname <- Sys.info()["nodename"]
if (grepl("levante|^l[0-9]{5}", hostname)) { # levante; compute nodes: l12345
    pathpost <- c("/work/ab1095/a270073/post", "/work/ba1103/a270073/post") # searched in this order; new derived files go to the 1st
    pathplot <- "/work/ab1095/a270073/plots"
} else if (grepl("albedo|^prod-[0-9]+$", hostname)) { # albedo; login nodes: albedo1/2; compute nodes: prod-123
    pathpost <- "/albedo/work/user/cdanek/post"
    pathplot <- "/albedo/work/user/cdanek/plots"
} else {
    stop("hostname \"", hostname, "\" unknown: set `pathpost` and `pathplot` for it")
}

#####################################################################################################

plots <- list()
if (T) {
    plots[[length(plots)+1]] <- list(
        datas=list(
            list(fin="AWI-ESM-1-REcoM/fldmean/co2_burden/co2_burden_mon_fldmean_AWI-ESM-1-REcoM_echam6_ssp126_r1i1p1f1_gn_2015-2100.nc", label="C-126", col="#377EB8")
            ,list(fin="AWI-ESM-1-REcoM/fldmean/co2_burden/co2_burden_mon_fldmean_AWI-ESM-1-REcoM_echam6_ssp585_and_ssp534-over2_r1i1p1f1_gn_2015-2100.nc", label="C-534", col="#E41A1C")
            ,list(fin="AWI-ESM-1-REcoM/fldmean/co2_burden/co2_burden_mon_fldmean_AWI-ESM-1-REcoM_echam6_esm-ssp126_r1i1p1f1_gn_2015-2100.nc", label="E-126", col="#377EB8")
            ,list(fin="AWI-ESM-1-REcoM/fldmean/co2_burden/co2_burden_mon_fldmean_AWI-ESM-1-REcoM_echam6_esm-ssp585_and_esm-ssp534-over2_r1i1p1f1_gn_2015-2100.nc", label="E-534", col="#E41A1C")
        ),
        fout=paste0(pathplot, "/AWI-ESM-1-REcoM/fldmean/co2_burden/co2_burden_mon_fldmean_AWI-ESM-1-REcoM_echam6_C-E-126-534_2015-2100.png")
    )
}

#####################################################################################################

if (length(plots) == 0) stop("no plot activated; flip at least one `if (T)`/`if (F)` above to `if (T)`")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")

