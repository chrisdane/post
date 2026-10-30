# r

# namelist for plot.r: activate blocks with `if (T)`; run with `Rscript namelist.plot.r`

# config (optional; defaults in plot.r)
#dry <- T # only check what would be plotted
#res <- 300
#default_cols <- c("black", "red", "blue")

force <- T # redo existing fout

hostname <- Sys.info()["nodename"]
if (grepl("levante|^l[0-9]{5}", hostname)) { # levante; compute nodes: l12345
    pathpost <- c("/work/ab1095/a270073/post", "/work/ba1103/a270073/post") # search order; derived files go to the 1st
    pathplot <- "/work/ab1095/a270073/plots"
} else if (grepl("albedo|^prod-[0-9]+$", hostname)) { # albedo; login nodes: albedo1/2; compute nodes: prod-123
    pathpost <- "/albedo/work/user/cdanek/post"
    pathplot <- "/albedo/work/user/cdanek/plots"
} else {
    stop("hostname \"", hostname, "\" unknown: set `pathpost` and `pathplot` for it")
}

#####################################################################################################

setup <- "AWI-ESM-1-REcoM"
plots <- list()
if (T) {
    plots[[length(plots)+1]] <- list(
        datas=list(
            list(fin=paste0(setup, "/fldmean/co2_burden/co2_burden_mon_fldmean_", setup, "_echam6_ssp126_r1i1p1f1_gn_2015-2100.nc"), label="C-126", col="#377EB8")
            ,list(fin=paste0(setup, "/fldmean/co2_burden/co2_burden_mon_fldmean_", setup, "_echam6_ssp585_and_ssp534-over2_r1i1p1f1_gn_2015-2100.nc"), label="C-534", col="#E41A1C")
            ,list(fin=paste0(setup, "/fldmean/co2_burden/co2_burden_mon_fldmean_", setup, "_echam6_esm-ssp126_r1i1p1f1_gn_2015-2100.nc"), label="E-126", col="#377EB8")
            ,list(fin=paste0(setup, "/fldmean/co2_burden/co2_burden_mon_fldmean_", setup, "_echam6_esm-ssp585_and_esm-ssp534-over2_r1i1p1f1_gn_2015-2100.nc"), label="E-534", col="#E41A1C")
        ),
        fout=paste0(pathplot, "/", setup, "/fldmean/co2_burden/co2_burden_mon_fldmean_", setup, "_echam6_C-E-126-534_2015-2100.png")
    )
}

#####################################################################################################

if (length(plots) == 0) stop("no plot activated; flip at least one `if (T)`/`if (F)` above to `if (T)`")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")

