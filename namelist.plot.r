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
for (dt in c("year", "mon")) {
    if (dt == "year" && T || dt == "mon" && F) {
        plots[[length(plots)+1]] <- list(
            datas=list(
                list(fin=paste0(pathpost[1], "/", setup, "/select/aCO2/aCO2_", dt, "_select_", setup, "_recom_esm-hist_r1i1p1f1_gn_1850-2014.nc"), label="recom aCO2 [uatm]")
                ,list(fin=paste0(pathpost[1], "/", setup, "/fldint/co2_burden/co2_burden_", dt, "_fldint_", setup, "_echam6_esm-hist_r1i1p1f1_gn_1850-2014.nc"), label="echam6 co2_burden [ppm]")
            ),
            ylab="atmospheric CO2",
            fout=paste0(pathplot, "/", setup, "/aCO2_vs_co2_burden_", dt, "_", setup, "_esm-hist_r1i1p1f1_1850-2014.png")
        )
    }
}

#####################################################################################################

if (length(plots) == 0) stop("no plot activated; flip at least one `if (T)`/`if (F)` above to `if (T)`")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")
