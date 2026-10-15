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

pathpost <- "/work/{ab1095,ba1103,ba1066}/a270073/post"
pathplot <- "/work/ab1095/a270073/plot"

plots <- list()
if (T) {
    plots[[length(plots)+1]] <- list(
        datas=list(
            list(fin="echam/fldmean/co2_burden/co2_burden_mon_fldmean_echam_ssp126_r1i1p1f1_gn_2015-2100.nc", label="C-126", col="#377EB8")
            ,list(fin="echam/fldmean/co2_burden/co2_burden_mon_fldmean_echam_ssp585_and_ssp534-over2_r1i1p1f1_gn_2015-2100.nc", label="C-534", col="#E41A1C")
            ,list(fin="echam/fldmean/co2_burden/co2_burden_mon_fldmean_echam_esm-ssp126_r1i1p1f1_gn_2015-2100.nc", label="E-126", col="#377EB8")
            ,list(fin="echam/fldmean/co2_burden/co2_burden_mon_fldmean_echam_esm-ssp585_and_esm-ssp534-over2_r1i1p1f1_gn_2015-2100.nc", label="E-534", col="#E41A1C")
        ),
        fout=paste0(pathplot, "/echam/fldmean/co2_burden/cu2_burden_mon_fldmean_C-E-126-534_2015-2100.png")
    )
}

if (length(plots) == 0) stop("no plot activated; flip at least one `if (T)`/`if (F)` above to `if (T)`")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")

