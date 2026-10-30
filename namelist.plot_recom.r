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
            list(fin="AWI-ESM-3-REcoM/fldsum/CO2f/CO2f_year_fldsum_AWI-ESM-3-REcoM_recom_AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230_r1i1p1f1_core3_1850-2339.nc", ylab="flux [PgC/yr] (>0: into ocean)")
        ),
        fout=paste0(pathplot, "/AWI-ESM-3-REcoM/fldsum/CO2f/CO2f_year_fldsum_AWI-ESM-3-REcoM_recom_AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230_1850-2339.png")
    )
}

#####################################################################################################

if (length(plots) == 0) stop("no plot activated; flip at least one `if (T)`/`if (F)` above to `if (T)`")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")

