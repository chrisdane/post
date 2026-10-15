# r

# Land + ocean carbon budget from lpj_guess and recom, without the atmosphere.
# Needs the derived series of namelist.plot_lpg_carbon.r (dcLandDt_yearly) and namelist.plot_recom_carbon.r (dcOceanDt): run these first.
# Run with: `Rscript namelist.plot_lpj_recom_carbon.r`

force <- F # default: F; set T to redo even if fout already exists

hostname <- Sys.info()["nodename"]
if (grepl("levante|^l[0-9]{5}", hostname)) { # levante; compute nodes: l12345
    pathpost <- c("/work/ab1095/a270073/post", "/work/ba1103/a270073/post") # searched in this order; new derived files go to the 1st
    pathplot <- "/work/ab1095/a270073/plots"
} else {
    stop("hostname \"", hostname, "\" unknown: set `pathpost` and `pathplot` for it")
}
# post file `f` relative to `pathpost`: its 1st existing copy, else where it is made (under the 1st `pathpost`)
ppost <- function(f) unname(sapply(f, function(x) { hits <- file.path(pathpost, x); hits <- hits[file.exists(hits)]; if (length(hits) > 0) hits[1] else file.path(pathpost[1], x) }))

experiments <- list(
    list(experiment="pi_noxios", from=1850, to=1858)
    #,list(experiment="pi_noxios_zstar", from=1850, to=1858)
)

fin_lpj <- function(v, exp) paste0("lpj_guess/fldint/", v, "/", v, "_year_fldint_lpj_guess_", exp$experiment, "_r1i1p1f1_r360x180_", exp$from, "-", exp$to, ".nc")
fin_recom <- function(v, exp) paste0("recom/fldsum/", v, "/", v, "_year_fldsum_recom_", exp$experiment, "_r1i1p1f1_core3_", exp$from, "-", exp$to, ".nc")
fin_both <- function(v, exp) paste0("lpj_recom/", v, "/", v, "_year_lpj_recom_", exp$experiment, "_", exp$from, "-", exp$to, ".nc")

comp_cols <- c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey50", "red")
plots <- list()

if (T) { # per experiment: d(cLand + cOcean)/dt = -NEE + CO2f + residual, residual ~ -wetland CH4 (PgC/yr, >0: into land/ocean; 1st year dropped)
    # land: dec 31 states and calendar-year NEE; ocean: annual means and the mean CO2f of the 2 years (see namelist.plot_recom_carbon.r)
    for (exp in experiments) {
        need <- ppost(c(fin_lpj("dcLandDt_yearly", exp), fin_recom("dcOceanDt", exp)))
        if (!all(file.exists(need))) stop("missing ", paste(need[!file.exists(need)], collapse=", "), "; run namelist.plot_lpg_carbon.r and namelist.plot_recom_carbon.r first")
        cmds <- list(dcLandOceanDt=paste0("-add ", need[1], " ", need[2]),
                     cLandOceanBudgetResidual=paste0("-sub -add ", ppost(fin_both("dcLandOceanDt", exp)), " -seltimestep,2/-1 ", ppost(fin_lpj("cflux_NEE", exp)),
                                                     " -runmean,2 ", ppost(fin_recom("CO2f", exp))))
        for (v in names(cmds)) { # in this order: the residual needs the sum
            f <- ppost(fin_both(v, exp))
            if (force || !file.exists(f)) {
                dir.create(dirname(f), recursive=T, showWarnings=F)
                cmd <- paste0("cdo -O -s -setname,", v, " ", cmds[[v]], " ", f)
                message("run `", cmd, "` ...")
                if (system(cmd) != 0) stop("cdo failed")
            }
        }
        plots[[length(plots)+1]] <- list(
            datas=list(
                list(fin=ppost(fin_both("dcLandOceanDt", exp)), label="d(cLand + cOcean)/dt", col="black", lwd=2),
                list(fin=ppost(fin_lpj("dcLandDt_yearly", exp)), label="d(cLand_yearly)/dt", col=comp_cols[4]),
                list(fin=ppost(fin_recom("dcOceanDt", exp)), label="d(cOcean)/dt", col=comp_cols[3]),
                list(fin=ppost(fin_lpj("cflux_NEE", exp)), fac=-1, label="-NEE (lpj)", col=comp_cols[4], lty=2),
                list(fin=ppost(fin_recom("CO2f", exp)), label="CO2f (recom)", col=comp_cols[3], lty=2),
                list(fin=ppost(fin_both("cLandOceanBudgetResidual", exp)), label="residual", col="red", lwd=2),
                list(fin=ppost(fin_lpj("wetlandCH4_monthly", exp)), fac=-1, label="-wetland CH4 (as C)", col=comp_cols[8], lty=2)
            ),
            ylab="flux [PgC/yr] (>0: into land/ocean)",
            #fout=paste0(pathplot, "/lpj_recom/cbudget/cland_cocean_budget_lpj_recom_", exp$experiment, "_", exp$from, "-", exp$to, ".", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_recom/cbudget/cland_cocean_budget_lpj_recom_", exp$experiment, "_", exp$from, "-", exp$to, ".", c("png"))
        )
    }
}

if (length(plots) == 0) stop("no plot activated; flip at least one `if (T)`/`if (F)` above to `if (T)`")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")

