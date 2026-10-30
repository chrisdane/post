# r

# Plots of the REcoM carbon time series made by namelist.post_recom_carbon.r.
# Run with: `Rscript namelist.plot_recom_carbon.r`

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

# experiments as in namelist.post_recom_carbon.r; overlaid in the per-variable plots
experiments <- list(
    #list(experiment="pi_noxios", from=1850, to=1858, col="black")
    #list(experiment="AWI-ESM3-CC-LR-CMIP7-Spinup", from=1850, to=2229, col="black")
    #,list(experiment="AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230", from=2230, to=2389, col="black") # from: 2230
    list(experiment="AE3CC_pi", from=1850, to=2444, col="black") # continues from 2230 (v3.5.1-cc) where the experiment above (v3.5.0-cc) stops at 2339 -- both overlaid 2230-2339 since they're different model versions, not the same data
)

# all annual means of the monthly output
vars <- list()
for (v in c("benC", "benCalc", "DetC", "DetCalc", "DiaC", "DIC", "DOC", "HetC", "idetz2c", "idetz2calc",
            "PhyC", "PhyCalc", "Zoo2C", "Zoo3C")) vars[[length(vars)+1]] <- list(v=v, unit="PgC")
for (v in c("calcdiss", "calcif", "CO2f", "docexd", "docexn", "GPPd", "GPPn", "NPPd", "NPPn", "respd",
            "respmacro", "respmeso", "respmicro", "respn")) { # >0: into the ocean, i.e. uptake by biota/ocean; releases (resp, calcdiss) flipped
    vars[[length(vars)+1]] <- list(v=v, unit="PgC/yr", fac=if (grepl("^resp|^calcdiss", v)) -1 else 1, signed=!grepl("^docex", v))
}

#####################################################################################################

# post file `f` relative to `pathpost`: its 1st existing copy, else where it is made (under the 1st `pathpost`)
ppost <- function(f) unname(sapply(f, function(x) { hits <- file.path(pathpost, x); hits <- hits[file.exists(hits)]; if (length(hits) > 0) hits[1] else file.path(pathpost[1], x) }))
setup <- "AWI-ESM-3-REcoM"
fin <- function(v, exp) paste0(setup, "/fldsum/", v, "/", v, "_year_fldsum_", setup, "_recom_", exp$experiment, "_r1i1p1f1_gn_", exp$from, "-", exp$to, ".nc")
exps_str <- paste(sapply(experiments, "[[", "experiment"), collapse="_vs_")
fout <- function(name, exp) paste0(pathplot, "/", setup, "/fldsum/cbudget/", name, "_fldsum_", setup, "_recom_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
#fout <- function(name, exp) paste0(pathplot, "/", setup, "/fldsum/cbudget/", name, "_fldsum_", setup, "_recom_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))

# sums and budget terms without own recom output, made here with cdo
mk <- function(vout, cmd, exp) {
    f <- ppost(fin(vout, exp))
    if (force || !file.exists(f)) {
        dir.create(dirname(f), recursive=T, showWarnings=F)
        cmd <- paste0("cdo -O -s -setname,", vout, " ", cmd, " ", f)
        message("run `", cmd, "` ...")
        if (system(cmd) != 0) stop("cdo failed")
    }
}
sums <- list(cBiomass=c("PhyC", "DiaC", "HetC", "Zoo2C", "Zoo3C"), # living
             cPOC=c("DetC", "idetz2c"),                              # detritus
             cPIC=c("PhyCalc", "DetCalc", "idetz2calc"),             # calcite in phytoplankton and detritus
             cBenthos=c("benC", "benCalc"),
             NPP=c("NPPd", "NPPn"), GPP=c("GPPd", "GPPn"),
             respPhy=c("respd", "respn"), respZoo=c("respmicro", "respmeso", "respmacro"))
sums$cOcean <- c("DIC", "DOC", sums$cBiomass, sums$cPOC, sums$cPIC, sums$cBenthos)
for (exp in experiments) {
    p <- function(v) ppost(fin(v, exp))
    for (s in names(sums)) mk(s, paste0("-enssum ", paste(p(sums[[s]]), collapse=" ")), exp)
    # annual means: the change between 2 years ~ the mean flux of these 2 years (1st year dropped)
    mk("dcOceanDt", paste0("-deltat ", p("cOcean")), exp)
    mk("cOceanBudgetResidual", paste0("-sub ", p("dcOceanDt"), " -runmean,2 ", p("CO2f")), exp)
}

comp_cols <- c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey50", "red")
datas_of <- function(comps, exp, sum=NULL) lapply(seq_along(comps), function(i) {
    list(fin=fin(comps[i], exp), label=comps[i], col=if (identical(comps[i], sum)) "black" else comp_cols[i+1], lwd=if (identical(comps[i], sum)) 2 else 1)
})

plots <- list()

if (T) { # per variable: experiments overlaid
    for (x in vars) {
        plots[[length(plots)+1]] <- list(
            datas=lapply(experiments, function(exp) list(fin=fin(x$v, exp), fac=if (is.null(x$fac)) 1 else x$fac, label=exp$experiment, col=exp$col)),
            ylab=paste0(if (identical(x$fac, -1)) "-", x$v, " [", x$unit, "]", if (isTRUE(x$signed)) " (>0: into ocean)"),
            fout=paste0(pathplot, "/", setup, "/fldsum/", x$v, "/", x$v, "_year_fldsum_", setup, "_recom_", exps_str, "_<plot_from>-<plot_to>.png")
        )
    }
}

if (T) { # per experiment: total ocean carbon and dic
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(datas=datas_of(c("DIC", "cOcean"), exp, sum="cOcean"), ylab="pool [PgC]", fout=fout("cocean_total", exp))
    }
}

if (T) { # per experiment: all pools except dic (sum of all incl. dic = cOcean)
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(datas=datas_of(c("DOC", "cBiomass", "cPOC", "cPIC", "cBenthos"), exp), ylab="pool [PgC]", fout=fout("cocean_pools", exp))
    }
}

if (T) { # per experiment: living biomass per group (sum = cBiomass)
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(datas=datas_of(c(sums$cBiomass, "cBiomass"), exp, sum="cBiomass"), ylab="pool [PgC]", fout=fout("cbiomass_components", exp))
    }
}

if (T) { # per experiment: main fluxes (>0: into ocean, i.e. uptake by biota; respiration flipped)
    for (exp in experiments) {
        # 1 color per group; sum solid and thick, components dashed/dotted
        grps <- list(GPP=c("GPP", "GPPd", "GPPn"), NPP=c("NPP", "NPPd", "NPPn"), respPhy=c("respPhy", "respd", "respn"),
                     respZoo=c("respZoo", "respmicro", "respmeso", "respmacro"))
        grp_cols <- c(GPP="#009E73", NPP="#56B4E9", respPhy="#E69F00", respZoo="#CC79A7")
        datas <- list()
        for (g in names(grps)) for (i in seq_along(grps[[g]])) {
            resp <- grepl("^resp", g)
            datas[[length(datas)+1]] <- list(fin=fin(grps[[g]][i], exp), fac=if (resp) -1 else 1, label=paste0(if (resp) "-", grps[[g]][i]),
                                             col=grp_cols[[g]], lty=i, lwd=if (i == 1) 2 else 1)
        }
        plots[[length(plots)+1]] <- list(datas=datas, ylab="flux [PgC/yr] (>0: into ocean)", fout=fout("cflux_main", exp))
    }
}

if (T) { # per experiment: carbon budget: d(cOcean)/dt = CO2f + residual (PgC/yr, >0: into the ocean)
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(
            datas=list(
                list(fin=fin("dcOceanDt", exp), label="d(cOcean)/dt", col="black"),
                list(fin=fin("CO2f", exp), label="CO2f (air-sea)", col=comp_cols[2]),
                list(fin=fin("cOceanBudgetResidual", exp), label="residual", col="red", lwd=2)
            ),
            ylab="flux [PgC/yr] (>0: into ocean)",
            fout=fout("cocean_budget", exp)
        )
    }
}

if (length(plots) == 0) stop("no plot activated; flip at least one `if (T)`/`if (F)` above to `if (T)`")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")

