# r

# Plots of the LPJ-GUESS carbon time series made by namelist.post_lpj_carbon.r.
# Run with: `Rscript namelist.plot_lpj_carbon.r`

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

# experiments as in namelist.post_lpj_carbon.r; overlaid in the per-variable plots
experiments <- list(
    #list(experiment="pi_noxios", from=1850, to=1858, col="black")
    #list(experiment="AWI-ESM3-CC-LR-CMIP7-Spinup", from=1850, to=2229, col="gray")
    #,list(experiment="AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230", from=2230, to=2389, col="black") # from: 2230
    list(experiment="AE3CC_pi", from=1850, to=2389, col="black")
)

# output variable of namelist.post_lpj_carbon.r and its <dtin> in the post.r file name
# ("year": annual output of lpj_guess or annual mean of a monthly series)
vars <- list(
    list(v="cLandYr_yearly", dt="year", unit="PgC"), list(v="cFluxYr_yearly", dt="year", unit="PgC/yr"), list(v="cLand_yearly", dt="year", unit="PgC"),
    list(v="cflux_NEE", dt="year", unit="PgC/yr"), list(v="wetlandCH4_monthly", dt="year", unit="PgC/yr"),
    list(v="cVeg_yearly", dt="year", unit="PgC"), list(v="cLitter_yearly", dt="year", unit="PgC"), list(v="cSoil_yearly", dt="year", unit="PgC"), list(v="cProduct_yearly", dt="year", unit="PgC"),
    list(v="cflux_Soil", dt="year", unit="PgC/yr"), list(v="cflux_Fire", dt="year", unit="PgC/yr") # anpp = cflux_Veg -- not its own post file (job commented out in namelist.post_lpj_carbon.r)
)
if (T) {
    for (v in c("cLand", "cLand1st", "cLeaf", "cLitter", "cLitterCwd", "cLitterSubSurf", "cLitterSurf", "cOther", "cProduct", "cRoot",
                "cSoil", "cSoilFast", "cSoilMedium", "cSoilSlow", "cStem", "cVegGrass", "cVegShrub", "cVegTree")) vars[[length(vars)+1]] <- list(v=paste0(v, "_monthly"), dt="year", unit="PgC") # cVeg_monthly some missing due to lpj2nc
    for (v in c("fAnthDisturb", "fCLandToOcean", "fco2antt", "fco2nat", "fDeforestToAtmos", "fDeforestToProduct", "fFire", "fFireAll", "fFireNat", "fHarvestToAtmos",
                "fHarvestToProduct", "fLitterFire", "fLitterSoil", "fLuc", "fProductDecomp", "fVegFire", "fVegLitter", "fVegLitterMortality", "fVegLitterSenescence", "fVegOther",
                "gpp", "gppGrass", "gppShrub", "gppTree", "nbp", "nep", "netAtmosLandCO2Flux", "npp", "nppGrass", "nppLeaf",
                "nppOther", "nppRoot", "nppShrub", "nppStem", "nppTree", "ra", "raGrass", "raLeaf", "raOther", "raRoot",
                "raShrub", "raStem", "raTree", "rGrowth", "rh", "rhGrass", "rhLitter", "rhShrub", "rhSoil", "rhTree",
                "rMaint", "wetlandCH4cons", "wetlandCH4prod")) vars[[length(vars)+1]] <- list(v=paste0(v, "_monthly"), dt="year", unit="PgC/yr")
    for (v in c("Est", "Harvest", "LU_ch", "Repr", "Seed", "Slow_h", "Veg")) vars[[length(vars)+1]] <- list(v=paste0("cflux_", v), dt="year", unit="PgC/yr")
}

#####################################################################################################

# post file `f` relative to `pathpost`: its 1st existing copy, else where it is made (under the 1st `pathpost`)
ppost <- function(f) unname(sapply(f, function(x) { hits <- file.path(pathpost, x); hits <- hits[file.exists(hits)]; if (length(hits) > 0) hits[1] else file.path(pathpost[1], x) }))
fin <- function(v, dt, exp) paste0("lpj_guess/fldint/", v, "/", v, "_", dt, "_fldint_lpj_guess_", exp$experiment, "_r1i1p1f1_r360x180_", exp$from, "-", exp$to, ".nc")
exps_str <- paste(sapply(experiments, "[[", "experiment"), collapse="_vs_")

plots <- list()

if (T) { # per variable: experiments overlaid
    for (x in vars) {
        plots[[length(plots)+1]] <- list(
            datas=lapply(experiments, function(exp) list(fin=fin(x$v, x$dt, exp), label=exp$experiment, col=exp$col)),
            ylab=paste0(x$v, " [", x$unit, "]"),
            fout=paste0(pathplot, "/lpj_guess/fldint/", x$v, "/", x$v, "_", x$dt, "_fldint_lpj_guess_", exps_str, "_<plot_from>-<plot_to>.png")
        )
    }
}

if (T) { # per experiment: the fluxes out of the land in the carbon budget (all PgC/yr, positive out of the land)
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(
            datas=list(
                list(fin=fin("cFluxYr_yearly", "year", exp), label="cFluxYr_yearly (to openifs)", col="black"),
                list(fin=fin("cflux_NEE", "year", exp), label="cflux_NEE", col="red"),
                list(fin=fin("wetlandCH4_monthly", "year", exp), label="wetlandCH4_monthly (as C)", col="blue")
            ),
            ylab="flux [PgC/yr] (>0: into atm)",
            #fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cflux_budget_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cflux_budget_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
        )
    }
}

if (T) { # per experiment: main fluxes (PgC/yr)
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(
            datas=list(
                #list(fin=fin("anpp", "year", exp), label="NPP", col="darkgreen"), # anpp = cflux_Veg; some anpp missing due to lpjg2nc
                list(fin=fin("cflux_Veg", "year", exp), fac=-1, label="NPP", col="darkgreen"), # cflux_Veg = anpp
                list(fin=fin("cflux_Soil", "year", exp), label="Soil (Rh)", col="brown"),
                list(fin=fin("cflux_Fire", "year", exp), label="fire", col="red")
            ),
            ylab="flux [PgC/yr]",
            #fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cflux_main_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cflux_main_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
        )
    }
}

comp_cols <- c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey50", "black")

if (T) { # per experiment: all carbon flux components (cflux columns; sum = NEE; >0: into atm)
    comps <- c("Veg", "Repr", "Soil", "Fire", "Est", "Seed", "Harvest", "LU_ch", "Slow_h", "NEE")
    comps_labs <- c("Veg (NPP)", "Repr", "Soil (Rh)", "Fire", "Est", "Seed", "Harvest", "LU_ch", "Slow_h", "NEE (sum)")
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(
            datas=lapply(seq_along(comps), function(i) list(fin=fin(paste0("cflux_", comps[i]), "year", exp), label=comps_labs[i], col=comp_cols[i], lwd=if (comps[i] == "NEE") 2 else 1)),
            ylab="flux [PgC/yr] (>0: into atm)",
            #fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cflux_components_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cflux_components_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
        )
    }
}

if (T) { # per experiment: all carbon pool components (sum = cLand_monthly)
    comps <- list(c("cLeaf_monthly", "year"), c("cStem_monthly", "year"), c("cRoot_monthly", "year"), c("cOther_monthly", "year"),
                  c("cLitterSurf_monthly", "year"), c("cLitterSubSurf_monthly", "year"),
                  c("cSoilFast_monthly", "year"), c("cSoilMedium_monthly", "year"), c("cSoilSlow_monthly", "year"), c("cProduct_monthly", "year"))
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(
            datas=lapply(seq_along(comps), function(i) list(fin=fin(comps[[i]][1], comps[[i]][2], exp), label=comps[[i]][1], col=comp_cols[i])),
            ylab="pool [PgC]",
            #fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cpool_components_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cpool_components_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
        )
    }
}

if (F) { # per experiment: main land carbon pools (sum = cLand_monthly)
    comps <- c("cVeg_monthly", "cLitter_monthly", "cSoil_monthly", "cProduct_monthly", "cLand_monthly")
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(
            datas=lapply(seq_along(comps), function(i) list(fin=fin(comps[i], "year", exp), label=comps[i], col=if (comps[i] == "cLand_monthly") "black" else comp_cols[i+1], lwd=if (comps[i] == "cLand_monthly") 2 else 1)),
            ylab="pool [PgC]",
            #fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cland_pools_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cland_pools_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
        )
    }
}

if (F) { # per experiment: cVeg per organ (sum = cVeg_monthly, incl. the npp not yet allocated to the organs)
    comps <- c("cLeaf_monthly", "cStem_monthly", "cRoot_monthly", "cOther_monthly", "cVeg_monthly")
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(
            datas=lapply(seq_along(comps), function(i) list(fin=fin(comps[i], "year", exp), label=comps[i], col=comp_cols[i], lwd=if (comps[i] == "cVeg_monthly") 2 else 1)),
            ylab="cVeg [PgC]",
            #fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cveg_organs_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cveg_organs_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
        )
    }
}

if (T) { # per experiment: cVeg per plant type and their sum (excl. the not yet allocated npp; no lpj output, made here with `cdo enssum`)
    comps <- c("cVegTree_monthly", "cVegShrub_monthly", "cVegGrass_monthly", "cVegTreeShrubGrass_monthly")
    for (exp in experiments) {
        fsum <- ppost(fin(comps[4], "year", exp))
        if (force || !file.exists(fsum)) {
            dir.create(dirname(fsum), recursive=T, showWarnings=F)
            cmd <- paste0("cdo -O -s -setname,", comps[4], " -enssum ", paste(ppost(sapply(comps[1:3], fin, dt="year", exp=exp)), collapse=" "), " ", fsum)
            message("run `", cmd, "` ...")
            if (system(cmd) != 0) stop("cdo failed")
        }
        plots[[length(plots)+1]] <- list(
            datas=lapply(seq_along(comps), function(i) list(fin=fin(comps[i], "year", exp), label=comps[i], col=comp_cols[i], lwd=if (i == 4) 2 else 1)),
            ylab="cVeg [PgC]",
            #fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cveg_types_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cveg_types_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
        )
    }
}

if (T) { # per experiment: cLitter per pool (surf + subsurf = cLitter_monthly; cwd is part of surf)
    comps <- c("cLitterSurf_monthly", "cLitterSubSurf_monthly", "cLitterCwd_monthly", "cLitter_monthly")
    comps_labs <- c("cLitterSurf_monthly", "cLitterSubSurf_monthly", "cLitterCwd_monthly (subset of Surf)", "cLitter_monthly")
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(
            datas=lapply(seq_along(comps), function(i) list(fin=fin(comps[i], "year", exp), label=comps_labs[i], col=comp_cols[i],
                                                             lty=if (comps[i] == "cLitterCwd_monthly") 2 else 1, lwd=if (comps[i] == "cLitter_monthly") 2 else 1)),
            ylab="cLitter [PgC]",
            #fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/clitter_pools_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/clitter_pools_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
        )
    }
}

if (T) { # per experiment: cSoil per pool (sum = cSoil_monthly)
    comps <- c("cSoilFast_monthly", "cSoilMedium_monthly", "cSoilSlow_monthly", "cSoil_monthly")
    for (exp in experiments) {
        plots[[length(plots)+1]] <- list(
            datas=lapply(seq_along(comps), function(i) list(fin=fin(comps[i], "year", exp), label=comps[i], col=comp_cols[i], lwd=if (comps[i] == "cSoil_monthly") 2 else 1)),
            ylab="cSoil [PgC]",
            #fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/csoil_pools_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/csoil_pools_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
        )
    }
}

if (T) { # per experiment: lpj's carbon budget without methane: d(cLand)/dt = -NEE + residual, residual ~ -wetland CH4 (PgC/yr, >0: into land)
    # cLand_yearly is on dec 31 --> its difference to the previous year is the change during the year (1st year dropped); made here with cdo
    for (exp in experiments) {
        f <- function(v) ppost(fin(v, "year", exp))
        if (force || !file.exists(f("dcLandDt_yearly"))) {
            cmds <- c(paste0("cdo -O -s -setname,dcLandDt_yearly -deltat ", f("cLand_yearly"), " ", f("dcLandDt_yearly")),
                      paste0("cdo -O -s -setname,cLandBudgetResidual -add ", f("dcLandDt_yearly"), " -delete,timestep=1 ", f("cflux_NEE"), " ", f("cLandBudgetResidual")))
            for (cmd in cmds) {
                dir.create(dirname(sub(".* ", "", cmd)), recursive=T, showWarnings=F)
                message("run `", cmd, "` ...")
                if (system(cmd) != 0) stop("cdo failed")
            }
        }
        plots[[length(plots)+1]] <- list(
            datas=list(
                list(fin=fin("dcLandDt_yearly", "year", exp), label="d(cLand_yearly)/dt", col="black"),
                list(fin=fin("cflux_NEE", "year", exp), fac=-1, label="-cflux_NEE", col=comp_cols[2]),
                list(fin=fin("cLandBudgetResidual", "year", exp), label="residual", col="red", lwd=2),
                list(fin=fin("wetlandCH4_monthly", "year", exp), fac=-1, label="-wetlandCH4_monthly (as C)", col=comp_cols[3], lty=2)
            ),
            ylab="flux [PgC/yr] (>0: into land)",
            #fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cland_budget_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png", "pdf"))
            fout=paste0(pathplot, "/lpj_guess/fldint/cbudget/cland_budget_fldint_lpj_guess_", exp$experiment, "_<plot_from>-<plot_to>.", c("png"))
        )
    }
}

if (length(plots) == 0) stop("no plot activated; flip at least one `if (T)`/`if (F)` above to `if (T)`")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")

