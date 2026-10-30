# r

#cdo <- "/sw/spack-levante/cdo-2.6.0-akkxhz/bin/cdo" # default: `which cdo`
#force <- T # default: F; set T to redo even if the output file already exists

kgC_to_PgC <- "-mulc,1e-12" # kg C (yr-1) --> PgC (yr-1)
kgCH4_s1_to_PgC_yr <- paste0("-mulc,", 86400*1e-12*12.011/16.043, " -muldpy") # kg CH4 s-1 --> PgC yr-1 (calendar days per year)
kgC_s1_to_PgC_yr <- paste0("-mulc,", 86400*1e-12, " -muldpy") # kg C s-1 --> PgC yr-1 (calendar days per year)

vars <- list(
    # budget
    list(f="cLandYr_yearly", v="cLandYr", conv=kgC_to_PgC, unit="PgC"), # on jan 1, incl. the pending annual fluxes (coupling)
    list(f="cFluxYr_yearly", v="cFluxYr", conv=kgC_to_PgC, unit="PgC/yr"), # sent to openifs, positive into the atmosphere
    list(f="cLand_yearly", v="cLand", conv=kgC_to_PgC, unit="PgC"), # on dec 31: cVeg + cLitter + cSoil + cProduct
    list(f="cflux", v="NEE", vout="cflux_NEE", conv=kgC_to_PgC, unit="PgC/yr"), # all c fluxes to the atmosphere (= -nbp)
    # nbp = netAtmosLandCO2Flux differ from cflux_NEE by 0.11–0.19 PgC/yr potentially due to monthly vs annual bookkeeping in lpj
    # npb - nep =   LU_ch   # wood harvest + clearing + land-use change
    #             + Seed    # seed carbon for crops
    #             + Harvest # crop/pasture harvest to the atmosphere
    #             + Slow_h  # decay of the slow harvested-product pool
    #           = fLuc + fHarvestToAtmos + fProductDecomp + seed
    #           = fAnthDisturb + fProductDecomp + seed
    list(f="wetlandCH4_monthly", v="wetlandCH4", conv=kgCH4_s1_to_PgC_yr, unit="PgC/yr"), # as c: lost from the coupled c cycle
    # pools on dec 31 [kg C m-2]
    list(f="cVeg_yearly", v="cVeg", conv=kgC_to_PgC, unit="PgC"), # cVeg = cLeaf + cStem + cRoot + cOther (cVegTree + cVegShrub + cVegGrass != cVeg since cVeg contains the NPP not yet allocated to the plant parts)
    list(f="cLitter_yearly", v="cLitter", conv=kgC_to_PgC, unit="PgC"),
    list(f="cSoil_yearly", v="cSoil", conv=kgC_to_PgC, unit="PgC"),
    list(f="cProduct_yearly", v="cProduct", conv=kgC_to_PgC, unit="PgC"),
    # main fluxes [kg C m-2 yr-1]
    #list(f="anpp", v="Total", vout="anpp", conv=kgC_to_PgC, unit="PgC/yr"), some missing due to lpjg2nc
    list(f="cflux", v="Soil", vout="cflux_Soil", conv=kgC_to_PgC, unit="PgC/yr"), # = annual SOILC + organic carbon leaching = rh + fCLandToOcean to within 0.001 PgC yr-1
    list(f="cflux", v="Fire", vout="cflux_Fire", conv=kgC_to_PgC, unit="PgC/yr")
)

# all other c variables (except *Lut, per-pft and duplicates); monthly pools [kg C m-2], fluxes [kg C m-2 s-1]
if (T) {
    for (v in c("cLand", "cLand1st", "cLeaf", "cLitter", "cLitterCwd", "cLitterSubSurf", "cLitterSurf", "cOther", "cProduct", "cRoot",
                "cSoil", "cSoilFast", "cSoilMedium", "cSoilSlow", "cStem",
                #"cVeg", # some missing due to lpjg2nc
                "cVegGrass", "cVegShrub", "cVegTree")) {
        vars[[length(vars)+1]] <- list(f=paste0(v, "_monthly"), v=v, conv=kgC_to_PgC, unit="PgC")
    }
    # fCLandToOcean is sent to atmopshere, included in:
    # - nep and nbp: subtracted as a loss from the land (CMIPoutput.cpp:1377).
    # - cflux: it's in Soil (= Rh + leaching, commonoutput.cpp:1863) and therefore also in NEE.
    for (v in c("fAnthDisturb", "fCLandToOcean", "fco2antt", "fco2nat", "fDeforestToAtmos", "fDeforestToProduct", "fFire", "fFireAll", "fFireNat", "fHarvestToAtmos",
                "fHarvestToProduct", "fLitterFire", "fLitterSoil", "fLuc", "fProductDecomp", "fVegFire", "fVegLitter", "fVegLitterMortality", "fVegLitterSenescence", "fVegOther",
                "gpp", "gppGrass", "gppShrub", "gppTree", "nbp", "nep", "netAtmosLandCO2Flux", "npp", "nppGrass", "nppLeaf",
                "nppOther", "nppRoot", "nppShrub", "nppStem", "nppTree", "ra", "raGrass", "raLeaf", "raOther", "raRoot",
                "raShrub", "raStem", "raTree", "rGrowth", "rh", "rhGrass", "rhLitter", "rhShrub", "rhSoil", "rhTree",
                "rMaint")) {
        vars[[length(vars)+1]] <- list(f=paste0(v, "_monthly"), v=v, conv=kgC_s1_to_PgC_yr, unit="PgC/yr")
    }
    for (v in c("wetlandCH4cons", "wetlandCH4prod")) { # kg CH4 m-2 s-1
        vars[[length(vars)+1]] <- list(f=paste0(v, "_monthly"), v=v, conv=kgCH4_s1_to_PgC_yr, unit="PgC/yr")
    }
    for (v in c("Est", "Harvest", "LU_ch", "Repr", "Seed", "Slow_h", "Veg")) { # remaining cflux columns [kg C m-2 yr-1]
        vars[[length(vars)+1]] <- list(f="cflux", v=v, vout=paste0("cflux_", v), conv=kgC_to_PgC, unit="PgC/yr")
    }
}

#####################################################################################################

if (F) { # levante
    experiments <- list(
        list(experiment="pi_noxios", pathin="/work/ab1095/a270073/out/awiesm3-v3.5.0-cc/pi_noxios/outdata/lpj_guess")
        #,list(experiment="pi_noxios_zstar", pathin="/work/ab1095/a270073/out/awiesm3-v3.5.0-cc/pi_noxios_zstar/outdata/lpj_guess")
    )
    lpj_remap_fpattern <- "remapcon_r360x180"
    flandarea <- "/work/ab1095/a270073/mesh/lpj/TCO95/landarea_remapcon_r360x180_destarea.nc"
    pathout <- "/work/ab1095/a270073/post"

} else if (T) { # albedo
    experiments <- list(
        #list(experiment="AWI-ESM3-CC-LR-CMIP7-Spinup", pathin="/albedo/work/projects/p_awiesm3_cmip7/lahajd001/runtime/awiesm3-v3.5.0-cc/AWI-ESM3-CC-LR-CMIP7-Spinup/outdata/lpj_guess") # , from=2060)
        list(experiment="AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230", pathin="/albedo/work/projects/p_awiesm3_cmip7/lahajd001/runtime/awiesm3-v3.5.1-cc/AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230/outdata/lpj_guess", from=2390) # from: 2230; 2230-2389 already post-processed
    )
    lpj_remap_fpattern <- "remapcon_r360x180"
    flandarea <- "/albedo/work/user/cdanek/mesh/lpj/TCO95/landarea_remapcon_r360x180_destarea.nc"
    pathout <- "/albedo/work/user/cdanek/post"

} # which setting

#####################################################################################################

jobs <- list()
for (exp in experiments) {
    for (x in vars) {
        jobs[[length(jobs) + 1]] <- list(
            setup      = "AWI-ESM-3-REcoM",
            model      = "lpj_guess",
            experiment = exp$experiment,
            pathin     = exp$pathin,
            from       = exp$from,
            to         = exp$to,
            fpattern   = paste0("<from>0101-<to>1231/", x$f, "_", lpj_remap_fpattern, ".nc"),
            varnamein  = x$v,
            varnameout = if (!is.null(x$vout)) x$vout else if (grepl("_(yearly|monthly)$", x$f)) x$f else x$v, # e.g. cVeg_yearly, cVeg_monthly
            mode       = "fldint",
            cdo_pre    = paste0("-yearmonmean -setgridarea,", flandarea), # lpjg2nc remapped with cdo's default CDO_REMAP_NORM=fracarea, i.e. values per m2 of land --> fldint uses land area instead of the cell area; same as `-mul flandfrac`
            dtin       = "year", # cdo_pre's `-yearmonmean` already reduces monthly input to annual before `fldint` runs on it (a no-op for already-yearly input)
            cdo_post   = paste0("-setunit,", x$unit, " ", x$conv),
            grid       = "r360x180",
            pathout    = pathout
        )
    }
}

message("run `source(\"~/scripts/r/post/post.r\")` ...")
source("~/scripts/r/post/post.r")

