# r

#cdo <- "/sw/spack-levante/cdo-2.6.0-akkxhz/bin/cdo" # default: `which cdo`
#force <- T # default: F; set T to redo even if the output file already exists

mmolC_to_PgC <- "-mulc,1.2011e-17" # mmol C --> PgC (12.011 g mol-1)
mmolC_d1_to_PgC_yr <- "-mulc,1.2011e-17 -muldpy" # mmol C d-1 --> PgC yr-1 (calendar days per year)
vars <- list()
for (v in c("CO2f", "GPPd", "GPPn", "NPPd", "NPPn")) { # fluxes [mmol m-2 d-1]; CO2f >0: into the ocean
    vars[[length(vars)+1]] <- list(v=v, dim="2d", conv=mmolC_d1_to_PgC_yr, unit="PgC/yr")
}
for (v in c("benC", "benCalc", "DetC", "DetCalc", "DiaC", "DIC", "DOC", "HetC", "idetz2c", "idetz2calc",
            "PhyC", "PhyCalc", "Zoo2C", "Zoo3C")) { # pools [mmol m-3]; benthos [mmol m-2]
    vars[[length(vars)+1]] <- list(v=v, dim=if (grepl("^ben", v)) "2d" else "3d", conv=mmolC_to_PgC, unit="PgC")
}
for (v in c("calcdiss", "calcif", "docexd", "docexn", "respd", "respmacro", "respmeso", "respmicro", "respn")) { # 3d rates; file says mmolC/m2/d, assumed mmol m-3 d-1
    vars[[length(vars)+1]] <- list(v=v, dim="3d", conv=mmolC_d1_to_PgC_yr, unit="PgC/yr")
}

#####################################################################################################

if (F) { # levante
    experiments <- list(
        list(experiment="pi_noxios", pathin="/work/ab1095/a270073/out/awiesm3-v3.5.0-cc/pi_noxios/outdata/fesom")
        #,list(experiment="pi_noxios_zstar", pathin="/work/ab1095/a270073/out/awiesm3-v3.5.0-cc/pi_noxios_zstar/outdata/fesom")
    )
    fgriddes <- "/work/ab1095/a270073/mesh/fesom2/core3/griddes_core3_node.nc"
    fnod_area_3d <- "/work/ab1095/a270073/mesh/fesom2/core3/nod_area_3d_core3.nc"
    pathout <- "/work/ab1095/a270073/post"

} else if (T) { # albedo
    experiments <- list(
        #list(experiment="AWI-ESM3-CC-LR-CMIP7-Spinup", pathin="/albedo/work/projects/p_awiesm3_cmip7/lahajd001/runtime/awiesm3-v3.5.0-cc/AWI-ESM3-CC-LR-CMIP7-Spinup/outdata/fesom", from=2060)
        list(experiment="AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230", pathin="/albedo/work/projects/p_awiesm3_cmip7/lahajd001/runtime/awiesm3-v3.5.1-cc/AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230/outdata/fesom", from=2390) # from: 2230; 2230-2389 already post-processed
    )
    fgriddes <- "/albedo/work/user/cdanek/mesh/fesom2/core3/griddes_core3_node.nc"
    fnod_area_3d <- "/albedo/work/user/cdanek/mesh/fesom2/core3/nod_area_3d_core3.nc"
    pathout <- "/albedo/work/user/cdanek/post"

} # which setting

#####################################################################################################

jobs <- list()
for (exp in experiments) {
    for (x in vars) {
        jobs[[length(jobs) + 1]] <- list(
            setup      = "AWI-ESM-3-REcoM",
            model      = "recom",
            experiment = exp$experiment,
            pathin     = exp$pathin,
            from       = exp$from,
            to         = exp$to,
            fpattern   = paste0(x$v, ".fesom.<year>.nc"),
            varnamein  = x$v,
            varnameout = x$v,
            mode       = "fldsum",
            cdo_pre    = paste0("-yearmonmean ", if (x$dim == "3d") paste0("-mul ", fnod_area_3d) else paste0("-mul -setzaxis,surface -sellevidx,1 ", fnod_area_3d), " -setgrid,", fgriddes),
            dtin       = "year", # cdo_pre's `-yearmonmean` already reduces the monthly input to annual before `fldsum` runs on it
            cdo_post   = paste0("-setunit,", x$unit, " ", x$conv, if (x$dim == "3d") " -vertint"),
            grid       = "core3",
            pathout    = pathout
        )
    }
}

message("run `source(\"~/scripts/r/post/post.r\")` ...")
source("~/scripts/r/post/post.r")
