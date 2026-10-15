# r

# Global integrals of the REcoM carbon pools and fluxes on the fesom2 core3 mesh.
# Run with: `Rscript namelist.post_recom_carbon.r`

#cdo <- "/sw/spack-levante/cdo-2.6.0-akkxhz/bin/cdo" # default: `which cdo`
#force <- T # default: F; set T to redo even if the output file already exists

mmolC_to_PgC <- "-mulc,1.2011e-17"                    # mmol C --> PgC (12.011 g mol-1)
mmolC_d1_to_PgC_yr <- "-mulc,1.2011e-17 -muldpy"     # mmol C d-1 --> PgC yr-1 (calendar days per year)
vars <- list()
for (v in c("benC", "benCalc", "DetC", "DetCalc", "DiaC", "DIC", "DOC", "HetC", "idetz2c", "idetz2calc",
            "PhyC", "PhyCalc", "Zoo2C", "Zoo3C")) { # pools [mmol m-3]; benthos [mmol m-2]
    vars[[length(vars)+1]] <- list(v=v, dim=if (grepl("^ben", v)) "2d" else "3d", conv=mmolC_to_PgC, unit="PgC")
}
for (v in c("CO2f", "GPPd", "GPPn", "NPPd", "NPPn")) { # fluxes [mmol m-2 d-1]; CO2f >0: into the ocean
    vars[[length(vars)+1]] <- list(v=v, dim="2d", conv=mmolC_d1_to_PgC_yr, unit="PgC/yr")
}
for (v in c("calcdiss", "calcif", "docexd", "docexn", "respd", "respmacro", "respmeso", "respmicro", "respn")) { # 3d rates; file says mmolC/m2/d, assumed mmol m-3 d-1
    vars[[length(vars)+1]] <- list(v=v, dim="3d", conv=mmolC_d1_to_PgC_yr, unit="PgC/yr")
}

#####################################################################################################

if (T) {
    experiments <- list(
        list(experiment="pi_noxios", pathin="/work/ab1095/a270073/out/awiesm3-v3.5.0-cc/pi_noxios/outdata/fesom")
        #,list(experiment="pi_noxios_zstar", pathin="/work/ab1095/a270073/out/awiesm3-v3.5.0-cc/pi_noxios_zstar/outdata/fesom")
    )
    pathmesh <- "/work/ab1095/a270073/mesh/fesom2/core3"
    pathout <- "/work/ab1095/a270073/post"
    # area of each node and layer from fesom.mesh.diag.nc (smaller than the surface `cell_area` near the bottom);
    # 1st in `-mul` since the trailing `-select` takes all remaining files; its z-axis with the layer bounds (for `vertint`) is the one kept
    fnod_area_3d <- paste0(pathmesh, "/nod_area_3d_core3.nc")
} # which setting

jobs <- list()
for (exp in experiments) {
    for (x in vars) {
        jobs[[length(jobs) + 1]] <- list(
            model      = "recom",
            experiment = exp$experiment,
            pathin     = exp$pathin,
            fpattern   = paste0(x$v, ".fesom.<year>.nc"),
            varnamein  = x$v,
            varnameout = x$v,
            mode       = "fldsum",
            griddes    = paste0(pathmesh, "/griddes_core3_node.nc"),
            cdo_pre    = if (x$dim == "3d") paste0("-mul ", fnod_area_3d) else paste0("-mul -setzaxis,surface -sellevidx,1 ", fnod_area_3d),
            cdo_post   = paste0("-setname,", x$v, " -setunit,", x$unit, " ", x$conv, if (x$dim == "3d") " -vertint"),
            grid       = "core3",
            pathout    = pathout
        )
    }
}

message("run `source(\"~/scripts/r/post/post.r\")` ...")
source("~/scripts/r/post/post.r")
