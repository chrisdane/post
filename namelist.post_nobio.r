# r

# post-processing for Hauck et al. (nobio paper), figs. 1, 2 and 4: awiesm1-recom (fesom1) emission-driven suite
#   - esm-piControl (esm-piControl_wout_talk_rest2) and esm-piControl_nobio_spinup: 3208-4527
#   - esm-hist{,_nobio}: 1850-2014; esm-ssp{245,585}{,_nobio}: 2015-2100
#   - perturbations esm-{hist,ssp245,ssp585}_{remin_m25,remin_p25,vdeta_m25_vdet_m25,vdeta_p25_vdet_p25}{,_i2,_i3}:
#     hist 1981-2014, ssp 2015-2100; member _i<n> --> variant r1i<n>p1f1
# variables (global integrals, monthly; post.r also writes the annual means):
#   - recom CO2f [mmolC d-1 after fldint; >0: into the ocean]: air-sea CO2 flux
#   - echam co2_flx_ocean, co2_flx_land [kg CO2 s-1; >0: into the atmosphere]: air-sea and air-land CO2 flux
#   - echam co2_burden [ppm]: atmospheric CO2; not recom's aCO2, which is wrong
# units of CO2f, co2_flx_ocean and co2_flx_land are left as they are, as in the old files renamed by
# rename_post_nobio.sh; co2_burden is converted to ppm
#   - recom bgc02 [mmolC m-3]: DIC volume-weighted mean of every level (annual), from levelwise_bgc02_nobio.sh
# not covered: DICremin
# run with: `Rscript namelist.post_nobio.r`

#dry <- T # default: F; set T to only print the cdo commands without running them
#force <- T # default: F; set T to redo even if the output file already exists

pathout <- "/work/ab1095/a270073/post"
setup <- "AWI-ESM-1-REcoM"
pathin <- "/work/{ab1095,ba1103,ba1066}/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/<run>/outdata/"
fgriddes <- "/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc"
kgCO2_to_ppm <- "-setunit,ppm -mulc,1.2848942e-13" # 12.0107/44.0095 kgC/kgCO2, 1e12 kg/Pg, 2.124 PgC/ppm

runs <- list(
    list(run="esm-piControl_wout_talk_rest2", from=3208, to=4527),
    list(run="esm-piControl_nobio_spinup", from=3208, to=4527)
)
for (r in c("esm-hist", "esm-hist_nobio")) runs[[length(runs)+1]] <- list(run=r, from=1850, to=2014)
for (r in c("esm-ssp245", "esm-ssp585", "esm-ssp245_nobio", "esm-ssp585_nobio")) {
    runs[[length(runs)+1]] <- list(run=r, from=2015, to=2100)
}
for (p in c("remin_m25", "remin_p25", "vdeta_m25_vdet_m25", "vdeta_p25_vdet_p25")) {
    for (i in c("", "_i2", "_i3")) {
        runs[[length(runs)+1]] <- list(run=paste0("esm-hist_", p, i), from=1981, to=2014)
        for (s in c("esm-ssp245_", "esm-ssp585_")) runs[[length(runs)+1]] <- list(run=paste0(s, p, i), from=2015, to=2100)
    }
}

vars <- list(
    list(model="recom", v="CO2f", fpattern="CO2f_fesom_mon_<year>0101.nc", # some runs also have daily CO2f_fesom_<year>0101.nc
         cdo_pre=paste0("-shifttime,-1s -setgrid,", fgriddes), # fesom1 time stamps are at the end of the month
         cdo_post="-setunit,mmolC/d"), # fldint of mmolC m-2 d-1
    list(model="echam6", v="co2_flx_ocean"),
    list(model="echam6", v="co2_flx_land"),
    list(model="echam6", v="co2_burden", cdo_post=kgCO2_to_ppm)
)

#####################################################################################################

jobs <- list()
for (r in runs) {
    experiment <- sub("_i[0-9]+$", "", r$run)
    member <- if (grepl("_i[0-9]+$", r$run)) sub(".*_i([0-9]+)$", "\\1", r$run) else "1"
    for (x in vars) {
        job <- list(
            setup      = setup,
            model      = x$model,
            experiment = experiment,
            variant    = paste0("r1i", member, "p1f1"),
            pathin     = paste0(sub("<run>", r$run, pathin, fixed=T), if (x$model == "recom") "recom" else "echam"),
            from       = r$from,
            to         = r$to,
            varnamein  = x$v,
            mode       = "fldint",
            pathout    = pathout
        )
        if (x$model == "echam6") {
            job$fpattern <- "*_<year><mon>.01_co2"
            job$code <- "*_<year><mon>.01_co2.codes"
        } else {
            job$fpattern <- x$fpattern
        }
        if (!is.null(x$cdo_pre)) job$cdo_pre <- x$cdo_pre
        if (!is.null(x$cdo_post)) job$cdo_post <- x$cdo_post
        jobs[[length(jobs)+1]] <- job
    }
}

# DIC depth profiles for the hovmoeller plots (figs. 1 and 2): volume-weighted mean of every level of the levelwise
# annual means made by levelwise_bgc02_nobio.sh. `fldsum` of bgc02 times the weights cluster_vol/fldsum(cluster_vol)
# per level (zero where there is no ocean) is the level mean; cdo's `fldmean` would weight every level with the
# 2d (surface) cell_area of griddes, which over-weights coastal and slope nodes at depth
fweights <- "/work/ab1095/a270073/mesh/fesom/core/fesom.initial.mesh.diag.nc_levelwise_0-5900m_fldmean_weights.nc"
for (r in runs[sapply(runs, function(x) !grepl("_(remin|vdeta)_", x$run))]) {
    jobs[[length(jobs)+1]] <- list(
        setup      = setup,
        model      = "recom",
        experiment = r$run,
        pathin     = paste0("/work/ba1066/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/", r$run,
                            "/outdata/post/recom/levelwise/yearmonmean"),
        from       = r$from,
        to         = r$to,
        fpattern   = "bgc02_fesom_<year>0101_levelwise_0-5900m.nc",
        varnamein  = "bgc02",
        mode       = "fldsum",
        mode_name  = "fldmean",
        cdo_pre    = paste0("-mul ", fweights),
        pathout    = pathout
    )
}

message("run `source(\"~/scripts/r/post/post.r\")` ...")
source("~/scripts/r/post/post.r")
