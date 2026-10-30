# r

# Example namelist for post.r: set up config (optional; post.r has sensible
# defaults for everything below), activate whichever jobs are wanted via
# `if (T)`/`if (F)`, then source post.r.
# Run with: `Rscript namelist.post.r`

# ---- config (all optional; shown here just to document the defaults) ----
#cdo <- "/sw/spack-levante/cdo-2.6.0-akkxhz/bin/cdo" # default: `which cdo`
#cdo_nthreads <- 32 # default: 75% of `nproc`
#dry <- T # default: F; set T to only print the cdo commands without running them
#force <- T # default: F; set T to redo even if the output file already exists
#clean <- F # default: T; set F to keep temporary chunk/select files for debugging
#save_fout_an <- F # default: T; set F to not also save the annual mean of a sub-annual time series `fout`

jobs <- list() # jobs collected below, from every active `if (T)` block

# One job per entry of `settings`: each dataset's fields override/extend
# `common` (a field set to NULL in a dataset removes it from that job).
# A single job is just `list(list(...))` as `settings`, or `common` alone
# with `settings = list(list())`. `do = F` in a job skips it (default T).
expand_jobs <- function(common, settings) lapply(settings, function(d) utils::modifyList(common, d))

#####################################################################################################

if (F) { # awiesm3 raw fesom/recom
    jobs <- c(jobs, expand_jobs(
        common=list(
            setup   = "AWI-ESM-3-REcoM",
            model   = "fesom",
            #fpattern   = "tra0004.nc",
            fpattern = "ssh.fesom.<year>.nc",
            cdo_pre = "-setgrid,/work/ab0246/a270092/input/fesom2/core3/mesh.nc",
            #cdo_pre = "-setgrid,/work/ab0246/a270092/input/fesom2/core3/mesh.nc -vertint -genlevelbounds",
            #mode    = "select",
            mode    = "fldint",
            #mode    = "fldmean",
            pathout = "/work/ab1095/a270073/post"
        ),
        settings=list(
            #list(experiment="dp_tp2_omp1_y1_r2050_5", pathin="/work/ab1095/a270073/out/awiesm3-develop-cc/dp_tp2_omp1_y1_r2050_5/restart/fesom")
            list(experiment="pi_noxios_zstar_noiceb", pathin="/work/ab1095/a270073/out/awiesm3-v3.5.0-cc/pi_noxios_zstar_noiceb/outdata/fesom")
        )
    ))
}

if (F) { # awiesm1 raw fesom/recom
    jobs <- c(jobs, expand_jobs(
        common=list(
            setup   = "AWI-ESM-1-REcoM",
            model   = "fesom",
            #model   = "recom",
            fpattern   = "siarean_fesom_*<year>0101.nc", mode="select",
            #fpattern   = "aCO2_fesom_*<year>0101.nc", mode="select",
            #fpattern   = "pCO2s_fesom_*<year>0101.nc", mode="fldmean",
            #cdo_pre    = "-shifttime,-1s"
            #cdo_pre    = "-shifttime,-1s -setgrid,/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc",
            #cdo_post    = "-setgrid,/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc",
            #cdo_post   = "-remapycon,r360x180 -setgrid,/pool/data/AWICM/FESOM1/MESHES/core/griddes.nc",
            pathout = "/work/ab1095/a270073/post"
        ),
        settings=list(
            list(
                experiment = "esm-ssp126",
                pathin     = "/work/{ab1095,ba1103,ba1066}/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp126/outdata/{fesom,recom}"
            )
            ,list(
                experiment = "esm-ssp534-over2",
                pathin     = "/work/{ab1095,ba1103,ba1066}/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp534os2/outdata/{fesom,recom}"
            )
            ,list(
                experiment = "esm-ssp585",
                pathin     = "/work/{ab1095,ba1103,ba1066}/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp585/outdata/{fesom,recom}"
                ,from  = 2015
                ,to    = 2039
            )
        )
    ))
}

if (T) { # awiesm1 raw echam/jsbach
    jobs <- c(jobs, expand_jobs(
        common=list(
            setup   = "AWI-ESM-1-REcoM",
            model   = "echam6", varnamein="co2_burden", fpattern="*_<year><mon>.01_co2", code="*_<year><mon>.01_co2.codes", mode="fldmean", # the model is "echam6", never "echam"
            #model   = "jsbach",
            pathout = "/work/ab1095/a270073/post"
        ),
        settings=list(
            list(
                experiment = "ssp126",
                pathin     = "/work/{ab1095,ba1103,ba1066}/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp126/outdata/{echam,jsbach}"
            )
            ,list(
                experiment = "ssp534-over2",
                pathin     = "/work/{ab1095,ba1103,ba1066}/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp534-over2/outdata/{echam,jsbach}"
            )
            ,list(
                experiment = "ssp585",
                pathin     = "/work/{ab1095,ba1103,ba1066}/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/ssp585/outdata/{echam,jsbach}"
                ,from  = 2015
                ,to    = 2039
            )
            ,list(
                experiment = "esm-ssp126",
                pathin     = "/work/{ab1095,ba1103,ba1066}/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp126/outdata/{echam,jsbach}"
            )
            ,list(
                experiment = "esm-ssp534-over2",
                pathin     = "/work/{ab1095,ba1103,ba1066}/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp534os2/outdata/{echam,jsbach}"
            )
            ,list(
                experiment = "esm-ssp585",
                pathin     = "/work/{ab1095,ba1103,ba1066}/a270073/out/awicm-1.0-recom/awi-esm-1-1-lr_kh800/esm-ssp585/outdata/{echam,jsbach}"
                ,from  = 2015
                ,to    = 2039
            )
        )
    ))
}


if (F) { # awiesm1 cmorized
    jobs <- c(jobs, expand_jobs(
        common=list(
            setup   = "AWI-ESM-1-REcoM", # no `model`: cmorized
            fpattern   = "spco2_Omon_*_<from>01-<to>12.nc",
            mode    = "select",
            #mode    = "fldmean",
            pathout = "/work/ab1095/a270073/post",
            grid    = "gr1"
        ),
        settings=list(
            list(
                experiment = "ssp126",
                pathin     = "/work/ik1017/ESGF_Buff/CMIP6/CMIP/AWI/AWI-ESM-1-REcoM_tmp/ScenarioMIP/AWI/AWI-ESM-1-REcoM/ssp126/r1i1p1f1"
            )
            ,list(
                do         = F,
                experiment = "ssp534-over",
                pathin     = "/work/ik1017/ESGF_Buff/CMIP6/CMIP/AWI/AWI-ESM-1-REcoM_tmp/ScenarioMIP/AWI/AWI-ESM-1-REcoM/ssp534-over/r1i1p1f1"
            )
            ,list(
                do         = F,
                experiment = "ssp585",
                pathin     = "/work/ik1017/ESGF_Buff/CMIP6/CMIP/AWI/AWI-ESM-1-REcoM_tmp/ScenarioMIP/AWI/AWI-ESM-1-REcoM/ssp585/r1i1p1f1",
                from  = 2015,
                to    = 2039
            )
        )
    ))
}

if (length(jobs) == 0) stop("no job activated; flip at least one `if (T)`/`if (F)` above to `if (T)`")

message("run `source(\"~/scripts/r/post/post.r\")` ...")
source("~/scripts/r/post/post.r")

