# r

# namelist for post.r: activate blocks with `if (T)`; run with `Rscript namelist.post.r`

# config (optional; defaults in post.r)
#cdo <- "/sw/spack-levante/cdo-2.6.0-akkxhz/bin/cdo"
#cdo_nthreads <- 32
#dry <- T # only print the cdo commands
#force <- T # redo existing fout
#clean <- F # keep temporary files
#save_fout_an <- F # no annual mean of sub-annual time series

jobs <- list()

# 1 job per entry of `settings`, merged into `common`; NULL removes a field
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
