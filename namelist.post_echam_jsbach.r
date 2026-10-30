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

if (T) { # awiesm1 raw echam/jsbach
    jobs <- c(jobs, expand_jobs(
        common=list(
            setup   = "AWI-ESM-1-REcoM",
            model   = "echam6", varnamein="co2_burden", fpattern="*_<year><mon>.01_co2", code="*_<year><mon>.01_co2.codes", mode="fldmean",
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



if (length(jobs) == 0) stop("no job activated; flip at least one `if (T)`/`if (F)` above to `if (T)`")

message("run `source(\"~/scripts/r/post/post.r\")` ...")
source("~/scripts/r/post/post.r")
