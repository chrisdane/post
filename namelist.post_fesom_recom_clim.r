# r

# Climatologies (timmean), seasonal cycles (ymonmean) and their zonal means (zonmean, after timmean and
# after ymonmean) of FESOM/REcoM variables that have a counterpart observational data product, for later
# comparison with namelist.post_data_oce.r's output (run separately; comparison itself is not
# done here). Variable choice/counterparts oriented on py_f2recom's eval conventions
# (github.com/RECOM-Regulated-Ecosystem-Model/py_f2recom, see esp. its `develop` branch's
# GlobalAssessment/*_Comparison_*.ipynb notebooks).
#
# Uses the last 100 years of AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230 (2510-2609). 3D variables are
# reduced to the levels nearest to `depths_want`, matching the depths selected on the data side.

mc_cores <- 8 # run jobs in parallel (shared login node -- stay moderate)
cdo_nthreads <- 2

setup <- "AWI-ESM-3-REcoM"
experiment <- "AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230"
from <- 2510
to <- 2609
pathin_fesom <- paste0("/albedo/work/projects/p_awiesm3_cmip7/lahajd001/runtime/awiesm3-v3.5.1-cc/", experiment, "/outdata/fesom")
pathout <- "/albedo/work/user/cdanek/post"

depths_want <- c(0, 50, 100, 200, 500, 1000) # m; nearest available FESOM level kept for each

# nearest-level values (as cdo prints them, for exact `-select,...,level=` matching) in `f`'s vertical axis
level_str_nearest <- function(f, want) {
    lev <- trimws(strsplit(paste(system(paste0("cdo -s showlevel ", f), intern=T), collapse=" "), "[[:space:]]+")[[1]])
    lev <- lev[nzchar(lev)]
    levn <- as.numeric(lev)
    idx <- sort(unique(sapply(want, function(w) which.min(abs(levn - w)))))
    paste(lev[idx], collapse=",")
}

# all REcoM 3D tracers + temp/salt share the same 47-level vertical axis -> compute the level string once
level_str_3d <- level_str_nearest(paste0(pathin_fesom, "/DIC.fesom.", from, ".nc"), depths_want)
message("3D level selection (nearest to ", paste(depths_want, collapse=","), " m): ", level_str_3d)

# vars: v=FESOM varname, is3d=has a depth dim (level selection applied); counterpart noted in comments
vars <- list(
    list(v="DIC",     is3d=T),  # vs glodap TCO2, keppler_etal_2020 MOBO-DIC
    list(v="Alk",     is3d=T),  # vs glodap TAlk
    list(v="DFe",     is3d=T),  # vs huang_etal_2022
    list(v="DiaC",    is3d=T),  # vs buitenhuis_etal_2013 Diatoms
    list(v="PhyCalc", is3d=T),  # vs buitenhuis_etal_2013 Coccolithophores (calcite proxy)
    list(v="HetC",    is3d=T),  # vs buitenhuis_etal_2013 Microzooplankton
    list(v="Zoo2C",   is3d=T),  # vs buitenhuis_etal_2013 Mesozooplankton
    list(v="Zoo3C",   is3d=T),  # vs buitenhuis_etal_2013 Macrozooplankton
    list(v="DiaChl",  is3d=T),  # + PhyChl summed below = total Chl, vs lewis_etal_2022 (Arctic)/sathyendranath_etal_2019/cmems (global)
    list(v="PhyChl",  is3d=T),
    list(v="temp",    is3d=T),  # vs steele_etal_2001 (PHC3.0), woa (WOA2023, independent)
    list(v="salt",    is3d=T),  # vs steele_etal_2001 (PHC3.0), woa (WOA2023, independent)
    list(v="DIN",     is3d=T),  # vs woa (WOA2023 nitrate)
    list(v="DSi",     is3d=T),  # vs woa (WOA2023 silicate)
    list(v="O2",      is3d=T),  # vs woa (WOA2023 dissolved oxygen)
    list(v="MLD3",    is3d=F),  # vs de_boyer_montegut_etal_2023 (mld_dr003 density criterion)
    list(v="CO2f",    is3d=F),  # vs gregor_and_fay_2021 (SeaFlux fgco2), cmems
    list(v="pCO2s",   is3d=F),  # vs gregor_and_fay_2021 (SeaFlux spco2); NOT the 3D, full-water-column `pCO2`
    list(v="a_ice",   is3d=F),  # vs nsidc (raw concentration here; an ice_ext/ice_area-style area-weighted
                                 # reduction, as in pyfesom2.ice_ext()/ice_area(), is left for later)
    list(v="NPPd",    is3d=F),  # + NPPn summed below = NPP, vs cmems (OCEANCOLOUR_*_BGC_L4_MY_*-pp PP)
    list(v="NPPn",    is3d=F)
)

fpattern_of <- function(v) paste0(v, ".fesom.<year>.nc")

jobs <- list()
for (x in vars) {
    level <- if (x$is3d) level_str_3d else ""
    base <- list(setup=setup, model="recom", experiment=experiment, pathin=pathin_fesom, from=from, to=to,
                 fpattern=fpattern_of(x$v), varnamein=x$v, dtin="mon", level=level, pathout=pathout)
    jobs[[length(jobs)+1]] <- c(base, list(mode="timmean"))
    jobs[[length(jobs)+1]] <- c(base, list(mode="ymonmean"))
    jobs[[length(jobs)+1]] <- c(base, list(mode="zonmean", mode_name="zonmean",      mode_args="zonal_1", cdo_pre="-timmean"))
    jobs[[length(jobs)+1]] <- c(base, list(mode="zonmean", mode_name="zonmean_ymon", mode_args="zonal_1", cdo_pre="-ymonmean"))
    if (x$is3d) { # `fldmean` of 3D data -> a vertical profile (output length = ndepth); matches the data
                  # side's `fldmean`/`fldmean_ymon` (namelist.post_data_oce.r) for direct
                  # model-obs profile comparison -- use *every* available depth, not just `level_str_3d`'s
                  # `depths_want`-nearest subset (no reason to throw away vertical resolution in a profile)
        base_alldepth <- c(base[setdiff(names(base), "level")], list(level=""))
        jobs[[length(jobs)+1]] <- c(base_alldepth, list(mode="fldmean", mode_name="fldmean",      cdo_pre="-timmean"))
        jobs[[length(jobs)+1]] <- c(base_alldepth, list(mode="fldmean", mode_name="fldmean_ymon", cdo_pre="-ymonmean"))
    }
}

message("run `source(\"~/scripts/r/post/post.r\")` ...")
source("~/scripts/r/post/post.r")

#####################################################################################################
# derived sums: REcoM splits some quantities that obs products report as 1 total --
# - Chl = DiaChl + PhyChl (REcoM splits chlorophyll by PFT; data products report total Chl)
# - NPP = NPPd + NPPn (REcoM splits net primary production into day/night; cmems PP is a daily total)

derived_sums <- list(list(out="Chl", a="DiaChl", b="PhyChl"), list(out="NPP", a="NPPd", b="NPPn"))
for (ds in derived_sums) {
    for (mode_name in c("timmean", "ymonmean", "zonmean", "zonmean_ymon", "fldmean", "fldmean_ymon")) { # NPP's
                        # inputs (NPPd/NPPn) are 2D -- no fldmean -- "skip" messages below for those are expected
        fa <- Sys.glob(file.path(pathout, setup, mode_name, ds$a, paste0(ds$a, "_*_", mode_name, "_*_", from, "-", to, ".nc")))
        fb <- Sys.glob(file.path(pathout, setup, mode_name, ds$b, paste0(ds$b, "_*_", mode_name, "_*_", from, "-", to, ".nc")))
        if (length(fa) != 1 || length(fb) != 1) {
            message("skip ", ds$out, " = ", ds$a, " + ", ds$b, " sum for \"", mode_name, "\": found ", length(fa), " ", ds$a,
                     " and ", length(fb), " ", ds$b, " match(es) (expected 1 each)")
            next
        }
        fout <- file.path(pathout, setup, mode_name, ds$out, paste0(ds$out, "_mon_", mode_name, "_", setup, "_recom_", experiment, "_r1i1p1f1_gn_", from, "-", to, ".nc"))
        if (!file.exists(fout)) {
            dir.create(dirname(fout), recursive=T, showWarnings=F)
            cmd <- paste0("cdo -O -s -setname,", ds$out, " -add ", fa, " ", fb, " ", fout)
            message("run `", cmd, "` ...")
            if (system(cmd) != 0) stop("cdo failed")
            message("--> done: \"", fout, "\"")
        }
    }
}
