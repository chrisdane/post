# r

# Climatologies (timmean), seasonal cycles (ymonmean) and their zonal means (zonmean, after timmean and
# after ymonmean) of observational data products, for later comparison with
# namelist.post_fesom_recom_clim.r's FESOM/REcoM output (run separately; comparison itself is not done
# here). Oriented on py_f2recom's eval conventions (github.com/RECOM-Regulated-Ecosystem-Model/py_f2recom;
# see its `develop` branch's GlobalAssessment/*_Comparison_*.ipynb notebooks for the FESOM/REcoM <-> obs
# variable pairings used on the model side).
#
# Uses post.r's job engine. Most of these datasets are a single static climatology file (no year in the
# name at all) -- this needs post.r's year-less `fpattern` support (no "<year>"/"<from>"/"<to>" placeholder
# at all; added for this namelist). cmems's real per-month files are matched the same (year-less,
# wildcarded) way, since post.r's "<year>"+"<mon>" completeness check would otherwise require every
# calendar month to be present with no gaps.
#
# post.r's `-select,name=...` (not `-selname,...`) accepts multiple input files directly (confirmed: cdo
# rejects multiple ifiles under `-selname`/`-timmean` without an explicit multi-file operator, but not
# under `-select`) -- so no `mergetime` is needed here even for cmems's ~350-file-per-product series.
#
# sathyendranath_etal_2019 (OC-CCI) was dropped on request -- cmems now covers global/Arctic Chl instead.

rm(list=ls()); graphics.off()

force <- F # redo existing fout
mc_cores <- 4 # run jobs in parallel (shared login node -- stay moderate); jobs vary hugely in cost
cdo <- "/albedo/soft/sw/spack-sw/cdo/2.6.0-7vr4w3i/bin/cdo"
cdo_nthreads <- 4 # shared login node -- stay moderate
save_fout_an <- F # these are climatologies/seasonal cycles, not multi-year time series -- no annual mean to add
depths_want <- c(0, 50, 100, 200, 500, 1000) # m

datapool_home <- path.expand("~/work/data")
datapool_work <- "/albedo/work/user/cdanek/data"
pathout <- "/albedo/work/user/cdanek/post" # shared with namelist.post_fesom_recom_clim.r's model-side output

# nearest-level value string (as cdo prints them, for exact `level=` matching), from a file's own
# (CF-recognized) vertical axis
levelstr_nearest_from_file <- function(f, want) {
    lev <- trimws(strsplit(paste(system(paste0(cdo, " -s showlevel ", f), intern=T), collapse=" "), "[[:space:]]+")[[1]])
    lev <- lev[nzchar(lev)]
    idx <- sort(unique(sapply(want, function(w) which.min(abs(as.numeric(lev) - w)))))
    paste(lev[idx], collapse=",")
}
# standard WOA depth axis (buitenhuis/glodap/steele); glodap's `Depth` coordinate variable is mismatched
# with its `depth_surface` dimension name, so cdo only ever reports its generic level VALUES as 1:33 (not
# real depths) -- conveniently, that means the position in `woa33` *is* the `level=` value to ask for
woa33 <- c(0, 10, 20, 30, 50, 75, 100, 125, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300,
           1400, 1500, 1750, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500)
levelstr_woa33_genericidx <- function(want) paste(sort(unique(sapply(want, function(w) which.min(abs(woa33 - w))))), collapse=",")

# builds up to 6 post.r jobs (1 per mode) for 1 variable of 1 dataset. `tm`/`mon`: the fpattern/varnamein
# used for the timmean- and ymonmean-based outputs respectively (may point at different files, e.g.
# huang_etal_2022/steele_etal_2001 reuse an already-annual-mean file+variable for "timmean"/"zonmean"
# instead of re-averaging the monthly one -> pass `reduce_tm=NULL` in that case: `mode="select"` is then
# used instead of "timmean", i.e. just select+level, no averaging, since the input is already the mean).
# `level_mon`: [o] separate `level` for the ymonmean-based outputs, if the "mon" file's vertical axis
# doesn't reach as deep as the "tm" file's (e.g. woa's monthly climatologies are shallower than its
# annual one) -- defaults to `level` (same axis for both, the common case).
add_var_jobs <- function(dataset, name, pathin, level=NULL, level_mon=level,
                          fpat_tm, fpat_mon=fpat_tm,
                          varname_tm, varname_mon=varname_tm, varnameout=varname_tm,
                          pre_tm=NULL, pre_mon=NULL,
                          reduce_tm="timmean",
                          from=NULL, to=NULL, # only for real "<year>"-placeholder fpatterns (e.g. lewis_etal_2022)
                          modes=c("timmean", "ymonmean", "zonmean", "zonmean_ymon")) {
    mkjob <- function(mode_name, fpattern, varnamein, pre, mode, mode_args=NULL, level_ov=level) {
        list(pathin=pathin, fpattern=fpattern, varnamein=varnamein, varnameout=varnameout, level=level_ov,
             mode=mode, mode_name=mode_name, mode_args=mode_args, cdo_pre=if (is.null(pre) || !nzchar(trimws(pre))) NULL else pre,
             setup=dataset, experiment="obs", pathout=pathout, fout=paste0(name, "_", mode_name, ".nc"),
             from=from, to=to)
    }
    specs <- list(
        timmean      = mkjob("timmean",      fpat_tm,  varname_tm,  pre_tm,
                              mode=if (is.null(reduce_tm)) "select" else reduce_tm),
        ymonmean     = mkjob("ymonmean",     fpat_mon, varname_mon, pre_mon, mode="ymonmean", level_ov=level_mon),
        # cdo executes right-to-left, so within a bundled `cdo_pre` string the RIGHTMOST token runs FIRST --
        # `pre_tm`/`pre_mon` (e.g. setmissval, settaxis) must come last (rightmost) to run before the
        # reduction, which must come first (leftmost) to run on the already-fixed-up stream. `mode_args=
        # "zonal_1"` (same 1-deg bin width as namelist.post_fesom_recom_clim.r) is required for `zonmean` to
        # support a curvilinear grid at all (bare `zonmean` aborts: "Unsupported gridtype: curvilinear") and
        # keeps bin edges identical across every dataset/model output for direct comparability.
        zonmean      = mkjob("zonmean",      fpat_tm,  varname_tm,
                              trimws(paste(c(if (!is.null(reduce_tm)) paste0("-", reduce_tm), pre_tm), collapse=" ")), mode="zonmean", mode_args="zonal_1"),
        zonmean_ymon = mkjob("zonmean_ymon", fpat_mon, varname_mon,
                              trimws(paste(c("-ymonmean", pre_mon), collapse=" ")), mode="zonmean", mode_args="zonal_1", level_ov=level_mon),
        # `fldmean` of 3D data -> 1 global-mean value per depth, i.e. output length = ndepth (a vertical
        # profile, as in py_f2recom's *_profiles_python3.ipynb notebooks) -- `level_ov=NULL`: unlike the
        # other modes, use *every* available depth, not just the `depths_want`-nearest subset (there is no
        # reason to throw away vertical resolution in a profile plot). Needs the exact same grid care as
        # `zonmean` (reuses its `cdo_pre`) -- unlike `zonmean`, `fldmean` doesn't take a bin-width-style
        # `mode_args`, but on a grid cdo cannot recognize (e.g. buitenhuis_etal_2013's, fixed via
        # `pre_tm`/`pre_mon`'s `-setgrid`) it does not just warn, it hard-crashes under `--pedantic`
        # ("Grid cell bounds not available" -> abort) -- so the same `-setgrid` fix is required.
        fldmean      = mkjob("fldmean",      fpat_tm,  varname_tm,
                              trimws(paste(c(if (!is.null(reduce_tm)) paste0("-", reduce_tm), pre_tm), collapse=" ")), mode="fldmean", level_ov=NULL),
        fldmean_ymon = mkjob("fldmean_ymon", fpat_mon, varname_mon,
                              trimws(paste(c("-ymonmean", pre_mon), collapse=" ")), mode="fldmean", level_ov=NULL)
    )
    modes_want <- intersect(modes, names(specs))
    # `fldmean`/`fldmean_ymon` are automatic for 3D data (`level` given) -- added alongside their
    # `zonmean`/`zonmean_ymon` counterpart whenever that is requested, no separate opt-in needed
    if (!is.null(level)) {
        if ("zonmean" %in% modes_want) modes_want <- c(modes_want, "fldmean")
        if ("zonmean_ymon" %in% modes_want) modes_want <- c(modes_want, "fldmean_ymon")
    }
    unname(specs[modes_want])
}

#####################################################################################################

jobs <- list()

# buitenhuis_etal_2013 -- vs FESOM/REcoM DiaC/PhyCalc/HetC/Zoo2C/Zoo3C (Phaeocystis has no REcoM PFT counterpart)
indir <- file.path(datapool_home, "buitenhuis_etal_2013/data")
bh_files <- sort(list.files(indir, pattern="\\.nc$"))
lev <- levelstr_nearest_from_file(file.path(indir, bh_files[1]), depths_want) # own axis is CF-recognized (real depth values), unlike glodap's
# a real global 1x1-deg grid, but with non-standard coordinate var names ("LONGITUDE"/"LATITUDE") -- cdo
# only ever detects it as a bare-point "generic" grid, which `zonmean,zonal_1` cannot handle at all
# ("gridToCurvilinear: Grid type >generic< unsupported!") -- `-setgrid,r360x180` fixes the grid type.
for (f in bh_files) {
    name <- paste0(tools::file_path_sans_ext(f), "_BIOMASS") # e.g. MarEDat20120716Diatoms_BIOMASS
    jobs <- c(jobs, add_var_jobs("buitenhuis_etal_2013", name, pathin=indir, level=lev, fpat_tm=f, varname_tm="BIOMASS",
                                  pre_tm="-setgrid,r360x180", pre_mon="-setgrid,r360x180"))
}

# de_boyer_montegut_etal_2023 -- vs FESOM/REcoM MLD3
indir <- file.path(datapool_home, "de_boyer_montegut_etal_2023/data/v2023")
# `time:climatology` attribute is invalid CF -> cdo decodes every step as 0000-00-00; `timmean` is fine as
# a plain average either way, but `ymonmean`/zonmean-after-ymonmean need a clean synthetic monthly axis first
jobs <- c(jobs, add_var_jobs("de_boyer_montegut_etal_2023", "mld_dr003", pathin=indir, fpat_tm="mld_dr003_ref10m_v2023.nc",
                              varname_tm="mld_dr003", pre_mon="-settaxis,1950-01-15,00:00:00,1mon"))

# glodap -- vs FESOM/REcoM DIC (TCO2) and Alk (TAlk); other GLODAP variables processed too, for completeness.
# Static climatology (no real time axis) -> no seasonal-cycle equivalent (modes limited to timmean/zonmean).
indir <- file.path(datapool_home, "glodap/data/GLODAPv2.2016b")
lev <- levelstr_woa33_genericidx(depths_want)
for (f in sort(list.files(indir, pattern="^GLODAPv2\\.2016b\\..*\\.nc$"))) {
    var <- sub("^GLODAPv2\\.2016b\\.", "", sub("\\.nc$", "", f))
    jobs <- c(jobs, add_var_jobs("glodap", paste0("GLODAPv2.2016b.", var), pathin=indir, level=lev, fpat_tm=f, varname_tm=var,
                                  modes=c("timmean", "zonmean")))
}

# gregor_and_fay_2021 -- vs FESOM/REcoM CO2f; masked ensemble-mean fgco2 (not the `unmasked/` copy).
# The exact same filename also exists under an `unmasked/` subdirectory of `indir` -- post.r's recursive
# search would otherwise find both and refuse as ambiguous, so give `fpattern` a disambiguating path
# prefix (one level up, then the real subdirectory name) instead of just the bare filename. And since
# that puts `indir` back at the leaf-symlink level (same issue as huang_etal_2022 above), use the real dir.
indir <- file.path(datapool_work, "gregor_and_fay_2021/data")
jobs <- c(jobs, add_var_jobs("gregor_and_fay_2021", "SeaFlux_v2021.04_fgco2_1990-2019", pathin=indir,
                              fpat_tm="v2021.04.03_5482547/SeaFlux_v2021.04_fgco2_1990-2019_ensmean_exclude_wind_NCEP.nc", varname_tm="fgco2"))
# same product family -- vs FESOM/REcoM pCO2s (surface pCO2; not the 3D, full-water-column `pCO2`). Real
# monthly time series (456 steps, 1982-2019), no `unmasked/` duplicate for this one -> no disambiguation needed.
jobs <- c(jobs, add_var_jobs("gregor_and_fay_2021", "SeaFlux_v2021.04_spco2_1982-2019", pathin=indir,
                              fpat_tm="v2021.04.03_5482547/SeaFlux_v2021.04_spco2_SOCOM_unfilled_ensmean_1982-2019.nc",
                              varname_tm="spco2", varnameout="pCO2s"))

# huang_etal_2022 -- vs FESOM/REcoM DFe. dFe_RF_annual is already the time-invariant annual mean (computed
# by this dataset's own raw.r) -> reuse it for "timmean"/"zonmean" (reduce_tm=NULL, mode="select") instead
# of re-averaging dFe_RF (used for "ymonmean"/"zonmean_ymon").
# `.../huang_etal_2022/data` is itself a symlink with nothing after it in the path -- plain `find` (no
# `-L`, used by post.r's find_input_files()) does not descend into a symlink that is the *leaf* of the
# given path, unlike one in the middle (e.g. other datasets' ".../data/v2023"), which the OS resolves
# transparently -- so point `pathin` at the real underlying directory instead
indir <- file.path(datapool_work, "huang_etal_2022/data")
lev <- levelstr_nearest_from_file(file.path(indir, "Monthly_dFe_Huang2021.nc"), depths_want) # own non-standard 31-level axis (shallowest = 11 m, not 0 m)
jobs <- c(jobs, add_var_jobs("huang_etal_2022", "Monthly_dFe_Huang2021", pathin=indir, level=lev,
                              fpat_tm="Monthly_dFe_Huang2021.nc", varname_tm="dFe_RF_annual", varname_mon="dFe_RF",
                              varnameout="dFe", reduce_tm=NULL))

# keppler_etal_2020 -- vs FESOM/REcoM DIC (2nd, independent DIC climatology besides glodap)
indir <- file.path(datapool_home, "keppler_etal_2020/data/v2020")
lev <- levelstr_nearest_from_file(file.path(indir, "MOBO-DIC_MPIM_monthly_clim.nc"), depths_want)
jobs <- c(jobs, add_var_jobs("keppler_etal_2020", "MOBO-DIC_MPIM", pathin=indir, level=lev,
                              fpat_tm="MOBO-DIC_MPIM_monthly_clim.nc", varname_tm="DIC"))

# lewis_etal_2022 -- vs FESOM/REcoM DiaChl+PhyChl=Chl (Arctic; cmems below covers global/Arctic too).
# Formerly its own standalone raw/post.r pipeline (removed) -- migrated here. 2 independent sensors
# (A=MODIS/Aqua 2003-2021, S=SeaWiFS 1998-2007), kept separate (different periods/algorithms, not merged).
# The curvilinear Arctic grid has no cell bounds of its own -- `-setgrid,<griddes>` (create_genbounds.r's
# output, data/version1/griddes.nc) gives cdo real area weights for `zonmean,zonal_1` (same 1-deg bin width
# used everywhere else in both namelists, for direct cross-dataset/cross-model comparability; 1 deg is far
# coarser than this product's native ~4 km resolution, so every bin is well-populated).
indir <- file.path(datapool_home, "lewis_etal_2022/data/version1")
setgrid_pre <- paste0("-setgrid,", file.path(indir, "griddes.nc"))
jobs <- c(jobs, add_var_jobs("lewis_etal_2022", "A_arctic_chlor_a", pathin=indir, fpat_tm="A<year>_arctic.L3m_8D_CHL.AOReg.emp.nc",
                              varname_tm="chlor_a", pre_tm=setgrid_pre, pre_mon=setgrid_pre, from=2003, to=2021))
jobs <- c(jobs, add_var_jobs("lewis_etal_2022", "S_arctic_chlor_a", pathin=indir, fpat_tm="S<year>_arctic.L3m_8D_CHL.AOReg.emp.nc",
                              varname_tm="chlor_a", pre_tm=setgrid_pre, pre_mon=setgrid_pre, from=1998, to=2007))

# nsidc (v4.0) -- hemispheric scalar sea ice area/extent time series (grid has 1 point -> no spatial dim,
# so `zonmean` is neither possible nor meaningful). vs FESOM/REcoM a_ice: FESOM's own native
# siarean/siareas/siextentn/siextents outputs only cover 2605-2619, not the full 2510-2609 window used
# here -> use a_ice itself on the model side (reducing it to a comparable hemispheric scalar is left for
# later, as already noted there).
indir <- file.path(datapool_home, "nsidc/data/G02135/data/v4.0")
for (hem in c("N", "S")) {
    jobs <- c(jobs, add_var_jobs("nsidc", paste0(hem, "_seaice_area_monthly"), pathin=indir,
                                  fpat_tm=paste0(hem, "_seaice_area_monthly_v4.0.nc"), varname_tm=paste0("siarea", tolower(hem)),
                                  modes=c("timmean", "ymonmean")))
    jobs <- c(jobs, add_var_jobs("nsidc", paste0(hem, "_seaice_extent_daily"), pathin=indir,
                                  fpat_tm=paste0(hem, "_seaice_extent_daily_v4.0.nc"), varname_tm=paste0("siextent", tolower(hem)),
                                  modes=c("timmean", "ymonmean")))
}

# cmems -- ocean-colour products: total Chl (3 independent L4 products, global+Arctic) vs FESOM/REcoM Chl
# (DiaChl+PhyChl); primary production (PP) vs a newly-added NPP = NPPn+NPPd (see
# namelist.post_fesom_recom_clim.r). DIATO/MICRO/NANO/PICO (Chl fraction by size class/group) have no
# established py_f2recom counterpart -- REcoM only carries 2 phytoplankton PFTs (Dia=diatoms, Phy=the
# rest) -- provided as-is; how (or whether) to pair them is left to the user. Other variables in these
# products (uncertainty/error/count bands, `flags`, the other 5 PFT groups DINO/HAPTO/GREEN/PROKAR/
# PROCHLO) have no REcoM counterpart at all and are skipped.
cmems_root <- file.path(datapool_work, "cmems/data")
# ARC_009_124's curvilinear grid has no cell bounds of its own -- `zonmean,zonal_1` doesn't abort on it
# (unlike lewis_etal_2022 pre-fix), it silently returns all-NaN (confirmed via `cdo info`) without one.
# Unlike lewis, a griddes (with bounds already computed, `xbounds`/`ybounds` present) ships right alongside
# the raw data -- `-setgrid,<that file>` fixes it the same way as lewis' create_genbounds.r output.
arc_griddes <- file.path(cmems_root, "OCEANCOLOUR_ARC_BGC_L4_MY_009_124/cmems_obs-oc_arc_bgc-plankton_my_l4-multi-4km_P1M",
                          "griddes_cmems_obs-oc_arc_bgc-plankton_my_l4-multi-4km_P1M.txt")
cmems_vars <- list(
    list(name="ARC_009_124_CHL",   sub="OCEANCOLOUR_ARC_BGC_L4_MY_009_124/cmems_obs-oc_arc_bgc-plankton_my_l4-multi-4km_P1M", var="CHL",
         pre=paste0("-setgrid,", arc_griddes)),
    list(name="GLO_009_104_CHL",   sub="OCEANCOLOUR_GLO_BGC_L4_MY_009_104/cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M", var="CHL"),
    list(name="GLO_009_104_DIATO", sub="OCEANCOLOUR_GLO_BGC_L4_MY_009_104/cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M", var="DIATO"),
    list(name="GLO_009_104_MICRO", sub="OCEANCOLOUR_GLO_BGC_L4_MY_009_104/cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M", var="MICRO"),
    list(name="GLO_009_104_NANO",  sub="OCEANCOLOUR_GLO_BGC_L4_MY_009_104/cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M", var="NANO"),
    list(name="GLO_009_104_PICO",  sub="OCEANCOLOUR_GLO_BGC_L4_MY_009_104/cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M", var="PICO"),
    list(name="GLO_009_104_PP",    sub="OCEANCOLOUR_GLO_BGC_L4_MY_009_104/cmems_obs-oc_glo_bgc-pp_my_l4-multi-4km_P1M",       var="PP"),
    list(name="GLO_009_108_CHL",   sub="OCEANCOLOUR_GLO_BGC_L4_MY_009_108/c3s_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M",   var="CHL")
)
for (p in cmems_vars) {
    jobs <- c(jobs, add_var_jobs("cmems", p$name, pathin=file.path(cmems_root, p$sub), fpat_tm="*.nc", varname_tm=p$var, varnameout=p$name,
                                  pre_tm=p$pre, pre_mon=p$pre))
}

# steele_etal_2001 -- vs FESOM/REcoM temp, salt. Land/missing cells are literal NaN with no
# _FillValue/missing_value declared -- cdo warns they are "not treated as missing values" and would
# silently corrupt zonmean/timmean without `-setmissval,nan`. phc3.0_annual.nc has no time dim (already
# the annual mean) -> reuse directly for "timmean"/"zonmean" (reduce_tm=NULL), as for huang's dFe_RF_annual.
indir <- file.path(datapool_home, "steele_etal_2001/data")
lev <- levelstr_nearest_from_file(file.path(indir, "phc3.0_annual.nc"), depths_want)
for (var in c("temp", "salt")) {
    jobs <- c(jobs, add_var_jobs("steele_etal_2001", paste0("phc3.0_", var), pathin=indir, level=lev,
                                  fpat_tm="phc3.0_annual.nc", fpat_mon="phc3.0_monthly.nc", varname_tm=var,
                                  pre_tm="-setmissval,nan", pre_mon="-setmissval,nan", reduce_tm=NULL))
}

# woa (2023) -- vs FESOM/REcoM temp/salt (a 2nd, independent climatology besides steele_etal_2001/PHC3.0,
# matching py_f2recom's separate Temp_WOA_Comparison/Sal_WOA_Comparison notebooks, distinct from its
# TS_PHC3_Comparison) and DIN/DSi/O2 (entirely missing from py_f2recom's DIN_WOA/DSi_WOA/DO2_WOA_Comparison
# notebooks otherwise). data/woa/2023/post/ ("decav" product, "t"/"s") and data/woa/2023/data/ ("all"
# product, the rest) cover the same period/stats (annual+monthly+seasonal; "an" = objectively analyzed
# mean) -- just prepared into 2 different subdirectories beforehand. p (phosphate) and O (percent O2
# saturation) have no REcoM counterpart and are skipped.
#
# WOA ships 1 file per period (00=annual, 01-12=months, 13-16=seasons) rather than 1 multi-step file --
# "00" is already the annual mean (like huang_etal_2022/steele_etal_2001's reuse of an annual file,
# reduce_tm=NULL), but "01".."12" cannot be matched by a single wildcard `fpattern` (post.r only supports
# "*", no "[0-9]"-style ranges) without also sweeping up "00" (a different, irrelevant year) and "13".."16"
# (seasonal means that collide exactly in their assigned date with 4 of the 12 real months -- e.g. "13"
# and "02" are both dated 1969-02-13 -- silently corrupting a `ymonmean`-based reduction). So the 12 real
# month files are assembled once here, directly by filename (no globbing), into their own static file --
# living alongside the raw period files, since every other job here uses an exact static filename (no
# wildcard that could sweep it up in turn). Both `.../woa/2023/post` and `.../data` are leaf symlinks --
# same issue as huang_etal_2022/gregor_and_fay_2021 above -- so use the real underlying directories.
woa_assemble_monthly <- function(indir, prefix, code) { # e.g. prefix="woa23_6522_" (post/) or "woa23_all_" (data/)
    f_monthly <- file.path(indir, paste0(prefix, code, "_monthly12.nc"))
    if (!file.exists(f_monthly)) {
        f_months <- file.path(indir, paste0(prefix, code, sprintf("%02d", 1:12), "_01.nc"))
        missing <- f_months[!file.exists(f_months)]
        if (length(missing) > 0) stop("missing woa month file(s): ", paste(missing, collapse=", "))
        cmd <- paste0(cdo, " -O -s -select,name=", code, "_an ", paste(f_months, collapse=" "), " ", f_monthly)
        message("run `", cmd, "` ...")
        if (system(cmd) != 0) stop("cdo failed")
    }
    f_monthly
}

woa_vars <- list(
    list(indir=file.path(datapool_work, "woa/2023/post"), prefix="woa23_6522_", code="t", name="temp"), # temp
    list(indir=file.path(datapool_work, "woa/2023/post"), prefix="woa23_6522_", code="s", name="salt"), # salt
    list(indir=file.path(datapool_work, "woa/2023/data"), prefix="woa23_all_",  code="n", name="DIN"),  # nitrate
    list(indir=file.path(datapool_work, "woa/2023/data"), prefix="woa23_all_",  code="i", name="DSi"),  # silicate
    list(indir=file.path(datapool_work, "woa/2023/data"), prefix="woa23_all_",  code="o", name="O2")    # dissolved oxygen
)
for (x in woa_vars) {
    f_monthly <- woa_assemble_monthly(x$indir, x$prefix, x$code)
    # the monthly climatology doesn't reach as deep as the annual mean (e.g. nitrate/silicate: 43 levels/
    # 800 m monthly vs. 102 levels/5500 m annual) -- `level`/`level_mon` computed separately from each
    # file's own (truncated or not) axis, or e.g. "Level >1000< not found!" on the shallower monthly one
    lev_tm <- levelstr_nearest_from_file(file.path(x$indir, paste0(x$prefix, x$code, "00_01.nc")), depths_want)
    lev_mon <- levelstr_nearest_from_file(f_monthly, depths_want)
    jobs <- c(jobs, add_var_jobs("woa", paste0(x$prefix, x$name), pathin=x$indir, level=lev_tm, level_mon=lev_mon,
                                  fpat_tm=paste0(x$prefix, x$code, "00_01.nc"), fpat_mon=basename(f_monthly),
                                  varname_tm=paste0(x$code, "_an"), varnameout=x$name, reduce_tm=NULL))
}

message("run `source(\"~/scripts/r/post/post.r\")` ...")
source("~/scripts/r/post/post.r")
