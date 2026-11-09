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

force <- T # redo existing fout
mc_cores <- 4 # run jobs in parallel (shared login node -- stay moderate); jobs vary hugely in cost
cdo <- Sys.which("cdo")
cdo_nthreads <- 4 # shared login node -- stay moderate

work_home <- path.expand("~/work/data")
hostname <- Sys.info()["nodename"]
if (grepl("levante|^l[0-9]{5}", hostname)) { # levante; compute nodes: l12345
    stop("hostname \"", hostname, "\" unknown: set `pathpost` and `pathplot` for it")
} else if (grepl("albedo|^prod-[0-9]+$", hostname)) { # albedo; login nodes: albedo1/2; compute nodes: prod-123
    pathout <- "/albedo/work/user/cdanek/post" # shared with namelist.post_fesom_recom_clim.r's model-side output
} else {
    stop("hostname \"", hostname, "\" unknown: set `pathpost` and `pathplot` for it")
}


# standard WOA depth axis if depth dim vals are broken (e.g. glodap2016)
depths_woa33_m <- c(0, 10, 20, 30, 50, 75, 100, 125, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300,
                    1400, 1500, 1750, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500)

cdo_match_levels <- function(f, want) { # find nearest level
    cmd <- paste0(cdo, " -s showlevel ", f)
    lev <- trimws(strsplit(paste(system(cmd, intern=T), collapse=" "), "[[:space:]]+")[[1]])
    lev <- lev[nzchar(lev)]
    if (length(lev) == 0) stop("cdo_match_levels(\"", f, "\"): `", cmd, "` returned no levels -- zaxis not properly defined?")
    levn <- as.numeric(lev)
    if (anyNA(levn)) stop("cdo_match_levels(\"", f, "\"): `", cmd, "` returned non-numeric level value(s): ", paste(lev[is.na(levn)], collapse=", "))
    idx <- sort(unique(sapply(want, function(w) which.min(abs(levn - w)))))
    paste(lev[idx], collapse=",") # e.g. "0,50,100,200,500,1000"
}

#####################################################################

depths_wanted_m <- c(0, 50, 100, 200, 500, 1000) # m

jobs <- list()

# buitenhuis_etal_2013 -- vs FESOM/REcoM DiaC/PhyCalc/HetC/Zoo2C/Zoo3C (Phaeocystis has no REcoM PFT counterpart)
if (F) {
    setup <- "buitenhuis_etal_2013"
    fsin <- list.files(file.path(work_home, "buitenhuis_etal_2013/data"), pattern="\\.nc$", full.names=T) # MarEDat20120216Macrozooplankton.nc (nlon=360, nlat=180, nd=33, nt=12)
    if (length(fsin) == 0) stop("found zero files")
    vars <- "BIOMASS"
    levels_wanted <- cdo_match_levels(fsin[1], depths_wanted)
    modes <- list(fldmean  = list(mode="fldmean",  cdo_pre="-timmean -setgrid,r360x180"), # -setgrid for `gridToCurvilinear: Grid type >generic< unsupported`
                  timmean  = list(mode="timmean",  cdo_pre="-setgrid,r360x180"),
                  zonmean  = list(mode="zonmean",  cdo_pre="-timmean -setgrid,r360x180"))
    for (fi in seq_along(fsin)) {
        model <- tools::file_path_sans_ext(basename(fsin[fi])) # MarEDat20120716Diatoms
        for (vari in seq_along(vars)) {
            for (mode_name in names(modes)) {
                m <- modes[[mode_name]]
                levels <- if (any(mode_name == c("timmean", "zonmean"))) levels_wanted else NULL # fldmean(_ymon): every depth
                tmp <- list(setup=setup, model=model,
                            pathin=dirname(fsin[fi]), fpattern=basename(fsin[fi]),
                            varnamein=vars[vari],
                            mode=m$mode, mode_name=mode_name, mode_args=m$mode_args, cdo_pre=m$cdo_pre,
                            level=levels,
                            pathout=pathout)
                jobs <- c(jobs, list(tmp))
            } # for mode_name
        } # for vari
    } # for fi
} # if (F)

# de_boyer_montegut_etal_2023 -- vs FESOM/REcoM MLD3
if (F) {
    setup <- "de_boyer_montegut_etal_2023"
    indir <- file.path(work_home, "de_boyer_montegut_etal_2023/data/v2023")
    fpattern <- "mld_dr003_ref10m_v2023.nc"
    # input time dim vals: 0000-00-00 --> set to average of 1970-2021 clim
    settaxis <- format(mean(seq.POSIXt(from=as.POSIXct("1970-1-1", tz="UTC"), to=as.POSIXct("2021-12-31", tz="UTC"), length.out=2)), "%Y-%m-%d")
    settaxis <- paste0("-settaxis,", settaxis, ",1mon")
    modes <- list(timmean = list(mode="timmean", cdo_pre=settaxis),
                  ymonmean = list(mode="ymonmean", cdo_pre=settaxis),
                  zonmean = list(mode="zonmean", cdo_pre=paste0("-timmean ", settaxis)),
                  zonmean_ymon = list(mode="zonmean", cdo_pre=paste("-ymonmean", settaxis)))
    for (mode_name in names(modes)) {
        m <- modes[[mode_name]]
        tmp <- list(setup=setup,
                    pathin=indir, fpattern=fpattern,
                    varnamein="mld_dr003",
                    mode=m$mode, mode_name=mode_name, mode_args=m$mode_args, cdo_pre=m$cdo_pre,
                    pathout=pathout)
        jobs <- c(jobs, list(tmp))
    } # for mode_name
} # if (F)

# glodap -- vs FESOM/REcoM
if (T) {
    setup <- "glodap"
    indir <- file.path(work_home, "glodap/data/GLODAPv2.2016b")
    vars <- c("NO3", "PI_TCO2", "PO4", "TAlk", "oxygen", "silicate")
    fsin <- list.files(indir, pattern=paste0("^GLODAPv2\\.2016b\\.(", paste(vars, collapse="|"), ")*\\.nc$"), full.names=T) # GLODAPv2.2016b.PO4.nc (nlon=360, nlat=180, nd=33, nt=1)
    if (length(fsin) != length(vars)) stop("did not find all files")
    # input depth dim vals not set --> set zaxis:
    fzaxis <- list.files(indir, pattern="^GLODAPv2\\.2016b_zaxis_33.txt$", full.names=T)
    if (length(fzaxis) != 1) stop("found not a unique zaxis file")
    setzaxis <- paste0("-setzaxis,", fzaxis)
    levels_wanted <- match(depths_wanted_m, depths_woa33_m)
    if (anyNA(levels_wanted)) stop("did not find all `depths_wanted_m` = ", paste(depths_wanted_m, collapse=","))
    levels_wanted <- depths_woa33_m[levels_wanted]
    # input time dim vals: 0000-00-00 --> set to average of 1972-2013 clim:
    settaxis <- format(mean(seq.POSIXt(from=as.POSIXct("1972-1-1", tz="UTC"), to=as.POSIXct("2013-12-31", tz="UTC"), length.out=2)), "%Y-%m-%d")
    settaxis <- paste0("-settaxis,", settaxis)
    modes <- list(fldmean = list(mode="fldmean", cdo_pre=paste0(setzaxis, " ", settaxis)),
                  timmean = list(mode="timmean", cdo_pre=paste0(setzaxis, " ", settaxis)),
                  zonmean = list(mode="zonmean", cdo_pre=paste0(setzaxis, " ", settaxis)))
    for (fi in seq_along(fsin)) { # = vari
        for (mode_name in names(modes)) {
            m <- modes[[mode_name]]
            levels <- if (mode_name == "fldmean") NULL else levels_wanted
            tmp <- list(setup=setup,
                        pathin=indir, fpattern=basename(fsin[fi]),
                        varnamein=vars[fi],
                        mode=m$mode, mode_name=mode_name, mode_args=m$mode_args, cdo_pre=m$cdo_pre,
                        level=levels,
                        pathout=pathout)
            jobs <- c(jobs, list(tmp))
        } # for mode_name
    } # for fi
} # if (T)
jobs <- jobs[2]

# gregor_and_fay_2021 -- vs FESOM/REcoM CO2f (fgco2) and pCO2s (spco2); both 2D -- no fldmean. "zonmean"/
# "zonmean_ymon" reduce via time-/month-mean first (cdo right-to-left: that token sits left of `cdo_pre`,
# empty here -- no grid/metadata fix needed for this dataset)
if (F) {
    setup <- "gregor_and_fay_2021"
    indir <- file.path(work_home, "gregor_and_fay_2021/data")
    modes <- list(timmean = list(mode="timmean"), ymonmean = list(mode="ymonmean"),
                  zonmean = list(mode="zonmean", mode_args="zonal_1", cdo_pre="-timmean"),
                  zonmean_ymon = list(mode="zonmean", mode_args="zonal_1", cdo_pre="-ymonmean"))
    gregor_vars <- list(
        # masked ensemble-mean fgco2 (not the `unmasked/` copy); the exact same filename also exists under an
        # `unmasked/` subdirectory of `indir` -- post.r's recursive search would otherwise find both and refuse
        # as ambiguous, so `fpattern` gets a disambiguating path prefix (one level up, then the real subdirectory
        # name) instead of just the bare filename
        list(fpattern="v2021.04.03_5482547/SeaFlux_v2021.04_fgco2_1990-2019_ensmean_exclude_wind_NCEP.nc", varnamein="fgco2", varnameout="fgco2"),
        # same product family -- vs FESOM/REcoM pCO2s (surface pCO2; not the 3D, full-water-column `pCO2`). Real
        # monthly time series (456 steps, 1982-2019), no `unmasked/` duplicate for this one -> no disambiguation needed
        list(fpattern="v2021.04.03_5482547/SeaFlux_v2021.04_spco2_SOCOM_unfilled_ensmean_1982-2019.nc", varnamein="spco2", varnameout="pCO2s")
    )
    for (v in gregor_vars) {
        for (mode_name in names(modes)) {
            m <- modes[[mode_name]]
            tmp <- list(setup=setup,
                        pathin=indir, fpattern=v$fpattern,
                        varnamein=v$varnamein, varnameout=v$varnameout,
                        mode=m$mode, mode_name=mode_name, mode_args=m$mode_args, cdo_pre=m$cdo_pre,
                        pathout=pathout)
            jobs <- c(jobs, list(tmp))
        } # for mode_name
    } # for v
} # if (F)

# huang_etal_2022 -- vs FESOM/REcoM DFe. dFe_RF_annual is already the time-invariant annual mean (computed
# by this dataset's own raw.r) -> reuse it for "timmean"/"zonmean"/"fldmean" (mode="select", i.e. just
# select+level, no averaging -- there's no time dim left to average over) instead of re-averaging dFe_RF
# (the monthly variable, used for "ymonmean"/"zonmean_ymon"/"fldmean_ymon"); both variables live in the same file.
# `.../huang_etal_2022/data` is itself a symlink with nothing after it in the path -- plain `find` (no
# `-L`, used by post.r's find_input_files()) does not descend into a symlink that is the *leaf* of the
# given path, unlike one in the middle (e.g. other datasets' ".../data/v2023"), which the OS resolves
# transparently -- so point `pathin` at the real underlying directory instead
if (F) {
    setup <- "huang_etal_2022"
    indir <- file.path(work_home, "huang_etal_2022/data")
    fpattern <- "Monthly_dFe_Huang2021.nc"
    lev <- cdo_match_levels(file.path(indir, fpattern), depths_wanted) # own non-standard 31-level axis (shallowest = 11 m, not 0 m)
    modes <- list(fldmean = list(mode="fldmean", varnamein="dFe_RF_annual"), fldmean_ymon = list(mode="fldmean", cdo_pre="-ymonmean", varnamein="dFe_RF"),
                  timmean = list(mode="select", varnamein="dFe_RF_annual"), ymonmean = list(mode="ymonmean", varnamein="dFe_RF"),
                  zonmean = list(mode="zonmean", mode_args="zonal_1", varnamein="dFe_RF_annual"),
                  zonmean_ymon = list(mode="zonmean", mode_args="zonal_1", cdo_pre="-ymonmean", varnamein="dFe_RF"))
    for (mode_name in names(modes)) {
        m <- modes[[mode_name]]
        tmp <- list(setup=setup,
                    pathin=indir, fpattern=fpattern,
                    varnamein=m$varnamein, varnameout="dFe",
                    mode=m$mode, mode_name=mode_name, mode_args=m$mode_args, cdo_pre=m$cdo_pre,
                    level=if (any(mode_name == c("fldmean", "fldmean_ymon"))) NULL else lev,
                    pathout=pathout)
        jobs <- c(jobs, list(tmp))
    } # for mode_name
} # if (F)

# keppler_etal_2020 -- vs FESOM/REcoM DIC (2nd, independent DIC climatology besides glodap)
if (F) {
    setup <- "keppler_etal_2020"
    indir <- file.path(work_home, "keppler_etal_2020/data/v2020")
    fpattern <- "MOBO-DIC_MPIM_monthly_clim.nc"
    lev <- cdo_match_levels(file.path(indir, fpattern), depths_wanted)
    modes <- list(fldmean = list(mode="fldmean", cdo_pre="-timmean"), fldmean_ymon = list(mode="fldmean", cdo_pre="-ymonmean"),
                  timmean = list(mode="timmean"), ymonmean = list(mode="ymonmean"),
                  zonmean = list(mode="zonmean", mode_args="zonal_1", cdo_pre="-timmean"),
                  zonmean_ymon = list(mode="zonmean", mode_args="zonal_1", cdo_pre="-ymonmean"))
    for (mode_name in names(modes)) {
        m <- modes[[mode_name]]
        tmp <- list(setup=setup,
                    pathin=indir, fpattern=fpattern,
                    varnamein="DIC",
                    mode=m$mode, mode_name=mode_name, mode_args=m$mode_args, cdo_pre=m$cdo_pre,
                    level=if (any(mode_name == c("fldmean", "fldmean_ymon"))) NULL else lev,
                    pathout=pathout)
        jobs <- c(jobs, list(tmp))
    } # for mode_name
} # if (F)

# lewis_etal_2022 -- vs FESOM/REcoM DiaChl+PhyChl=Chl (Arctic; cmems below covers global/Arctic too).
# Formerly its own standalone raw/post.r pipeline (removed) -- migrated here. 2 independent sensors
# (A=MODIS/Aqua 2003-2021, S=SeaWiFS 1998-2007), kept separate (different periods/algorithms, not merged).
# The curvilinear Arctic grid has no cell bounds of its own -- `-setgrid,<griddes>` (create_genbounds.r's
# output, data/version1/griddes.nc) gives cdo real area weights for `zonmean,zonal_1` (same 1-deg bin width
# used everywhere else in both namelists, for direct cross-dataset/cross-model comparability; 1 deg is far
# coarser than this product's native ~4 km resolution, so every bin is well-populated).
if (F) {
    setup <- "lewis_etal_2022"
    indir <- file.path(work_home, "lewis_etal_2022/data/version1")
    setgrid_pre <- paste0("-setgrid,", file.path(indir, "griddes.nc"))
    modes <- list(timmean = list(mode="timmean", cdo_pre=setgrid_pre), ymonmean = list(mode="ymonmean", cdo_pre=setgrid_pre),
                  zonmean = list(mode="zonmean", mode_args="zonal_1", cdo_pre=paste("-timmean", setgrid_pre)),
                  zonmean_ymon = list(mode="zonmean", mode_args="zonal_1", cdo_pre=paste("-ymonmean", setgrid_pre)))
    lewis_vars <- list( # 2D (surface chl) -- no fldmean
        list(model="A_arctic_chlor_a", fpattern="A<year>_arctic.L3m_8D_CHL.AOReg.emp.nc", from=2003, to=2021),
        list(model="S_arctic_chlor_a", fpattern="S<year>_arctic.L3m_8D_CHL.AOReg.emp.nc", from=1998, to=2007)
    )
    for (v in lewis_vars) { # `model=` disambiguates A/S in post.r's auto `fout` (both share varnamein="chlor_a")
        for (mode_name in names(modes)) {
            m <- modes[[mode_name]]
            tmp <- list(setup=setup, model=v$model,
                        pathin=indir, fpattern=v$fpattern, from=v$from, to=v$to,
                        varnamein="chlor_a",
                        mode=m$mode, mode_name=mode_name, mode_args=m$mode_args, cdo_pre=m$cdo_pre,
                        pathout=pathout)
            jobs <- c(jobs, list(tmp))
        } # for mode_name
    } # for v
} # if (F)

# nsidc (v4.0) -- hemispheric scalar sea ice area/extent time series (grid has 1 point -> no spatial dim,
# so `zonmean` is neither possible nor meaningful). vs FESOM/REcoM a_ice: FESOM's own native
# siarean/siareas/siextentn/siextents outputs only cover 2605-2619, not the full 2510-2609 window used
# here -> use a_ice itself on the model side (reducing it to a comparable hemispheric scalar is left for
# later, as already noted there).
if (F) {
    setup <- "nsidc"
    indir <- file.path(work_home, "nsidc/data/G02135/data/v4.0")
    modes <- c("timmean", "ymonmean") # no zonmean: single-point grid, zonal reduction neither possible nor meaningful
    for (hem in c("N", "S")) {
        nsidc_vars <- list(
            list(fpattern=paste0(hem, "_seaice_area_monthly_v4.0.nc"), varnamein=paste0("siarea", tolower(hem))),
            list(fpattern=paste0(hem, "_seaice_extent_daily_v4.0.nc"), varnamein=paste0("siextent", tolower(hem)))
        )
        for (v in nsidc_vars) {
            for (mode_name in modes) {
                tmp <- list(setup=setup,
                            pathin=indir, fpattern=v$fpattern,
                            varnamein=v$varnamein,
                            mode=mode_name, mode_name=mode_name,
                            pathout=pathout)
                jobs <- c(jobs, list(tmp))
            } # for mode_name
        } # for v
    } # for hem
} # if (F)

# cmems -- ocean-colour products: total Chl (3 independent L4 products, global+Arctic) vs FESOM/REcoM Chl
# (DiaChl+PhyChl); primary production (PP) vs a newly-added NPP = NPPn+NPPd (see
# namelist.post_fesom_recom_clim.r). DIATO/MICRO/NANO/PICO (Chl fraction by size class/group) have no
# established py_f2recom counterpart -- REcoM only carries 2 phytoplankton PFTs (Dia=diatoms, Phy=the
# rest) -- provided as-is; how (or whether) to pair them is left to the user. Other variables in these
# products (uncertainty/error/count bands, `flags`, the other 5 PFT groups DINO/HAPTO/GREEN/PROKAR/
# PROCHLO) have no REcoM counterpart at all and are skipped.
if (F) {
    cmems_root <- file.path(work_home, "cmems/data")
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
    setup <- "cmems"
    for (p in cmems_vars) { # 2D (surface) -- no fldmean
        modes <- list(timmean = list(mode="timmean", cdo_pre=p$pre), ymonmean = list(mode="ymonmean", cdo_pre=p$pre),
                      zonmean = list(mode="zonmean", mode_args="zonal_1", cdo_pre=trimws(paste(c("-timmean", p$pre), collapse=" "))),
                      zonmean_ymon = list(mode="zonmean", mode_args="zonal_1", cdo_pre=trimws(paste(c("-ymonmean", p$pre), collapse=" "))))
        for (mode_name in names(modes)) {
            m <- modes[[mode_name]]
            tmp <- list(setup=setup,
                        pathin=file.path(cmems_root, p$sub), fpattern="*.nc",
                        varnamein=p$var, varnameout=p$name,
                        mode=m$mode, mode_name=mode_name, mode_args=m$mode_args, cdo_pre=m$cdo_pre,
                        pathout=pathout)
            jobs <- c(jobs, list(tmp))
        } # for mode_name
    } # for p
} # if (F)

# steele_etal_2001 -- vs FESOM/REcoM temp, salt. Land/missing cells are literal NaN with no
# _FillValue/missing_value declared -- cdo warns they are "not treated as missing values" and would
# silently corrupt zonmean/timmean without `-setmissval,nan`. phc3.0_annual.nc has no time dim (already
# the annual mean) -> reuse directly for "timmean"/"zonmean"/"fldmean" (mode="select"/no extra reduction
# prefix -- there's no time dim left to average over), as for huang's dFe_RF_annual; phc3.0_monthly.nc
# (has a time dim) is used for "ymonmean"/"zonmean_ymon"/"fldmean_ymon".
if (F) {
    setup <- "steele_etal_2001"
    indir <- file.path(work_home, "steele_etal_2001/data")
    fpat_tm <- "phc3.0_annual.nc"; fpat_mon <- "phc3.0_monthly.nc"
    lev <- cdo_match_levels(file.path(indir, fpat_tm), depths_wanted)
    missval_pre <- "-setmissval,nan"
    modes <- list(fldmean = list(mode="fldmean", fpattern=fpat_tm, cdo_pre=missval_pre),
                  fldmean_ymon = list(mode="fldmean", fpattern=fpat_mon, cdo_pre=paste("-ymonmean", missval_pre)),
                  timmean = list(mode="select", fpattern=fpat_tm, cdo_pre=missval_pre), ymonmean = list(mode="ymonmean", fpattern=fpat_mon, cdo_pre=missval_pre),
                  zonmean = list(mode="zonmean", fpattern=fpat_tm, mode_args="zonal_1", cdo_pre=missval_pre),
                  zonmean_ymon = list(mode="zonmean", fpattern=fpat_mon, mode_args="zonal_1", cdo_pre=paste("-ymonmean", missval_pre)))
    for (var in c("temp", "salt")) {
        for (mode_name in names(modes)) {
            m <- modes[[mode_name]]
            tmp <- list(setup=setup,
                        pathin=indir, fpattern=m$fpattern,
                        varnamein=var,
                        mode=m$mode, mode_name=mode_name, mode_args=m$mode_args, cdo_pre=m$cdo_pre,
                        level=if (any(mode_name == c("fldmean", "fldmean_ymon"))) NULL else lev,
                        pathout=pathout)
            jobs <- c(jobs, list(tmp))
        } # for mode_name
    } # for var
} # if (F)

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
if (F) {
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
        list(indir=file.path(work_home, "woa/2023/post"), prefix="woa23_6522_", code="t", name="temp"), # temp
        list(indir=file.path(work_home, "woa/2023/post"), prefix="woa23_6522_", code="s", name="salt"), # salt
        list(indir=file.path(work_home, "woa/2023/data"), prefix="woa23_all_",  code="n", name="DIN"),  # nitrate
        list(indir=file.path(work_home, "woa/2023/data"), prefix="woa23_all_",  code="i", name="DSi"),  # silicate
        list(indir=file.path(work_home, "woa/2023/data"), prefix="woa23_all_",  code="o", name="O2")    # dissolved oxygen
    )
    setup <- "woa"
    for (x in woa_vars) {
        f_monthly <- woa_assemble_monthly(x$indir, x$prefix, x$code)
        fpat_tm <- paste0(x$prefix, x$code, "00_01.nc"); fpat_mon <- basename(f_monthly)
        # the monthly climatology doesn't reach as deep as the annual mean (e.g. nitrate/silicate: 43 levels/
        # 800 m monthly vs. 102 levels/5500 m annual) -- `level`/`level_mon` computed separately from each
        # file's own (truncated or not) axis, or e.g. "Level >1000< not found!" on the shallower monthly one
        lev_tm <- cdo_match_levels(file.path(x$indir, fpat_tm), depths_wanted)
        lev_mon <- cdo_match_levels(f_monthly, depths_wanted)
        # "00" (annual) is already the time-invariant mean -> mode="select"/no extra reduction prefix for
        # timmean/zonmean/fldmean, same `reduce_tm=NULL` reasoning as huang_etal_2022/steele_etal_2001
        modes <- list(fldmean = list(mode="fldmean", fpattern=fpat_tm, level=NULL),
                      fldmean_ymon = list(mode="fldmean", fpattern=fpat_mon, level=NULL, cdo_pre="-ymonmean"),
                      timmean = list(mode="select", fpattern=fpat_tm, level=lev_tm), ymonmean = list(mode="ymonmean", fpattern=fpat_mon, level=lev_mon),
                      zonmean = list(mode="zonmean", fpattern=fpat_tm, level=lev_tm, mode_args="zonal_1"),
                      zonmean_ymon = list(mode="zonmean", fpattern=fpat_mon, level=lev_mon, mode_args="zonal_1", cdo_pre="-ymonmean"))
        for (mode_name in names(modes)) {
            m <- modes[[mode_name]]
            tmp <- list(setup=setup,
                        pathin=x$indir, fpattern=m$fpattern,
                        varnamein=paste0(x$code, "_an"), varnameout=x$name,
                        mode=m$mode, mode_name=mode_name, mode_args=m$mode_args, cdo_pre=m$cdo_pre,
                        level=m$level,
                        pathout=pathout)
            jobs <- c(jobs, list(tmp))
        } # for mode_name
    } # for x
} # if (F)

message("run `source(\"~/scripts/r/post/post.r\")` ...")
source("~/scripts/r/post/post.r")
