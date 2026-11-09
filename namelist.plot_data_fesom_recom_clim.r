# r

# Side-by-side comparison of namelist.post_data_oce.r's (obs) and
# namelist.post_fesom_recom_clim.r's (FESOM/REcoM) output, per the variable pairing established
# throughout both of those namelists' own comments (`pairs` below). For standalone (no-comparison) plots
# of just the obs side, see namelist.data_clim.r.
#
# - maps (`timmean`): obs on the left panel, FESOM/REcoM on the right, remapped (`cdo remapcon`) onto the
#   obs file's own grid -- FESOM's native output is on its own unstructured mesh, not directly
#   comparable/plottable the way a regular lon-lat grid is. 1 map per depth in `depths_want` for a 3D
#   variable (nearest-matched independently on each side, as elsewhere in this project), skipping an
#   all-missing depth on the obs side.
# - 1-dim reductions (`zonmean` of a 2D variable; `fldmean` of a 3D one, `mode` = "fldmean"): all obs of the
#   model variable + model overlaid on 1 line/profile plot.
# - 2-dim reductions (`zonmean` of a 3D variable: lat-depth section; `zonmean_ymon` of a 2D one:
#   lat-month; `fldmean_ymon` of a 3D one: depth-month): obs left panel, model right panel -- no remap
#   needed here, both sides are already reduced to the same kind of axes (lat/depth/month bins).
# - `zonmean_ymon` of a 3D variable (3 remaining dims: lat, depth, month) is skipped (plot.r draws 1 or 2).
# - all 2-dim plots also get an anomaly plot "model minus data" ("_anom"; plot.r's `anomaly`), with the data in the units of the model (`fac` of `pairs`).
#
# Datasets/variables with no established FESOM/REcoM counterpart are left out of `pairs` entirely (not
# silently skipped at plot time): nsidc (scalar hemispheric time series, not comparable to a_ice's
# spatial field without first reducing it the same way -- "left for later", see
# namelist.post_fesom_recom_clim.r); cmems's DIATO/MICRO/NANO/PICO and glodap's
# Cant/OmegaA/OmegaC/PI_TCO2/PO4/pHts25p0/pHtsinsitutp (no REcoM counterpart tracked here).

force <- F
library(ncdf4)

hostname <- Sys.info()["nodename"]
if (grepl("levante|^l[0-9]{5}", hostname)) {
    pathpost <- c("/work/ab1095/a270073/post", "/work/ba1103/a270073/post")
    pathplot <- "/work/ab1095/a270073/plots"
} else if (grepl("albedo|^prod-[0-9]+$", hostname)) {
    pathpost <- "/albedo/work/user/cdanek/post"
    pathplot <- "/albedo/work/user/cdanek/plots"
} else {
    stop("hostname \"", hostname, "\" unknown: set `pathpost` and `pathplot` for it")
}

cdo <- "/albedo/soft/sw/spack-sw/cdo/2.6.0-7vr4w3i/bin/cdo"
fesom_mesh_griddes <- "/albedo/work/user/cdanek/mesh/fesom2/core3/griddes_core3_node.nc"

# as in namelist.post_fesom_recom_clim.r
setup <- "AWI-ESM-3-REcoM"
experiment <- "AWI-ESM3-CC-LR-CMIP7-Spinup_v351_from2230"
from <- 2510
to <- 2609
depths_want <- c(0, 50, 100, 200, 500, 1000) # m; same convention used throughout this project

known_depth_dimnames <- c("depth", "Depth", "DEPTH", "depth_surface", "nz", "lev", "level", "Level", "LEVEL") # as in plot.r

# 3D vars' zonmean/fldmean/timmean filenames embed the `level` string, which differs per variable and
# isn't reproduced here -- `*` in its place (2D vars have no such segment at all, which "*" also matches)
fin_fesom <- function(var, mode_name) {
    pattern <- file.path(pathpost, setup, mode_name, var,
                          paste0(var, "_mon_", mode_name, "_*", setup, "_recom_", experiment, "_r1i1p1f1_gn_", from, "-", to, ".nc"))
    hits <- Sys.glob(pattern)
    if (length(hits) != 1) {
        message("fin_fesom(\"", var, "\", \"", mode_name, "\"): expected 1 match for \"", pattern, "\", got ", length(hits))
        return(NULL)
    }
    hits
}

# `f`'s 1st (real) variable's dims (name, length, values)
nc_var_dims <- function(f) {
    varname <- trimws(strsplit(trimws(system(paste0(cdo, " -s showname ", f), intern=T)), "[[:space:]]+")[[1]][1])
    nc <- nc_open(f)
    on.exit(nc_close(nc))
    if (is.null(nc$var[[varname]])) stop("nc_var_dims(\"", f, "\"): variable \"", varname, "\" not found")
    dims <- nc$var[[varname]]$dim
    list(varname=varname, names=sapply(dims, "[[", "name"), lens=sapply(dims, "[[", "len"), vals=lapply(dims, "[[", "vals"))
}
nearest_idx <- function(vals, want) which.min(abs(vals - want))

# `varname`'s units attribute in `f`; used to build a map's zlab from the FESOM variable alone (e.g.
# "salt [psu]"), not `data_label()`'s default of the *obs* dataset's label (e.g. "glodap salinity [...]")
var_units <- function(f, varname) {
    nc <- nc_open(f); on.exit(nc_close(nc))
    att <- ncatt_get(nc, varname, "units")
    if (att$hasatt) att$value else ""
}
map_zlab <- function(p, model_f) {
    units <- var_units(model_f, nc_var_dims(model_f)$varname)
    units <- sub("^\\[(.*)\\]$", "\\1", units) # some FESOM/REcoM vars store units already bracketed, e.g. "[mmol/m3]"
    if (nzchar(units)) paste0(p$fesom_var, " [", units, "]") else p$fesom_var
}

# remap FESOM/REcoM's `var` timmean map onto `target_f`'s own grid -- at the depth nearest `depth_want`
# (3D variable; `target_f`'s own nearest depth is picked by the caller) or the whole field (2D variable,
# `depth_want` = NULL). cdo accepts a plain data file in place of a griddes to define the target grid.
# Cached under `remap_dir` by variable + depth + target file (so re-used across a target with >1 own file,
# e.g. lewis_etal_2022's 2 sensors).
remap_fesom_map <- function(var, target_f, depth_want=NULL) {
    fesom_f <- fin_fesom(var, "timmean")
    if (is.null(fesom_f)) return(NULL)
    dd <- nc_var_dims(fesom_f)
    survive <- dd$lens > 1
    depth_i <- which(survive & !is.na(match(dd$names, known_depth_dimnames)))
    if (length(depth_i) == 1) {
        # e.g. FESOM's "Chl" (DiaChl+PhyChl) has a depth dim even though the paired obs product
        # (cmems/lewis_etal_2022 surface ocean-colour chlorophyll) is 2D -- compare against the surface
        if (is.null(depth_want)) depth_want <- 0
        k <- nearest_idx(dd$vals[[depth_i]], depth_want)
        sellev <- paste0("-sellevidx,", k)
        tag <- paste0(dd$names[depth_i], round(dd$vals[[depth_i]][k], 1))
    } else {
        sellev <- ""
        tag <- "2d"
    }
    outdir <- dirname(fesom_f) # next to the unstructured input, not a separate tree
    fout <- file.path(outdir, paste0(var, "_timmean_remap_", tag, "_onto_", tools::file_path_sans_ext(basename(target_f)), ".nc"))
    if (!file.exists(fout) || force) {
        fout_tmp <- paste0(fout, ".tmp_", Sys.getpid()) # write-then-rename: a killed run must not leave a
                                                         # corrupt file that a later run's `file.exists(fout)` mistakes for done
        cmd <- paste0(cdo, " -O -s remapcon,", target_f, " ", sellev, " -setgrid,", fesom_mesh_griddes, " ", fesom_f, " ", fout_tmp)
        message("run `", cmd, "` ...")
        if (system(cmd) != 0 || !file.exists(fout_tmp)) {
            message("--> remap failed -- skip this map")
            unlink(fout_tmp)
            return(NULL)
        }
        file.rename(fout_tmp, fout)
    }
    fout
}

#####################################################################################################

# unit conversions (`fac`, applied to the obs/data side via plot.r's existing `fac` option) -- checked
# against py_f2recom's own code (github.com/RECOM-Regulated-Ecosystem-Model/py_f2recom, `develop` branch):
# - DIC_Glodap/Alk_Glodap/DIN_WOA_Comparison_python3.ipynb all explicitly label GLODAP/WOA's native
#   "micromol/kg" the same as FESOM's "mmol/m3" ("unitfesom = '... [mmol m$^{-3}$] # equals to mumol/L'",
#   i.e. no seawater-density correction) -- `fac` omitted (1) for DIC/Alk/DIN/DSi/O2 here too, matching
#   that convention exactly (DSi/O2 aren't in py_f2recom's own set, but same unit pair, same treatment).
# - DiaC_MarEDAT_python3.ipynb carries (commented out) "#DiaC = DiaC * 12.01" with FESOM's DiaC labelled
#   "[mmol C m-3]" vs MarEDAT's "[mg C m-3]" -- confirms the intended conversion (just disabled in their
#   code); applied here (`fac` = 1/12.01, converting MarEDAT/buitenhuis's mg C/L obs data to FESOM's own
#   mmol C/m3) for all 5 buitenhuis_etal_2013 pairs.
# - NPPsurf_OCNPP_Comparison_python3.ipynb notes the same "C: 12.01 g/mol" (also effectively disabled,
#   printing "FESOM data in not converted...") for mg C m-2 d-1 (obs) vs REcoM's mmol C m-2 d-1 -- applied
#   here for cmems's PP vs NPP.
# - CO2fluxSpatialTakahashi_python3.ipynb: "CO2ffesom = -365.*CO2ffesom/1000." for mmolC/m2/d <-> molC/m2/yr
#   (FESOM -> Takahashi direction, with an extra sign flip specific to Takahashi's own convention).
#   gregor_and_fay_2021/SeaFlux's fgco2 mean is -0.54 molC/m2/yr (globally, i.e. net *into* ocean under a
#   ">0: into atmosphere" convention -- physically correct, the ocean is a net sink), while FESOM's CO2f
#   mean is +0.79 mmolC/m2/d under its own documented ">0: into the surface water" convention -- opposite
#   signs, so despite the initial (wrong) assumption that these already matched, a sign flip IS needed here;
#   applied as `fac` = -1000/365 for gregor_and_fay_2021's fgco2.
# - huang_etal_2022's dFe (nmol/L) has no py_f2recom precedent (PISCES, their own DFe comparison, is
#   already in matching mmol/m3) -- nmol/L -> mmol/m3 is an exact volumetric conversion (`fac` = 1e-3),
#   not a density-dependent one like DIC/Alk/DIN/DSi/O2, so applied regardless.
# - temp/salt/pCO2s/chlorophyll (CHL, chlor_a, DiaChl+PhyChl): units already match on both sides, no
#   `fac` needed.
# - MLD3 (Griffies mixed layer depth): FESOM reports it as a *negative* z-coordinate (min/mean/max =
#   -941/-56/-7.5 m), while de_boyer_montegut_etal_2023's mld_dr003 is a *positive* depth-below-surface
#   (min/mean/max = 10/55/311 m) -- same units (m), opposite sign convention. Flipped via `fesom_fac`
#   (applied to the model side, so the report keeps everyone else's positive-depth convention).
# names of the datasets in the labels
data_name <- function(p) {
    if (p$dataset == "cmems") return(c(ARC_009_124_CHL="CMEMS ARC", GLO_009_104_CHL="GlobColour", GLO_009_108_CHL="ESA-CCI v6", GLO_009_104_PP="GlobColour")[[p$varname]])
    if (p$dataset == "lewis_etal_2022") return(if (identical(p$file_match, "^A_")) "AOReg.emp MODIS" else "AOReg.emp SeaWiFS")
    if (p$dataset == "buitenhuis_etal_2013") return(paste0("MAREDAT ", p$file_match))
    c(de_boyer_montegut_etal_2023="de Boyer Mont\u00e9gut 2023", glodap="GLODAPv2.2016b", gregor_and_fay_2021="SeaFlux",
      huang_etal_2022="Huang 2022", keppler_etal_2020="MOBO-DIC", steele_etal_2001="PHC3", woa="WOA23")[[p$dataset]]
}
model_name <- function(p) paste0("AWI-ESM3 ", p$fesom_var)

pairs <- list(
    # buitenhuis_etal_2013 (PFT biomass) vs REcoM PFT carbon; Phaeocystis has no REcoM PFT counterpart
    list(dataset="buitenhuis_etal_2013", varname="BIOMASS", file_match="Diatoms",          fesom_var="DiaC",    fac=1/12.01), # obs ug/L (=mg C/m3) -> FESOM mmol C/m3
    list(dataset="buitenhuis_etal_2013", varname="BIOMASS", file_match="Coccolithophores", fesom_var="PhyCalc", fac=1/12.01), # obs ug/L (=mg C/m3) -> FESOM mmol C/m3
    list(dataset="buitenhuis_etal_2013", varname="BIOMASS", file_match="Microzooplankton", fesom_var="HetC",    fac=1/12.01), # obs ug/L (=mg C/m3) -> FESOM mmol C/m3
    list(dataset="buitenhuis_etal_2013", varname="BIOMASS", file_match="Mesozooplankton",  fesom_var="Zoo2C",   fac=1/12.01), # obs ug/L (=mg C/m3) -> FESOM mmol C/m3
    list(dataset="buitenhuis_etal_2013", varname="BIOMASS", file_match="Macrozooplankton", fesom_var="Zoo3C",   fac=1/12.01), # obs ug/L (=mg C/m3) -> FESOM mmol C/m3
    list(dataset="de_boyer_montegut_etal_2023", varname="mld_dr003",   fesom_var="MLD3", fesom_fac=-1), # FESOM's MLD3 is negative-down, obs is positive-down
    list(dataset="glodap", varname="TCO2",        fesom_var="DIC"),
    list(dataset="glodap", varname="TAlk",        fesom_var="Alk"),
    list(dataset="glodap", varname="NO3",         fesom_var="DIN"),
    list(dataset="glodap", varname="silicate",    fesom_var="DSi"),
    list(dataset="glodap", varname="oxygen",      fesom_var="O2"),
    list(dataset="glodap", varname="temperature", fesom_var="temp"),
    list(dataset="glodap", varname="salinity",    fesom_var="salt"),
    list(dataset="gregor_and_fay_2021", varname="fgco2", fesom_var="CO2f", fac=-1000/365), # obs molC/m2/yr (>0 into atm) -> FESOM mmolC/m2/d (>0 into ocean): unit conversion + sign flip
    list(dataset="gregor_and_fay_2021", varname="pCO2s", fesom_var="pCO2s"),
    list(dataset="huang_etal_2022",     varname="dFe",   fesom_var="DFe", fac=1e-3), # obs nmol/L -> FESOM mmol/m3
    list(dataset="keppler_etal_2020",   varname="DIC",   fesom_var="DIC"),
    list(dataset="lewis_etal_2022", varname="chlor_a", file_match="^A_", fesom_var="Chl"),
    list(dataset="lewis_etal_2022", varname="chlor_a", file_match="^S_", fesom_var="Chl"),
    # list(dataset="cmems", varname="ARC_009_124_CHL", fesom_var="Chl"), # disabled: curvilinear grid has
    # no cell bounds usable by `cdo remapcon` -- FESOM's remap onto it comes back entirely missing
    list(dataset="cmems", varname="GLO_009_104_CHL", fesom_var="Chl"),
    list(dataset="cmems", varname="GLO_009_108_CHL", fesom_var="Chl"),
    list(dataset="cmems", varname="GLO_009_104_PP",  fesom_var="NPP", fac=1/12.01), # obs mg C/m2/d -> FESOM mmol C/m2/d
    list(dataset="steele_etal_2001", varname="temp", fesom_var="temp"),
    list(dataset="steele_etal_2001", varname="salt", fesom_var="salt"),
    list(dataset="woa", varname="temp", fesom_var="temp"),
    list(dataset="woa", varname="salt", fesom_var="salt"),
    list(dataset="woa", varname="DIN",  fesom_var="DIN"),
    list(dataset="woa", varname="DSi",  fesom_var="DSi"),
    list(dataset="woa", varname="O2",   fesom_var="O2")
)
fac_of <- function(p) if (is.null(p$fac)) 1 else p$fac
fesom_fac_of <- function(p) if (is.null(p$fesom_fac)) 1 else p$fesom_fac # like `fac`, but applied to the model side

# colorbar label of the anomaly plots: the data are in the units of the model after `fac`
anomaly_zlab_of <- function(p, model_f) {
    units <- var_units(model_f, nc_var_dims(model_f)$varname)
    units <- sub("^\\[(.*)\\]$", "\\1", units)
    if (nzchar(units)) paste0("anomaly [", units, "]") else "anomaly"
}

plots <- list()
lines_1d <- list() # per model variable and mode: all obs and the model, for 1 line plot
for (p in pairs) {
    anom_col <- if (p$fesom_var == "temp") F else NULL # sequential colors for temperature, although it has positive and negative values

    # --- maps (timmean): obs left, FESOM/REcoM (remapped onto the obs grid) right ---
    ddir <- file.path(pathpost, p$dataset, "timmean", p$varname)
    if (dir.exists(ddir)) {
        dfiles <- list.files(ddir, pattern="\\.nc$", full.names=T)
        if (!is.null(p$file_match)) dfiles <- dfiles[grepl(p$file_match, basename(dfiles))]
        for (f in dfiles) {
            dd <- nc_var_dims(f)
            survive <- dd$lens > 1
            ndims <- sum(survive)
            label <- tools::file_path_sans_ext(basename(f))

            if (ndims == 2) {
                model_f <- remap_fesom_map(p$fesom_var, f)
                if (!is.null(model_f)) {
                    fout <- file.path(pathplot, "comparison", p$dataset, "timmean", p$varname, paste0(label, "_vs_", p$fesom_var, ".png"))
                    plots[[length(plots)+1]] <- list(datas=list(list(fin=f, label=data_name(p), fac=fac_of(p)),
                                                                 list(fin=model_f, label=model_name(p), fac=fesom_fac_of(p))),
                                                      anomaly=T, same_units=T, anomaly_zlab=anomaly_zlab_of(p, model_f), zlab=map_zlab(p, model_f),
                                                      zlim_quantiles=c(0.1, 0.9), anom_colorbar=anom_col, fout=fout)
                }
            } else if (ndims == 3) {
                depth_i <- which(survive & !is.na(match(dd$names, known_depth_dimnames)))
                if (length(depth_i) != 1) {
                    message("skip \"", f, "\" (3 remaining dims, but none/more than 1 recognized as depth)")
                } else {
                    nc <- nc_open(f)
                    for (dw in depths_want) {
                        k <- nearest_idx(dd$vals[[depth_i]], dw)
                        start <- rep(1, length(dd$names)); count <- dd$lens; start[depth_i] <- k; count[depth_i] <- 1
                        slice <- ncvar_get(nc, dd$varname, start=start, count=count)
                        if (all(is.na(slice))) {
                            message("skip \"", f, "\" (", dd$names[depth_i], "=", dd$vals[[depth_i]][k], ": all-NA slice)")
                            next
                        }
                        model_f <- remap_fesom_map(p$fesom_var, f, depth_want=dw)
                        if (is.null(model_f)) next
                        dtag <- paste0(dd$names[depth_i], round(dd$vals[[depth_i]][k]))
                        fout <- file.path(pathplot, "comparison", p$dataset, "timmean", p$varname, paste0(label, "_", dtag, "_vs_", p$fesom_var, ".png"))
                        plots[[length(plots)+1]] <- list(datas=list(
                            list(fin=f, label=data_name(p), index=setNames(list(k), dd$names[depth_i]), fac=fac_of(p)),
                            list(fin=model_f, label=model_name(p), fac=fesom_fac_of(p))), anomaly=T, same_units=T,
                            anomaly_zlab=anomaly_zlab_of(p, model_f), zlab=map_zlab(p, model_f),
                            zlim_quantiles=c(0.1, 0.9), anom_colorbar=anom_col, fout=fout)
                    }
                    nc_close(nc)
                }
            }
        }
    }

    # --- zonmean/fldmean family: 1-dim overlaid, 2-dim side-by-side (obs left, model right) ---
    for (mode_name in c("zonmean", "zonmean_ymon", "fldmean", "fldmean_ymon")) {
        ddir2 <- file.path(pathpost, p$dataset, mode_name, p$varname)
        if (!dir.exists(ddir2)) next
        model_f <- fin_fesom(p$fesom_var, mode_name)
        if (is.null(model_f)) next
        dfiles2 <- list.files(ddir2, pattern="\\.nc$", full.names=T)
        if (!is.null(p$file_match)) dfiles2 <- dfiles2[grepl(p$file_match, basename(dfiles2))]
        if (length(dfiles2) == 0) next

        # e.g. FESOM's "Chl" (DiaChl+PhyChl, 3D) paired with a surface-only obs product (cmems/
        # lewis_etal_2022 chlor_a, 2D) -- model keeps a depth dim the data side never had; slice model
        # to the surface (nearest depth to 0) once per `mode_name`, same convention as the map case above
        data_dd <- nc_var_dims(dfiles2[1])
        data_has_depth <- any(!is.na(match(data_dd$names[data_dd$lens > 1], known_depth_dimnames)))
        model_dd <- nc_var_dims(model_f)
        model_survive <- model_dd$lens > 1
        model_depth_i <- which(model_survive & !is.na(match(model_dd$names, known_depth_dimnames)))
        model_index <- NULL
        if (!data_has_depth && length(model_depth_i) == 1) {
            k <- nearest_idx(model_dd$vals[[model_depth_i]], 0)
            model_index <- setNames(list(k), model_dd$names[model_depth_i])
        }

        for (f in dfiles2) {
            ndims <- sum(nc_var_dims(f)$lens > 1)
            if (!(ndims %in% 1:2)) {
                message("skip \"", f, "\" (", ndims, " remaining dim(s), plot.r draws 1 or 2)")
                next
            }
            if (ndims == 1) { # collected: 1 plot per model variable and mode, see below
                key <- paste0(p$fesom_var, "_", mode_name)
                if (is.null(lines_1d[[key]])) lines_1d[[key]] <- list(var=p$fesom_var, mode_name=mode_name, model_f=model_f, model_fac=fesom_fac_of(p), model_index=model_index,
                                                                      model_label=model_name(p), ylab=map_zlab(p, model_f), datas=list())
                lines_1d[[key]]$datas[[length(lines_1d[[key]]$datas)+1]] <- list(fin=f, label=data_name(p), fac=fac_of(p))
                next
            }
            label <- tools::file_path_sans_ext(basename(f))
            fout <- file.path(pathplot, "comparison", p$dataset, mode_name, p$varname, paste0(label, "_vs_", p$fesom_var, ".png"))
            model_data <- list(fin=model_f, label=model_name(p), fac=fesom_fac_of(p))
            if (!is.null(model_index)) model_data$index <- model_index
            plots[[length(plots)+1]] <- list(datas=list(list(fin=f, label=data_name(p), fac=fac_of(p)), model_data),
                                             anomaly=T, same_units=T, anomaly_zlab=anomaly_zlab_of(p, model_f), zlab=map_zlab(p, model_f),
                                             anom_colorbar=anom_col, fout=fout)
        }
    }
}

# 1 line plot with all obs of a model variable; model in black. `fac` of `pairs` brings all obs to the units of the model
for (l in lines_1d) {
    model_data <- list(fin=l$model_f, label=l$model_label, fac=l$model_fac, col="black", lwd=2)
    if (!is.null(l$model_index)) model_data$index <- l$model_index
    job <- list(datas=c(l$datas, list(model_data)), ylab=l$ylab,
                fout=file.path(pathplot, "comparison", "all_data", l$mode_name, l$var, paste0(l$var, "_", l$mode_name, "_all_data_vs_model.png")))
    if (l$mode_name == "fldmean") job$mode <- "fldmean"
    plots[[length(plots)+1]] <- job
}

message("--> ", length(plots), " comparison plot(s) built from ", length(pairs), " pairing(s)")

message("run `source(\"~/scripts/r/post/plot.r\")` ...")
source("~/scripts/r/post/plot.r")
