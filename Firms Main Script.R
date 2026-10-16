# ============================================================
# Firms Main Script.R
#
#   Combined data preparation and analysis script.
# ============================================================

rm(list = ls())
t_start <- proc.time()

# ============================================================
# USER SETTINGS
# ============================================================
# main_path : path to your project folder. It must contain an "Extract"
#             subfolder with your prepared Child_Panel / Parents_Panel /
#             AKM_Sample files (see Data Preparation Guide).
# country   : your country name (used only to name the output folder).

main_path <- "<path to your project folder>"
country   <- "<your country name>"

# ============================================================
# PACKAGES
# ============================================================

pkgs <- c("data.table","haven","lfe","ggplot2","systemfit","car",
          "openxlsx","e1071")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, quiet = TRUE)
}
library(data.table); library(haven);      library(lfe)
library(ggplot2);    library(systemfit);  library(car)
library(openxlsx);   library(e1071)

config <- list(

  # ── Specification ─────────────────────────────────────────
  # "main"              : cohorts 1980-1984, child ages 35-40, years 2015-2019
  # "robustness"        : Age extension - cohorts
  #                       1965-1969, child ages 25-40, years 2005+. Looks
  #                       for panels with the "_robustness" suffix.
  # "robustness_cohort" : Cohort extension - cohorts
  #                       1965-1969, child ages 35-40, years 2005+. Looks
  #                       for panels with the "_robustness_cohort" suffix.
  
  spec = "main",

  # ── IGM sample (children + parents) ─────────────────────

  # ── AKM panel ─────────────────────────────────────────────
  akm_year_min     = 2015L,
  akm_year_max     = 2019L,
  akm_age_span     = c(25L, 65L),

  min_firm_size    = 10L,
  female_code      = 1L,

  # ── Sample definitions (thresholds) ────────────────────────
  
  thresholdPct1  = 0.325,
  thrQDenom      = 4,     # Thr0325Q = thresholdPct1 / thrQDenom

  # ── K-means / BLM firm clustering ──────────────────────────
  # K          : number of firm clusterss
  # Nq         : number of global log-wage quantile bins used to build
  #              each firm's empirical wage-distribution CDF
  # nBootstrap : bootstrap replications for the weighted k-means (bclust)
  # kMeansIter : max k-means iterations within each bootstrap replication
  K             = 10L,
  Nq            = 20L,
  nBootstrap    = 10L,
  kMeansIter    = 100L,
  cluster_seed  = 123L,

  # ── Output / figures ──────────────────────────────────────
  n_bins      = 100L,
  fig_width   = 10,
  fig_height  = 7,
  dpi         = 300,
  keep_gender = c(0L, 1L),
  n_digits    = 3,
  col_firm    = "#4472C4",
  col_ind     = "#ED7D31"
)

# ── Spec presets ─────────────────────────────────────────────

spec_presets <- list(
  main = list(
    suffix             = "",
    child_age_span     = c(35L, 40L),
    child_cohort_span  = c(1980L, 1984L),
    child_obs_year_min = 2015L,
    child_obs_year_max = 2019L
  ),
  robustness = list(
    suffix             = "_robustness",
    child_age_span     = c(25L, 40L),
    child_cohort_span  = c(1965L, 1969L),
    child_obs_year_min = 2005L,
    child_obs_year_max = Inf
  ),
  robustness_cohort = list(
    suffix             = "_robustness_cohort",
    child_age_span     = c(35L, 40L),
    child_cohort_span  = c(1965L, 1969L),
    child_obs_year_min = 2005L,
    child_obs_year_max = Inf
  )
)

if (!config$spec %in% names(spec_presets)) {
  stop(sprintf(
    "config$spec = '%s' not recognized (expected one of: %s)",
    config$spec, paste(names(spec_presets), collapse = ", ")))
}
sp <- spec_presets[[config$spec]]
config$child_age_span      <- sp$child_age_span
config$child_cohort_span   <- sp$child_cohort_span
config$child_obs_year_min  <- sp$child_obs_year_min
config$child_obs_year_max  <- sp$child_obs_year_max

suffix      <- sp$suffix
extract_dir <- file.path(main_path, "Extract")
output_dir  <- file.path(main_path, paste0("Firms_intergen_", country))
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat(sprintf("main_path   : %s\n", main_path))
cat(sprintf("extract_dir : %s\n", extract_dir))

# ============================================================
# INLINED HELPER FUNCTIONS
#
# read_panel(): auto-detects file format so teams can supply
#   .dta / .sav / .sas7bdat  (via haven)  OR  .rds  (native R).
#   The file_path argument should include the extension.
read_panel <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  dt  <- switch(ext,
    rds      = readRDS(file_path),
    dta      = haven::read_dta(file_path),
    sav      = haven::read_sav(file_path),
    sas7bdat = haven::read_sas(file_path),
    stop(sprintf(
      "Unsupported file extension '.%s' for '%s' (expected .dta, .rds, .sav, or .sas7bdat)",
      ext, file_path)))
  as.data.table(dt)
}

# Auto-detect file extension: checks .rds first, falls back to .dta/.sav/.sas7bdat
find_panel <- function(dir, stem) {
  for (ext in c(".rds", ".dta", ".sav", ".sas7bdat")) {
    p <- file.path(dir, paste0(stem, ext))
    if (file.exists(p)) return(p)
  }
  stop(sprintf("No panel file found for '%s' in %s\n(tried .rds, .dta, .sav, .sas7bdat)", stem, dir))
}

# Like find_panel(), but returns NULL instead of stopping when nothing is
# found - used for the Parents_Panel / Father_Panel fallback below.
find_panel_opt <- function(dir, stem) {
  tryCatch(find_panel(dir, stem), error = function(e) NULL)
}
# ============================================================

createBinsPlot <- function(xPlot, yPlot, nBins) {
  flag  <- is.finite(xPlot) & is.finite(yPlot)
  xPlot <- xPlot[flag]; yPlot <- yPlot[flag]
  brks    <- quantile(xPlot, seq(0, 1 - 1/nBins, 1/nBins))
  binPlot <- findInterval(xPlot, brks)
  dt <- data.table(xPlot, yPlot, binPlot)
  dt[, .(N=.N, x=mean(xPlot), y=mean(yPlot)), by=binPlot]
}

frac_rank <- function(x, scale=100) {
  n <- sum(!is.na(x))
  if (n == 0L) return(rep(NA_real_, length(x)))
  r <- rank(x, ties.method="average", na.last="keep")
  r / n * scale
}

theme_ige <- function(base_size=14) {
  theme_bw(base_size=base_size) +
    theme(panel.background=element_rect(fill="white",colour="black"),
          plot.background =element_rect(fill="white",colour="white"),
          legend.background=element_rect(fill="white"),
          legend.key       =element_rect(fill="white",colour="white"),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
}

ige_colors <- c(blue="royalblue",
                orange=rgb(219/255,112/255,41/255))


cat("=======================================================\n")
cat("Firms Main Script.R\n")
cat(sprintf("  Spec : %s  |  Methods : Standard AKM + K-means AKM (K=%d)\n",
            config$spec, config$K))
cat("=======================================================\n\n")

# ============================================================
# process_panel()
#
# Cleans a worker-year panel: positive earnings floor, log
# transform, residualisation on a plain quadratic age + year
# control (pooled across gender -- no age centering, no gender
# interaction; see config block above), within-cell fractional
# rank.
#
# ============================================================

process_panel <- function(panel, label = "") {
  d <- copy(panel)

  d <- d[earning > 0 & is.finite(earning)]
  d[, logW := log(earning)]
  d <- d[is.finite(logW)]

  cat(sprintf("  [%s] %d obs entering residualisation\n", label, nrow(d)))

  fit <- lm(logW ~ age + I(age^2) + factor(year), data = d)
  d[, logWResid := resid(fit)]
  cat(sprintf("  [%s] R^2 = %.4f\n", label, summary(fit)$r.squared))

  d[, rank_earn := frac_rank(earning), by = .(gender, age, year)]
  d
}


# ============================================================
# Groups firms into K clusters based on the shape of their
# empirical log-wage distribution:
#   1. bin every worker-year observation into one of Nq GLOBAL
#      log-wage quantile bins
#   2. collapse to firm x bin counts, normalise by firm size and
#      sqrt(Nq), and take the cumulative sum -> each firm is
#      represented by a smoothed empirical wage-distribution CDF
#   3. run a weighted bootstrap k-means (e1071::bclust) on these
#      per-firm CDF vectors, weighting by firm size
#
# Returns: data.table(firmID, clusterID)
# ============================================================

clusterFirms <- function(lm, K = 10L, Nq = 20L, nBootstrap = 10L,
                         kMeansIter = 100L, seed = 123L) {

  set.seed(seed)
  dt <- copy(lm)
  dt <- dt[earning > 0 & is.finite(earning) & !is.na(firmID)]
  dt[, logW := log(earning)]

  n_firms <- uniqueN(dt$firmID)
  if (n_firms <= K) {
    cat(sprintf("  clusterFirms: only %d firms (<= K=%d) - each firm becomes its own cluster\n",
                n_firms, K))
    return(data.table(firmID = unique(dt$firmID),
                      clusterID = as.character(unique(dt$firmID))))
  }

  # Global log-wage quantile bins (Nq bins, open-ended at both tails)
  qbrks <- unique(quantile(dt$logW, probs = seq(0, 1, length.out = Nq + 1), na.rm = TRUE))
  if (length(qbrks) < 3L) {
    warning("clusterFirms: insufficient wage variation to form quantile bins - skipping clustering")
    return(data.table(firmID = unique(dt$firmID),
                      clusterID = as.character(unique(dt$firmID))))
  }
  qbrks[1] <- -Inf; qbrks[length(qbrks)] <- Inf
  dt[, qbin := findInterval(logW, qbrks, all.inside = TRUE)]

  n_bins_actual <- length(qbrks) - 1L

  firm_bin  <- dt[, .N, by = .(firmID, qbin)]
  firm_size <- dt[, .(W = .N), by = firmID]

  wide <- dcast(firm_bin, firmID ~ qbin, value.var = "N", fill = 0)
  bin_cols <- setdiff(names(wide), "firmID")
  missing_bins <- setdiff(as.character(seq_len(n_bins_actual)), bin_cols)
  for (mb in missing_bins) wide[, (mb) := 0]
  bin_cols <- as.character(seq_len(n_bins_actual))
  setcolorder(wide, c("firmID", bin_cols))

  wide <- merge(wide, firm_size, by = "firmID")

  mat <- as.matrix(wide[, ..bin_cols])
  mat <- mat / (wide$W * sqrt(n_bins_actual))
  cdf_mat <- t(apply(mat, 1, cumsum))
  rownames(cdf_mat) <- wide$firmID

  cluster_assign <- tryCatch({
    bc <- e1071::bclust(
      cdf_mat,
      centers        = K,
      iter.base      = nBootstrap,
      minsize        = 0,
      dist.method    = "euclidean",
      hclust.method  = "average",
      base.method    = "kmeans",
      base.centers   = max(K, 20L),
      verbose        = FALSE,
      final.kmeans   = TRUE,
      resample       = TRUE,
      weights        = wide$W,
      maxcluster     = max(K, 20L))
    bc$cluster
  }, error = function(e) {
    cat(sprintf("  clusterFirms: bclust failed (%s) - falling back to plain kmeans\n", conditionMessage(e)))
    km <- kmeans(cdf_mat, centers = min(K, n_firms), iter.max = kMeansIter, nstart = 5)
    km$cluster
  })

  data.table(firmID = wide$firmID, clusterID = as.character(cluster_assign))
}


# ============================================================
# Runs log-space and rank-space two-way FE AKM regressions on
# an already-filtered AKM panel, using `feVar` ("firmID" or
# "clusterID") as the firm-side fixed effect. Returns worker-
# level alpha / psi (psi averaged over the child analytical age
# window) plus a plug-in variance decomposition row computed
# over the full estimation sample.
# ============================================================

run_akm_method <- function(dt, feVar = "firmID", child_age_span, min_firm_size,
                           method_label = "", tag = "") {

  dt <- copy(dt)
  dt[, (feVar) := as.character(get(feVar))]

  # Firm/cluster-size filter re-applied on feVar grouping
  sz        <- dt[, .(n = uniqueN(workerID)), by = feVar]
  keep_ids  <- sz[n >= min_firm_size][[feVar]]
  dt        <- dt[get(feVar) %in% keep_ids]

  dt[, workerID_f := as.factor(workerID)]
  dt[, fe_f        := as.factor(get(feVar))]

  cat(sprintf("  [%s] estimation sample: %d obs | %d workers | %d %s\n",
              method_label, nrow(dt), uniqueN(dt$workerID), uniqueN(dt[[feVar]]), feVar))

  # ── Log-space AKM ──────────────────────────────────────────
  # Plain quadratic age + year FE, pooled across gender (see config
  # block) -- matches process_panel()'s age/year control.
  fit_log <- felm(logW ~ age + I(age^2) + factor(year) |
                     workerID_f + fe_f, data = dt)
  fe_log   <- setDT(getfe(fit_log))
  alpha_dt <- fe_log[fe == "workerID_f", .(workerID = as.character(idx), alpha = effect)]
  psi_firm <- fe_log[fe == "fe_f",       .(id = as.character(idx), psi = effect)]
  setnames(psi_firm, "id", feVar)
  cat(sprintf("  [%s] log-space AKM R^2 = %.4f\n", method_label, summary(fit_log)$r2))

  # ── Rank-space AKM ──────────────────────────────────────────
  dt[earning > 0 & is.finite(earning), rank_earn := frac_rank(earning), by = .(gender, age, year)]
  fit_rank <- felm(rank_earn ~ 1 | workerID_f + fe_f, data = dt[is.finite(rank_earn)])
  fe_rank       <- setDT(getfe(fit_rank))
  alpha_rank_dt <- fe_rank[fe == "workerID_f", .(workerID = as.character(idx), alpha_rank = effect)]
  psi_rank_firm <- fe_rank[fe == "fe_f",       .(id = as.character(idx), psi_rank = effect)]
  setnames(psi_rank_firm, "id", feVar)
  cat(sprintf("  [%s] rank-space AKM R^2 = %.4f\n", method_label, summary(fit_rank)$r2))

  # ── Average psi over the child analytical age window ────────
  psi_worker <- dt[age >= child_age_span[1] & age <= child_age_span[2]][
    psi_firm, on = feVar, nomatch = 0L][
    , .(psi = mean(psi, na.rm = TRUE)), by = workerID]
  psi_rank_worker <- dt[age >= child_age_span[1] & age <= child_age_span[2]][
    psi_rank_firm, on = feVar, nomatch = 0L][
    , .(psi_rank = mean(psi_rank, na.rm = TRUE)), by = workerID]

  # ── Plug-in variance decomposition (full estimation sample) ─
  vd <- merge(dt[, .(workerID, fe_id = get(feVar))], alpha_dt, by = "workerID")
  vd <- merge(vd, psi_firm, by.x = "fe_id", by.y = feVar)
  var_alpha <- var(vd$alpha, na.rm = TRUE)
  var_psi   <- var(vd$psi,   na.rm = TRUE)
  cov_ap    <- cov(vd$alpha, vd$psi, use = "complete.obs")
  var_total <- var(dt$logW, na.rm = TRUE)

  vardecomp_row <- data.table(
    tag            = tag,
    method         = method_label,
    n_obs          = nrow(dt),
    n_workers      = uniqueN(dt$workerID),
    n_fe_units     = uniqueN(dt[[feVar]]),
    var_logW       = var_total,
    var_alpha      = var_alpha,
    var_psi        = var_psi,
    cov_alpha_psi  = cov_ap,
    two_cov        = 2 * cov_ap,
    var_explained  = var_alpha + var_psi + 2 * cov_ap,
    share_alpha    = var_alpha / var_total,
    share_psi      = var_psi   / var_total,
    share_2cov     = (2 * cov_ap) / var_total,
    r2_log         = summary(fit_log)$r2,
    r2_rank        = summary(fit_rank)$r2
  )

  list(alpha = alpha_dt, psi = psi_worker,
       alpha_rank = alpha_rank_dt, psi_rank = psi_rank_worker,
       vardecomp_row = vardecomp_row)
}


# ============================================================
# Runs Steps 2-4 (SUR decomposition, covariate controls,
# bar charts) for given variable names and file prefix.
# ============================================================

fmt_num   <- function(x, d = config$n_digits) formatC(x, format = "f", digits = d)

run_decomp <- function(sample, total_var, alpha_var, psi_var,
                       prefix, run_rank = TRUE) {

  cat("-------------------------------------------------------\n")
  cat(sprintf("STEPS 2-4: %s\n", prefix)); cat("-------------------------------------------------------\n\n")

  adt <- sample[is.finite(get(total_var)) & is.finite(get(alpha_var)) &
                is.finite(get(psi_var))   & is.finite(f_logWResid)]
  cat(sprintf("  %d complete cases\n", nrow(adt)))

  # ── Step 2a: Log SUR ───────────────────────────────────────────────
  cat("  Step 2: Log-space SUR...\n")
  fml_sur <- list(
    regIGE     = as.formula(paste(total_var, "~ f_logWResid")),
    regIndIGE  = as.formula(paste(alpha_var, "~ f_logWResid")),
    regFirmIGE = as.formula(paste(psi_var,   "~ f_logWResid"))
  )
  fit_s   <- systemfit(fml_sur, data = adt)
  n_obs_s <- sapply(fit_s$eq, function(e) length(residuals(e)))
  cl_s    <- c(fit_s$coefficients["regIGE_f_logWResid"],
               fit_s$coefficients["regIndIGE_f_logWResid"],
               fit_s$coefficients["regFirmIGE_f_logWResid"])
  se_s    <- c(sqrt(fit_s$coefCov["regIGE_f_logWResid","regIGE_f_logWResid"]),
               sqrt(fit_s$coefCov["regIndIGE_f_logWResid","regIndIGE_f_logWResid"]),
               sqrt(fit_s$coefCov["regFirmIGE_f_logWResid","regFirmIGE_f_logWResid"]))
  dm_i  <- deltaMethod(fit_s, "regIndIGE_f_logWResid/regIGE_f_logWResid")
  dm_f  <- deltaMethod(fit_s, "regFirmIGE_f_logWResid/regIGE_f_logWResid")
  sh_s  <- c(1, dm_i$Estimate, dm_f$Estimate)
  se_sh <- c(NA, dm_i$SE, dm_f$SE)
  cat(sprintf("  IGE=%.4f | Ind=%.4f | Firm=%.4f\n", cl_s[1], cl_s[2], cl_s[3]))

  write.csv(data.table(component=c("IGE","Individual-IGE","Firm-IGE"),
    beta=cl_s, se_beta=se_s, share_of_IGE=sh_s, se_share_of_IGE=se_sh,
    n_obs=n_obs_s),
    file.path(output_dir, paste0("FirmWorkerDecomposition_IGE_",prefix,".csv")),
    row.names=FALSE)

  cat(sprintf("  Saved: FirmWorkerDecomposition_IGE_%s.csv\n", prefix))

  # ── Step 2b: Rank SUR ──────────────────────────────────────────────
  if (run_rank && all(c("resRankAKM","alpha_rank","psi_rank","f_rank") %in% names(adt))) {
    cat("  Step 2: Rank-space SUR...\n")
    rdt <- adt[is.finite(resRankAKM)&is.finite(alpha_rank)&
               is.finite(psi_rank)  &is.finite(f_rank)]
    fr  <- systemfit(list(regRRS=resRankAKM~f_rank, regIndRRS=alpha_rank~f_rank,
                          regFirmRRS=psi_rank~f_rank), data=rdt)
    nr  <- sapply(fr$eq,function(e) length(residuals(e)))
    cr  <- c(fr$coefficients["regRRS_f_rank"],
             fr$coefficients["regIndRRS_f_rank"],
             fr$coefficients["regFirmRRS_f_rank"])
    sr  <- c(sqrt(fr$coefCov["regRRS_f_rank","regRRS_f_rank"]),
             sqrt(fr$coefCov["regIndRRS_f_rank","regIndRRS_f_rank"]),
             sqrt(fr$coefCov["regFirmRRS_f_rank","regFirmRRS_f_rank"]))
    dir <- deltaMethod(fr,"regIndRRS_f_rank/regRRS_f_rank")
    dfr <- deltaMethod(fr,"regFirmRRS_f_rank/regRRS_f_rank")
    shr <- c(1,dir$Estimate,dfr$Estimate); sse <- c(NA,dir$SE,dfr$SE)
    cat(sprintf("  RRS=%.4f | Ind=%.4f | Firm=%.4f\n",cr[1],cr[2],cr[3]))
    write.csv(data.table(component=c("RRS","Individual-RRS","Firm-RRS"),
      beta=cr,se_beta=sr,share_of_RRS=shr,se_share_of_RRS=sse,n_obs=nr),
      file.path(output_dir,paste0("FirmWorkerDecomposition_Rank_",prefix,".csv")),row.names=FALSE)
    cat(sprintf("  Saved: FirmWorkerDecomposition_Rank_%s.csv\n", prefix))
  }

  # ── Step 3: Covariate controls ─────────────────────────────────────
  # Controls are optional — only those present in the data are used.
  # If none are found, Step 3 is skipped entirely.
  cat("  Step 3: Covariate controls...\n")
  cdt <- copy(adt)

  cov_map <- c(ed_ID       = "ed_ID_f",
               immigrant_bg = "immigrant_bg_f",
               czID        = "czID_f")
  cov_lbl <- c(ed_ID       = "Education FE",
               immigrant_bg = "Demographics FE",
               czID        = "Comm. Zone FE")
  cov_avail <- names(cov_map)[names(cov_map) %in% names(cdt)]

  if (length(cov_avail) == 0L) {

    cat("  No covariate columns found — skipping Step 3.\n")

  } else {

    cat(sprintf("  Covariates found: %s\n", paste(cov_avail, collapse=", ")))

    for (cv in cov_avail)
      cdt[, (cov_map[cv]) := factor(get(cv))]

    cov_specs <- lapply(cov_avail, function(cv)
      list(label = cov_lbl[cv], controls = cov_map[cv]))
    if (length(cov_avail) > 1L) {
      all_ctrl <- paste(cov_map[cov_avail], collapse = "+")
      cov_specs <- c(cov_specs, list(list(label="All controls", controls=all_ctrl)))
    }
    specs <- c(list(list(label="No controls", controls="")), cov_specs)
    n_sp  <- length(specs)

    run_ctrl <- function(ov, cf, dt, pred = "f_logWResid") {
      fml <- if (nzchar(cf)) paste(ov, "~", pred, "|", cf) else paste(ov, "~", pred)
      fit <- felm(as.formula(fml), data = dt)
      sm  <- summary(fit)
      list(coef = sm$coefficients[pred,"Estimate"],
           se   = sm$coefficients[pred,"Std. Error"],
           n    = length(residuals(fit)))
    }

    fs_t <- function(x) paste0("(", formatC(round(x,3), format="f", digits=3), ")")

    build_panel <- function(ov, bb, rl, row_lbl) {
      cr  <- sapply(specs, function(s) fmt_num(rl[[s$label]]$coef))
      sr  <- sapply(specs, function(s) fs_t(rl[[s$label]]$se))
      nr  <- sapply(specs, function(s) formatC(rl[[s$label]]$n, format="d", big.mark=","))
      sv  <- sapply(specs, function(s)
        if (s$label == "No controls") "---"
        else { v <- (1 - rl[[s$label]]$coef/bb)*100
               paste0(formatC(v, format="f", digits=1), "\\%") })
      fe_rows <- lapply(cov_avail, function(cv)
        sapply(specs, function(s) if (grepl(cov_map[cv], s$controls)) "Yes" else "No"))
      names(fe_rows) <- cov_lbl[cov_avail]
      mat <- rbind(cr, sr, do.call(rbind, fe_rows), nr, sv)
      rownames(mat) <- c(row_lbl, "", names(fe_rows), "Observations", "Share explained")
      mat
    }

    res_log <- list()
    for (ov in c(alpha_var, psi_var)) {
      rl <- lapply(specs, function(s) run_ctrl(ov, s$controls, cdt))
      names(rl) <- sapply(specs, `[[`, "label"); res_log[[ov]] <- rl
    }
    bb_a <- res_log[[alpha_var]][["No controls"]]$coef
    bb_p <- res_log[[psi_var  ]][["No controls"]]$coef

    tidy_log <- rbindlist(lapply(c(alpha_var, psi_var), function(ov) {
      bb <- if (ov == alpha_var) bb_a else bb_p
      rbindlist(lapply(specs, function(s) { r <- res_log[[ov]][[s$label]]
        data.table(outcome=ov, control_label=s$label,
                   coef=r$coef, se=r$se, n=r$n, share_explained=1-r$coef/bb) }))
    }))
    saveRDS(tidy_log, file.path(output_dir, paste0("controlsIGE_results_",prefix,".rds")))
    fwrite(tidy_log,  file.path(output_dir, paste0("controlsIGE_results_",prefix,".csv")))

    pa  <- build_panel(alpha_var, bb_a, res_log[[alpha_var]], "$\\overline{\\log Y_{f(i)}}$")
    pb  <- build_panel(psi_var,   bb_p, res_log[[psi_var  ]], "$\\overline{\\log Y_{f(i)}}$")
    sep <- matrix("", nrow=1, ncol=n_sp); rownames(sep) <- " "
    fm  <- rbind(pa, sep, pb)
    fwrite(as.data.table(fm, keep.rownames="statistic"),
           file.path(output_dir, paste0("CovariatesDecomposition_IGE_",prefix,".csv")))
    cat(sprintf("  Saved: CovariatesDecomposition_IGE_%s.csv\n", prefix))

    if (run_rank && all(c("alpha_rank","psi_rank","f_rank") %in% names(cdt))) {
      res_rr <- list()
      for (ov in c("alpha_rank","psi_rank")) {
        rl <- lapply(specs, function(s) run_ctrl(ov, s$controls, cdt, "f_rank"))
        names(rl) <- sapply(specs, `[[`, "label"); res_rr[[ov]] <- rl
      }
      bb_ar <- res_rr[["alpha_rank"]][["No controls"]]$coef
      bb_pr <- res_rr[["psi_rank"  ]][["No controls"]]$coef
      tidy_rr <- rbindlist(lapply(c("alpha_rank","psi_rank"), function(ov) {
        bb <- if (ov == "alpha_rank") bb_ar else bb_pr
        rbindlist(lapply(specs, function(s) { r <- res_rr[[ov]][[s$label]]
          data.table(outcome=ov, control_label=s$label,
                     coef=r$coef, se=r$se, n=r$n, share_explained=1-r$coef/bb) }))
      }))
      saveRDS(tidy_rr, file.path(output_dir, paste0("controlsRR_results_",prefix,".rds")))
      fwrite(tidy_rr,  file.path(output_dir, paste0("controlsRR_results_",prefix,".csv")))
      par2 <- build_panel("alpha_rank", bb_ar, res_rr[["alpha_rank"]], "$\\overline{R_{f(i)}}$")
      pbr2 <- build_panel("psi_rank",   bb_pr, res_rr[["psi_rank"  ]], "$\\overline{R_{f(i)}}$")
      rownames(par2)[nrow(par2)] <- "Share of RRS explained"
      rownames(pbr2)[nrow(pbr2)] <- "Share of RRS explained"
      fmr <- rbind(par2, sep, pbr2)
      fwrite(as.data.table(fmr, keep.rownames="statistic"),
             file.path(output_dir, paste0("CovariatesDecomposition_Rank_",prefix,".csv")))
      cat(sprintf("  Saved: CovariatesDecomposition_Rank_%s.csv\n", prefix))
    }

  } # end covariate section

  # ── Step 4: Bar charts ─────────────────────────────────────────────
  cat("  Step 4: Bar charts...\n")
  label_map<-c("Education FE"="Education","Demographics FE"="Demographics","Comm. Zone FE"="Comm. Zone")
  x_order<-c("Education","Demographics","Comm. Zone")

  make_bar<-function(rds_path,outcome_map,y_lbl,col_vals,stem){
    if(!file.exists(rds_path)) return(invisible(NULL))
    res<-readRDS(rds_path); setDT(res)
    plt<-res[control_label %in% names(label_map)]
    plt[,display_label:=label_map[control_label]]
    plt[,spct:=share_explained*100]
    plt[,ige_type:=outcome_map[outcome]]
    plt[,display_label:=factor(display_label,levels=x_order)]
    plt[,ige_type:=factor(ige_type,levels=names(col_vals))]
    y_mn<-min(-40,floor(min(plt$spct,na.rm=TRUE)/10)*10-5)
    y_mx<-max(55, ceiling(max(plt$spct,na.rm=TRUE)/10)*10+5)
    g<-ggplot(plt,aes(x=display_label,y=spct,fill=ige_type))+
      geom_bar(stat="identity",position=position_dodge(width=0.6),width=0.55)+
      geom_text(aes(label=paste0(formatC(spct,format="f",digits=1),"%"),
                    vjust=ifelse(spct>=5,1.4,-0.3)),
                position=position_dodge(width=0.6),size=3.5,color="black")+
      scale_fill_manual(values=col_vals,name=NULL)+
      scale_y_continuous(limits=c(y_mn,y_mx),breaks=seq(y_mn,y_mx,by=10),
                         labels=function(x)paste0(x,"%"),expand=expansion(mult=c(0,0.05)))+
      geom_hline(yintercept=0,colour="black",linewidth=0.4)+
      labs(x="Explanatory Variables",y=y_lbl,title=NULL)+
      theme_bw(base_size=13)+
      theme(legend.position="bottom",legend.key.size=unit(0.5,"cm"),
            legend.text=element_text(size=12),axis.title=element_text(size=12),
            axis.text=element_text(size=11),panel.grid.minor=element_blank(),
            panel.grid.major.x=element_blank())
    ggsave(paste0(stem,".pdf"),plot=g,width=6,height=4.5)
    ggsave(paste0(stem,".png"),plot=g,width=6,height=4.5,dpi=300)
    cat(sprintf("  Saved: %s.pdf/.png\n", basename(stem)))
  }

  make_bar(file.path(output_dir,paste0("controlsIGE_results_",prefix,".rds")),
    setNames(c("Individual-IGE","Firm-IGE"),c(alpha_var,psi_var)),
    "Share of IGE Explained",
    c("Firm-IGE"=config$col_firm,"Individual-IGE"=config$col_ind),
    file.path(output_dir,paste0("CovariatesDecomposition_IGE_",prefix)))

  if(run_rank)
    make_bar(file.path(output_dir,paste0("controlsRR_results_",prefix,".rds")),
      c("alpha_rank"="Individual-RRS","psi_rank"="Firm-RRS"),
      "Share of RRS Explained",
      c("Firm-RRS"=config$col_firm,"Individual-RRS"=config$col_ind),
      file.path(output_dir,paste0("CovariatesDecomposition_Rank_",prefix)))

  cat(sprintf("  [%s] Steps 2-4 complete.\n\n", prefix))
}


# ============================================================
# Produces the IGE and rank-rank binscatter plots/exports for
# a given IGMAKM sample, tagged by sample x method.
# ============================================================

make_binscatters <- function(IGMAKM_Sample, tag) {

  cat("-------------------------------------------------------\n")
  cat(sprintf("Binscatter plots: %s\n", tag)); cat("-------------------------------------------------------\n\n")

  bs_dt <- copy(IGMAKM_Sample)
  bs_dt[, f_logWResid := f_logWResid + mean(f_logW,na.rm=TRUE) - mean(f_logWResid,na.rm=TRUE)]
  bs_dt[, logWResid   := logWResid   + mean(logW,  na.rm=TRUE) - mean(logWResid,  na.rm=TRUE)]

  reg_ige   <- summary(lm(logWResid ~ f_logWResid, data=bs_dt))
  label_ige <- sprintf("Slope = %.2f (%.2f)",
                       round(reg_ige$coefficients[2,1],2),
                       round(reg_ige$coefficients[2,2],2))
  cat(sprintf("  IGE: %s\n", label_ige))

  bins_ige <- createBinsPlot(bs_dt$f_logWResid, bs_dt$logWResid, config$n_bins)
  y_lo <- floor(min(bins_ige$y,na.rm=TRUE)*10)/10
  y_hi <- ceiling(max(bins_ige$y,na.rm=TRUE)*10)/10
  x_lo <- floor(min(bins_ige$x,na.rm=TRUE))
  x_hi <- ceiling(max(bins_ige$x,na.rm=TRUE))

  g_ige <- ggplot(bins_ige, aes(x=x,y=y)) + theme_bw() +
    labs(x="log(father's earnings)", y="log(earnings)") +
    geom_point(size=3, color=unname(ige_colors["blue"])) +
    geom_smooth(method="lm",se=FALSE,formula="y~x",color=unname(ige_colors["orange"])) +
    scale_x_continuous(breaks=seq(x_lo,x_hi,1),limits=c(x_lo,x_hi)) +
    scale_y_continuous(limits=c(y_lo,y_hi)) +
    theme(text=element_text(size=34),legend.position="none") +
    geom_text(x=x_lo,y=y_hi,label=label_ige,hjust="left",vjust="top",size=12,family="mono")
  ggsave(file.path(output_dir,paste0("Binscatter_IGE_",tag,".png")), g_ige+theme_ige(),
         width=config$fig_width, height=config$fig_height, dpi=config$dpi)
  cat(sprintf("  Saved: Binscatter_IGE_%s.png\n", tag))

  ige_export <- data.frame(
    bin = bins_ige$binPlot,
    mean_parent_log_earnings    = round(bins_ige$x, 4),
    expected_child_log_earnings = round(bins_ige$y, 4))
  write.xlsx(ige_export, file.path(output_dir,paste0("Binscatter_IGE_data_",tag,".xlsx")),
             sheetName="Expected Log Earnings", rowNames=FALSE)
  cat(sprintf("  Saved: Binscatter_IGE_data_%s.xlsx\n", tag))

  if (all(c("rank","f_rank") %in% names(bs_dt))) {
    rr_dt <- bs_dt[is.finite(rank) & is.finite(f_rank)]
    reg_rr    <- summary(lm(rank ~ f_rank, data=rr_dt))
    label_rr  <- sprintf("Slope = %.3f (%.3f)",
                         round(reg_rr$coefficients[2,1],3),
                         round(reg_rr$coefficients[2,2],3))
    cat(sprintf("  RRS: %s\n", label_rr))
    bins_rr <- createBinsPlot(rr_dt$f_rank, rr_dt$rank, config$n_bins)
    y_lo_rr <- floor(min(bins_rr$y,na.rm=TRUE)*2)/2
    y_hi_rr <- ceiling(max(bins_rr$y,na.rm=TRUE)*2)/2
    g_rr <- ggplot(bins_rr, aes(x=x,y=y)) + theme_bw() +
      labs(x="Father's percentile rank", y="Child's percentile rank") +
      geom_point(size=3, color=unname(ige_colors["blue"])) +
      geom_smooth(method="lm",se=FALSE,formula="y~x",color=unname(ige_colors["orange"])) +
      scale_x_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
      scale_y_continuous(limits=c(y_lo_rr,y_hi_rr)) +
      theme(text=element_text(size=34),legend.position="none") +
      geom_text(x=2,y=y_hi_rr,label=label_rr,hjust="left",vjust="top",size=12,family="mono")
    ggsave(file.path(output_dir,paste0("Binscatter_Rank_",tag,".png")), g_rr+theme_ige(),
           width=config$fig_width, height=config$fig_height, dpi=config$dpi)
    cat(sprintf("  Saved: Binscatter_Rank_%s.png\n", tag))
    rexp <- data.frame(parent_percentile=bins_rr$binPlot,
                       expected_child_rank=round(bins_rr$y,2))
    write.xlsx(rexp, file.path(output_dir,paste0("Percentile_Expected_Rank_",tag,".xlsx")),
               sheetName="Expected Rank", rowNames=FALSE)
    cat(sprintf("  Saved: Percentile_Expected_Rank_%s.xlsx\n", tag))
  }
  cat("\n")
}


# ============================================================
# STEP 1 — LOAD BASE PANELS  (run once, independent of sample/method)
# ============================================================

cat("-------------------------------------------------------\n")
cat("STEP 1: Loading base panels & computing sample thresholds\n")
cat("-------------------------------------------------------\n\n")

child_file <- find_panel(extract_dir, paste0("Child_Panel", suffix))

# ── 1a. Children (cohort/age/year filter only — sample thresholds are
#        applied later, inside the per-sample loop) ──────────────────
cat("--- Children (base) ---\n")
child_panel_base <- read_panel(child_file)

if ("female" %in% names(child_panel_base)) {
  child_panel_base[, gender := 1L - as.integer(female)]
  child_panel_base[, female := NULL]
}
child_panel_base[, workerID   := as.character(workerID)]
if ("f_workerID" %in% names(child_panel_base))
  child_panel_base[, f_workerID := as.character(f_workerID)]
if ("m_workerID" %in% names(child_panel_base))
  child_panel_base[, m_workerID := as.character(m_workerID)]
cat(sprintf("  Loaded: %d worker-year observations\n", nrow(child_panel_base)))

n_before <- nrow(child_panel_base)
child_panel_base <- child_panel_base[
  yob  >= config$child_cohort_span[1] & yob  <= config$child_cohort_span[2] &
  age  >= config$child_age_span[1]    & age  <= config$child_age_span[2]    &
  year >= config$child_obs_year_min   & year <= config$child_obs_year_max]
cat(sprintf("  Cohort/age/year filter: %d -> %d obs (%d removed)\n",
            n_before, nrow(child_panel_base), n_before - nrow(child_panel_base)))

# ── 1b. AKM panel (year/age filter only) ─────────────────────────────
cat("\n--- AKM panel (base) ---\n")
akm_file     <- find_panel(extract_dir, paste0("AKM_Sample", suffix))
lm_akm_base  <- read_panel(akm_file)
cat(sprintf("  Loaded: %d obs\n", nrow(lm_akm_base)))

if ("female" %in% names(lm_akm_base)) {
  lm_akm_base[, gender := as.integer(as.integer(female) != config$female_code)]
  lm_akm_base[, female := NULL]
}
lm_akm_base[, workerID := as.character(workerID)]
if ("firmID" %in% names(lm_akm_base)) lm_akm_base[, firmID := as.character(firmID)]

lm_akm_base <- lm_akm_base[year >= config$akm_year_min & year <= config$akm_year_max]
cat(sprintf("  After year filter (%d-%d)   : %d obs\n",
            config$akm_year_min, config$akm_year_max, nrow(lm_akm_base)))
lm_akm_base <- lm_akm_base[age >= config$akm_age_span[1] & age <= config$akm_age_span[2]]
cat(sprintf("  After age filter (%d-%d)     : %d obs\n",
            config$akm_age_span[1], config$akm_age_span[2], nrow(lm_akm_base)))

# ── 1c. Compute the two threshold cutoffs from the AKM population ───
# Both are computed BEFORE the firm-size filter.
akm_earn_vec <- lm_akm_base[earning > 0 & is.finite(earning), earning]

thr1_cutoff <- config$thresholdPct1 * mean(akm_earn_vec, na.rm = TRUE)
thrQ_cutoff <- thr1_cutoff / config$thrQDenom

cat(sprintf("\n  Thr0325  cutoff (%.3f x mean AKM earning) : %.2f\n",
            config$thresholdPct1, thr1_cutoff))
cat(sprintf("  Thr0325Q cutoff (%.3f x mean AKM earning) : %.2f\n",
            config$thresholdPct1 / config$thrQDenom, thrQ_cutoff))

# ── 1d. Parents_Panel (replaces Father_Panel) — fathers + mothers,
#        kept as one pooled person-year table (parents_panel_raw) so
#        it can serve as the distribution the percentile transplant
#        (Step 1e) is applied to, as well as feed build_parent_out()
#        below for each sample. ───────────────────────────────────────
# Parents_Panel is a unified long panel of parent-year observations
# with p_female = 1 for mothers, 0 for fathers (Data Preparation Guide,
# 18 Sept 2026). Legacy Father_Panel.dta files (f_-prefixed, fathers
# only) are still auto-detected and used as a fallback so existing
# teams are not forced to re-extract their data immediately.
cat("\n--- Parents (base) ---\n")
parents_file   <- find_panel_opt(extract_dir, paste0("Parents_Panel", suffix))
legacy_parents <- is.null(parents_file)
if (legacy_parents) {
  cat("  Parents_Panel not found — falling back to legacy Father_Panel format.\n")
  parents_file <- find_panel(extract_dir, paste0("Father_Panel", suffix))
}
parents_panel_raw <- read_panel(parents_file)

if (legacy_parents) {
  # Legacy Father_Panel.dta is already f_-prefixed (workerID/yob/age/
  # year/earning -> f_workerID/f_yob/f_age/f_year/f_earning), fathers only.
  parents_panel_raw[, p_female := 0L]
  setnames(parents_panel_raw,
    old = c("f_workerID","f_yob","f_age","f_year","f_earning"),
    new = c("p_workerID","p_yob","p_age","p_year","p_earning"),
    skip_absent = TRUE)
}

setnames(parents_panel_raw,
  old = c("p_workerID","p_yob","p_age","p_year","p_earning"),
  new = c("workerID",   "yob",  "age",  "year",  "earning"),
  skip_absent = TRUE)
parents_panel_raw[, workerID := as.character(workerID)]
cat(sprintf("  Loaded: %d parent-year observations (fathers + mothers pooled)\n",
            nrow(parents_panel_raw)))

n_child_total  <- nrow(child_panel_base)
n_parent_total <- nrow(parents_panel_raw)
parent_earn_vec <- parents_panel_raw[earning > 0 & is.finite(earning), earning]

# ── 1e. Percentile transplant ────────────────────────────────────────
# There is no full-population parent-era earnings benchmark to apply a
# level-based (SEK) cutoff to directly, so for each child-side cutoff
# we find the percentile of the CHILD generation (the cohort/age/year-
# filtered Child_Panel, BEFORE any threshold) that it corresponds to,
# and then drop parent observations below that SAME percentile of
# whatever parent earnings distribution the country has
# (parents_panel_raw, fathers + mothers pooled). This only assumes the
# SHAPE of the earnings distribution is reasonably stable, not that
# absolute 1990s earnings levels are known.
child_earn_vec <- child_panel_base[earning > 0 & is.finite(earning), earning]

pct_thr1 <- mean(child_earn_vec <= thr1_cutoff, na.rm = TRUE)
pct_thrQ <- mean(child_earn_vec <= thrQ_cutoff, na.rm = TRUE)

parent_cutoff_thr1 <- as.numeric(quantile(parent_earn_vec, probs = pct_thr1, na.rm = TRUE, type = 7))
parent_cutoff_thrQ <- as.numeric(quantile(parent_earn_vec, probs = pct_thrQ, na.rm = TRUE, type = 7))

cat("\n  Percentile transplant (child cutoff -> child-population percentile -> parent cutoff):\n")
cat(sprintf("    Thr0325  : child cutoff %.2f -> percentile %.4f -> parent cutoff %.2f\n",
            thr1_cutoff, pct_thr1, parent_cutoff_thr1))
cat(sprintf("    Thr0325Q : child cutoff %.2f -> percentile %.4f -> parent cutoff %.2f\n",
            thrQ_cutoff, pct_thrQ, parent_cutoff_thrQ))

samples <- list(
  Thr0325 = list(type = "threshold", cutoff = thr1_cutoff, parent_cutoff = parent_cutoff_thr1,
                 label = sprintf("earning >= %.3f x mean AKM earning (both generations; parent via percentile transplant)",
                                 config$thresholdPct1)),
  Thr0325Q = list(type = "threshold", cutoff = thrQ_cutoff, parent_cutoff = parent_cutoff_thrQ,
                 label = sprintf("earning >= %.3f x mean AKM earning (both generations; parent via percentile transplant)",
                                 config$thresholdPct1 / config$thrQDenom))
)

cat(sprintf("\n  Samples to run: %s\n\n", paste(names(samples), collapse = ", ")))


# ============================================================
# STEP 2 — THRESHOLD ATTRITION TABLE (Excel)
#
# N obs / % of sample removed by each threshold, for both the child
# generation (Child_Panel, direct cutoff) and the parent generation
# (parents_panel_raw, percentile-transplanted cutoff).
# ============================================================

cat("-------------------------------------------------------\n")
cat("STEP 2: Threshold attrition (child + parent generations)\n")
cat("-------------------------------------------------------\n\n")

attrition_rows <- list()
for (sname in names(samples)) {
  s <- samples[[sname]]

  n_child_after <- nrow(child_panel_base[earning >= s$cutoff & is.finite(earning)])
  attrition_rows[[paste0(sname, "_child")]] <- data.table(
    Sample = sname, Generation = "Child", Cutoff_Earning = round(s$cutoff, 2),
    N_Before = n_child_total, N_After = n_child_after,
    N_Removed = n_child_total - n_child_after,
    Pct_Removed = round(100 * (n_child_total - n_child_after) / n_child_total, 2))

  n_parent_after <- nrow(parents_panel_raw[earning >= s$parent_cutoff & is.finite(earning)])
  attrition_rows[[paste0(sname, "_parent")]] <- data.table(
    Sample = sname, Generation = "Parent", Cutoff_Earning = round(s$parent_cutoff, 2),
    N_Before = n_parent_total, N_After = n_parent_after,
    N_Removed = n_parent_total - n_parent_after,
    Pct_Removed = round(100 * (n_parent_total - n_parent_after) / n_parent_total, 2))
}
attrition_tbl <- rbindlist(attrition_rows)
cat("Threshold attrition (child + parent):\n"); print(attrition_tbl)

wb_attr <- createWorkbook()
addWorksheet(wb_attr, "Threshold_Attrition")
writeData(wb_attr, "Threshold_Attrition", attrition_tbl)
addStyle(wb_attr, "Threshold_Attrition",
         createStyle(textDecoration = "bold", fgFill = "#1E3A5F", fontColour = "white", halign = "center"),
         rows = 1, cols = 1:ncol(attrition_tbl))
setColWidths(wb_attr, "Threshold_Attrition", cols = 1:ncol(attrition_tbl), widths = 16)
attr_path <- file.path(output_dir, "Threshold_Attrition.xlsx")
saveWorkbook(wb_attr, attr_path, overwrite = TRUE)
cat(sprintf("  Saved: %s\n\n", attr_path))


# ── 1f. Parent-side processing: child/parent linkage keys (independent
#        of sample/threshold, computed once) and the collapsing
#        functions. `earn_cutoff = NULL` reproduces the EXISTING rule
#        only (drop parents with >= 3 zero/missing income years, no
#        earning-level floor); a non-NULL `earn_cutoff` additionally
#        requires earning >= earn_cutoff (and finite) at the parent-
#        year level — the percentile-transplanted cutoff for that
#        sample, applied the same way the child-side threshold is
#        applied to Child_Panel. ──────────────────────────────────────
father_link <- unique(child_panel_base[
  !is.na(f_workerID), .(workerID_child = workerID, parentID = as.character(f_workerID))])
mother_link <- unique(child_panel_base[
  !is.na(m_workerID), .(workerID_child = workerID, parentID = as.character(m_workerID))])

build_parent_out <- function(sex_flag, gender_code, link, label, earn_cutoff = NULL) {

  pp <- parents_panel_raw[p_female == sex_flag]
  pp[, gender := gender_code]
  cat(sprintf("  Loaded: %d %s-year observations\n", nrow(pp), label))

  if (nrow(pp) == 0L) {
    return(data.table(workerID = character(0), logW = numeric(0),
                       logWResid = numeric(0), rank_earn = numeric(0),
                       own_workerID = character(0), yob = integer(0),
                       earning_mean = numeric(0)))
  }

  # Existing rule: remove parents with >= 3 years of zero or missing income
  n_bad   <- pp[, .(n_zero = sum(earning == 0 | !is.finite(earning),
                                 na.rm = TRUE)), by = workerID]
  bad_ids <- n_bad[n_zero >= 3L, workerID]
  if (length(bad_ids) > 0L) {
    pp <- pp[!workerID %in% bad_ids]
    cat(sprintf("  Dropped %d %ss with >= 3 zero/missing income years\n",
                length(bad_ids), label))
  }

  # Percentile-transplanted earning floor for this sample
  if (!is.null(earn_cutoff)) {
    n_pre <- nrow(pp)
    pp <- pp[earning >= earn_cutoff & is.finite(earning)]
    cat(sprintf("  Threshold filter (>= %.2f): %d -> %d %s-year obs\n",
                earn_cutoff, n_pre, nrow(pp), label))
  }

  if (nrow(pp) == 0L) {
    return(data.table(workerID = character(0), logW = numeric(0),
                       logWResid = numeric(0), rank_earn = numeric(0),
                       own_workerID = character(0), yob = integer(0),
                       earning_mean = numeric(0)))
  }

  processed <- process_panel(
    panel = pp,
    label = paste0(label, "s")
  )

  # Collapse to one row per parent, averaging over whatever years are
  # already present in Parents_Panel for that parent (i.e., whatever
  # 5-year window the country team chose at the data-prep stage — see
  # the config block above). No child-age-based re-filtering is applied.
  out <- processed[
    is.finite(logWResid),
    .(logW         = mean(logW,      na.rm = TRUE),
      logWResid    = mean(logWResid, na.rm = TRUE),
      rank_earn    = mean(rank_earn, na.rm = TRUE),
      earning_mean = mean(earning,   na.rm = TRUE),
      own_workerID = workerID[1],
      yob          = yob[1]),
    by = workerID]

  # `out` is keyed by the PARENT's own workerID so far; link it to
  # children now so both parent types end up keyed by the CHILD's
  # workerID (needed to compare father vs. mother per child).
  setnames(out, "workerID", "parentID")
  out <- merge(link, out, by = "parentID")
  setnames(out, "workerID_child", "workerID")
  out[, parentID := NULL]

  cat(sprintf("  %ss collapsed/linked to %d child-level rows\n", label, nrow(out)))
  out
}

# Combine father_raw + mother_raw into a single child-keyed table,
# keeping whichever parent earned more on average (raw earning level,
# not log/residualised earning); if only one parent is present/usable
# for a child, that one is used by default. `father_out`/`f_`-prefixed
# column names are kept so the rest of the script (IGM_Sample assembly,
# Step 2/3 decompositions, etc.) does not need to change — those
# columns now mean "the higher-earning parent", not literally "the
# father".
build_father_out <- function(earn_cutoff = NULL) {

  father_raw <- build_parent_out(0L, 1L, father_link, "father", earn_cutoff)
  mother_raw <- build_parent_out(1L, 0L, mother_link, "mother", earn_cutoff)

  setnames(father_raw, setdiff(names(father_raw), "workerID"),
           paste0("f_", setdiff(names(father_raw), "workerID")))
  setnames(mother_raw, setdiff(names(mother_raw), "workerID"),
           paste0("m_", setdiff(names(mother_raw), "workerID")))

  parent_combined <- merge(father_raw, mother_raw, by = "workerID", all = TRUE)

  parent_combined[, use_mother := !is.na(m_earning_mean) &
                     (is.na(f_earning_mean) | m_earning_mean > f_earning_mean)]

  father_out <- parent_combined[, .(
    workerID,
    f_logW      = fifelse(use_mother, m_logW,         f_logW),
    f_logWResid = fifelse(use_mother, m_logWResid,    f_logWResid),
    f_rank      = fifelse(use_mother, m_rank_earn,    f_rank_earn),
    f_workerID  = fifelse(use_mother, m_own_workerID, f_own_workerID),
    f_yob       = fifelse(use_mother, m_yob,          f_yob),
    f_earning   = fifelse(use_mother, m_earning_mean, f_earning_mean),
    parent_used = fifelse(use_mother, "mother", "father")
  )]

  cat(sprintf(
    "  Parents collapsed to %d individuals (%d use father, %d use mother)\n\n",
    nrow(father_out),
    sum(father_out$parent_used == "father"),
    sum(father_out$parent_used == "mother")))

  father_out
}

# One father_out per sample — the earning floor (via percentile
# transplant) differs by sample, so this can no longer be built once
# and reused across all samples the way it was when only children were
# thresholded.
father_out_by_sample <- list()
for (sname in names(samples)) {
  cat(sprintf("--- Parent-side table for sample: %s ---\n", sname))
  father_out_by_sample[[sname]] <- build_father_out(earn_cutoff = samples[[sname]]$parent_cutoff)
}
rm(parents_panel_raw)

# ── Methods to run on every sample ───────────────────────────────────
methods <- list(
  Std    = list(feVar = "firmID",    label = "Standard AKM",
               tag = "StdAKM"),
  Kmeans = list(feVar = "clusterID", label = sprintf("K-means AKM (K=%d)", config$K),
               tag = sprintf("KmeansAKM_K%d", config$K))
)

vardecomp_all <- list()


# ============================================================
# MAIN LOOP — for each SAMPLE, for each METHOD
# ============================================================

for (sname in names(samples)) {

  s <- samples[[sname]]

  cat("=========================================================\n")
  cat(sprintf("SAMPLE: %s  (%s)\n", sname, s$label))
  cat("=========================================================\n\n")

  # ── Filter Child_Panel for this sample ─────────────────────────────
  child_panel_s <- child_panel_base[earning >= s$cutoff & is.finite(earning)]
  cat(sprintf("  Child_Panel after sample filter: %d -> %d obs\n",
              nrow(child_panel_base), nrow(child_panel_s)))

  child_processed <- process_panel(
    panel = child_panel_s,
    label = "children"
  )
  child_earn <- child_processed[
    is.finite(logWResid),
    .(logW=mean(logW), logWResid=mean(logWResid), rank=mean(rank_earn)),
    by = workerID]

  child_demo_optional <- c("ed_ID", "immigrant_bg", "czID", "munID",
                            "industryID", "public", "m_workerID")
  child_demo_optional <- intersect(child_demo_optional, names(child_panel_s))
  # NOTE: child_demo's own father-link column is renamed to
  # `child_f_workerID` (rather than `f_workerID`) so it does not collide
  # with `father_out$f_workerID`, which after the father/mother
  # higher-earner selection below represents whichever PARENT was
  # actually selected (could be the mother's ID), not necessarily the
  # child's linked father.
  child_demo <- child_panel_s[
    !is.na(workerID),
    c(list(yob = yob[1], gender = gender[1], child_f_workerID = f_workerID[1]),
      lapply(mget(child_demo_optional), function(x) x[1])),
    by = workerID]
  child_out <- merge(child_demo, child_earn, by="workerID", all.x=TRUE)
  cat(sprintf("  Children collapsed to %d individuals\n", nrow(child_out)))
  rm(child_processed, child_earn, child_demo)

  # ── Assemble IGM_Sample for this sample ────────────────────────────
  # father_out_by_sample[[sname]] (from build_father_out()) is always
  # keyed by the CHILD's workerID, so a single merge suffices here. Each
  # sample has its own parent-side table because the percentile-
  # transplanted parent earning floor differs by sample.
  individualData <- merge(child_out, father_out_by_sample[[sname]], by="workerID", all.x=TRUE)

  IGM_Sample_s <- individualData[is.finite(logWResid) & is.finite(f_logWResid)]
  IGM_Sample_s[, rank   := frac_rank(rank),   by = .(gender, yob)]
  if ("f_yob" %in% names(IGM_Sample_s)) {
    IGM_Sample_s[, f_rank := frac_rank(f_rank), by = f_yob]
  } else {
    IGM_Sample_s[, f_rank := frac_rank(f_rank), by = yob]
  }
  cat(sprintf("  IGM_Sample (%s): %d individuals (rank mean = %.1f)\n",
              sname, nrow(IGM_Sample_s), mean(IGM_Sample_s$rank, na.rm=TRUE)))

  saveRDS(IGM_Sample_s, file.path(extract_dir, paste0("IGM_Sample_", sname, suffix, ".rds")))
  rm(child_out, individualData)

  # ── Filter AKM panel for this sample ───────────────────────────────
  lm_akm_s <- copy(lm_akm_base)
  lm_akm_s <- lm_akm_s[earning >= s$cutoff & is.finite(earning)]
  lm_akm_s[, logW := log(earning)]
  lm_akm_s <- lm_akm_s[is.finite(logW)]

  firm_sz     <- lm_akm_s[, .(n = uniqueN(workerID)), by = firmID]
  large_firms <- firm_sz[n >= config$min_firm_size, firmID]
  lm_akm_s    <- lm_akm_s[firmID %in% large_firms]
  cat(sprintf("  AKM panel after sample + firm-size filter: %d obs | %d workers | %d firms\n",
              nrow(lm_akm_s), uniqueN(lm_akm_s$workerID), uniqueN(lm_akm_s$firmID)))

  setorder(lm_akm_s, workerID, year)

  # ── For each method: (standard AKM, k-means/BLM AKM) ───────────────
  for (mname in names(methods)) {

    m   <- methods[[mname]]
    tag <- paste0(sname, "_", m$tag)

    cat(sprintf("\n---------------------------------------------------------\n"))
    cat(sprintf("METHOD: %s  |  SAMPLE: %s  ->  tag = %s\n", m$label, sname, tag))
    cat(sprintf("---------------------------------------------------------\n\n"))

    dt_m <- copy(lm_akm_s)

    if (m$feVar == "clusterID") {
      cat(sprintf("  Clustering firms into K=%d groups (BLM / Bonhomme et al.)...\n", config$K))
      clusters <- clusterFirms(dt_m, K = config$K, Nq = config$Nq,
                               nBootstrap = config$nBootstrap,
                               kMeansIter = config$kMeansIter,
                               seed = config$cluster_seed)
      dt_m <- merge(dt_m, clusters, by = "firmID", all.x = TRUE)
      dt_m <- dt_m[!is.na(clusterID)]
      cat(sprintf("  %d firms -> %d clusters\n", uniqueN(dt_m$firmID), uniqueN(dt_m$clusterID)))
    }

    res <- run_akm_method(dt_m, feVar = m$feVar,
                          child_age_span = config$child_age_span,
                          min_firm_size  = config$min_firm_size,
                          method_label   = m$label, tag = tag)

    # ── Merge FEs into IGMAKM_Sample ──────────────────────────────────
    IGMAKM_Sample <- copy(IGM_Sample_s)
    IGMAKM_Sample[, workerID := as.character(workerID)]
    IGMAKM_Sample <- merge(IGMAKM_Sample, res$alpha,      by="workerID", all.x=TRUE)
    IGMAKM_Sample <- merge(IGMAKM_Sample, res$psi,        by="workerID", all.x=TRUE)
    IGMAKM_Sample <- merge(IGMAKM_Sample, res$alpha_rank, by="workerID", all.x=TRUE)
    IGMAKM_Sample <- merge(IGMAKM_Sample, res$psi_rank,   by="workerID", all.x=TRUE)

    IGMAKM_Sample[, resIncAKM  := alpha      + psi      ]
    IGMAKM_Sample[, resRankAKM := alpha_rank + psi_rank  ]

    IGMAKM_Sample <- IGMAKM_Sample[is.finite(resIncAKM) & is.finite(resRankAKM)]
    IGMAKM_Sample <- IGMAKM_Sample[(1L - gender) %in% config$keep_gender]

    cat(sprintf("  IGMAKM_Sample (%s): %d individuals (%.1f%% of IGM_Sample)\n",
                tag, nrow(IGMAKM_Sample), 100*nrow(IGMAKM_Sample)/nrow(IGM_Sample_s)))

    saveRDS(IGMAKM_Sample,
            file.path(extract_dir, paste0("IGMAKM_Sample_", tag, suffix, ".rds")))

    # ── Binscatter plots ───────────────────────────────────────────────
    make_binscatters(IGMAKM_Sample, tag)

    # ── SUR decomposition (Steps 2-4) ───────────────────────────────────
    run_decomp(IGMAKM_Sample,
               total_var = "resIncAKM",
               alpha_var = "alpha",
               psi_var   = "psi",
               prefix    = tag,
               run_rank  = TRUE)

    # ── Plug-in variance decomposition row ──────────────────────────────
    vardecomp_all[[tag]] <- data.table(sample = sname, res$vardecomp_row)

    rm(dt_m, res, IGMAKM_Sample); gc()
  }

  rm(lm_akm_s, IGM_Sample_s); gc()
}


# ============================================================
# WAGE VARIANCE DECOMPOSITION  (Excel, all samples x methods)
# ============================================================

cat("\n--- Plug-in wage variance decomposition (all samples x methods) ---\n")
vardecomp_tbl <- rbindlist(vardecomp_all)
print(vardecomp_tbl)

wb <- createWorkbook()
addWorksheet(wb, "VarDecomp")
writeData(wb, "VarDecomp",
          "Plug-in Wage Variance Decomposition — one row per sample x method",
          startRow=1, startCol=1)
writeData(wb, "VarDecomp", vardecomp_tbl, startRow=3, startCol=1)
addStyle(wb,"VarDecomp",createStyle(textDecoration="bold",fontSize=12),rows=1,cols=1)
addStyle(wb,"VarDecomp",
         createStyle(textDecoration="bold",fgFill="#1E3A5F",fontColour="white",halign="center"),
         rows=3,cols=1:ncol(vardecomp_tbl))
setColWidths(wb,"VarDecomp",cols=1:ncol(vardecomp_tbl),widths=18)
xp <- file.path(output_dir,"VarDecomp.xlsx")
saveWorkbook(wb, xp, overwrite=TRUE)
cat(sprintf("  Saved: %s\n", xp))


# ============================================================
# DONE
# ============================================================

elapsed <- proc.time() - t_start
cat(sprintf("\nTotal runtime: %.1f min (%.0f sec)\n",
            elapsed["elapsed"]/60, elapsed["elapsed"]))

cat("\n=======================================================\n")
cat("Firms Main Script.R complete.\n")
cat(sprintf("  Spec    : %s\n", config$spec))
cat(sprintf("  Output  : %s\n", output_dir))
cat(sprintf("  Samples : %s\n", paste(names(samples), collapse=", ")))
cat(sprintf("  Methods : Standard AKM, K-means AKM (K=%d)\n", config$K))
cat("=======================================================\n")
