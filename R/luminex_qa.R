# Luminex cell-culture quality assurance ---------------------------------------
#
# luminex_qa()          runs every check and returns a "luminex_qa" object
# luminex_contrasts()   the "A should be higher than B" comparisons
# luminex_dose_series() the inhibitor dose series (cortisol, IL-10)
# luminex_qa_report()   (see luminex_qa_report.R) renders the HTML report


#' Expected stimulation / inhibition comparisons for Luminex QA
#'
#' Each row is one check: within a participant, the `higher` well should have
#' more cytokine than the `lower` well. A participant is flagged when the
#' reverse is true. `tier = "sensitive"` marks the low-dose checks (crt_6.5,
#' crt_7, the lowest IL-10 dose), where noise alone often produces small
#' reversals; these are kept out of the summary and listed separately
#' (`qa$low_dose`, the "Just in case" section of the report).
#'
#' Condition names must match the condition part of the sample id exactly
#' (lower case), e.g. `MHS_1000_V1_crt_5.5` -> `crt_5.5`.
#'
#' @param hsp60,r848 Does the study have HSP60 / R848 wells? Default `TRUE`.
#' @param il10 Does the study have IL-10 wells (`il10_.02`, `il10_.1`,
#'   `il10_.5`)? Default `FALSE`.
#' @return A data frame with columns `higher`, `lower`, `tier`, `test`.
#' @examples
#' luminex_contrasts()
#' luminex_contrasts(il10 = TRUE)
#' @export
luminex_contrasts <- function(hsp60 = TRUE, r848 = TRUE, il10 = FALSE) {
  out <- data.frame(
    higher = c("hsp60", "r848", "lps", "lps",     "lps",   "lps",       "lps"),
    lower  = c("us",    "us",   "us",  "crt_5.5", "crt_6", "crt_6.5",   "crt_7"),
    tier   = c("core",  "core", "core", "core",   "core",  "sensitive", "sensitive"),
    stringsAsFactors = FALSE
  )
  if (!isTRUE(hsp60)) out <- out[out$higher != "hsp60", ]
  if (!isTRUE(r848)) out <- out[out$higher != "r848", ]
  if (isTRUE(il10)) {
    out <- rbind(out, data.frame(
      higher = "lps", lower = c("il10_.5", "il10_.1", "il10_.02"),
      tier = c("core", "core", "sensitive"), stringsAsFactors = FALSE))
  }
  out$test <- paste0(out$lower, " > ", out$higher)
  rownames(out) <- NULL
  out
}


#' Inhibitor dose series for Luminex QA
#'
#' Describes each inhibitor's wells so the report can check that cytokine
#' goes down as the dose goes up. `doses` is a named vector: names are the
#' condition labels, values are log10 concentrations (larger = more inhibitor).
#' `reference` is the stimulated-only well the inhibitor is added to.
#'
#' Cortisol labels are -log10 molar, so `crt_5.5` (10^-5.5 M) is the highest
#' dose. IL-10 labels are the concentration, so `il10_.5` is the highest dose.
#'
#' @param il10 Include the IL-10 series? Default `FALSE`.
#' @return A named list; each element has `reference`, `doses`, `unit`.
#' @examples
#' luminex_dose_series(il10 = TRUE)
#' @export
luminex_dose_series <- function(il10 = FALSE) {
  out <- list(
    cortisol = list(reference = "lps",
                    doses = c(crt_7 = -7, crt_6.5 = -6.5, crt_6 = -6, crt_5.5 = -5.5),
                    unit = "log10 M")
  )
  if (isTRUE(il10)) {
    out$il10 <- list(reference = "lps",
                     doses = c(il10_.02 = log10(0.02), il10_.1 = log10(0.1), il10_.5 = log10(0.5)),
                     unit = "log10 concentration")
  }
  out
}


#' Run Luminex cell-culture quality assurance checks
#'
#' Reads a master sheet (the "for master" or "final master" tab produced by
#' [luminex()]) and runs every QA check, returning the results as tables.
#' Nothing is written to disk; pass the result to [luminex_qa_report()] for
#' the HTML report, or look at the pieces directly (e.g. `qa$summary`).
#'
#' Sample ids must look like `STUDY_IDNUM_VISIT_condition`, e.g.
#' `MHS_1000_V1_crt_5.5` or `OTR_101_V3_il10_.1`: study letters, a 3- or
#' 4-digit id number, `V` plus the visit number, then the condition.
#' This is the pre-repeats QA: run it on the "for master" tab before any
#' repeats are done. If rows ending in `_repeat`, `_repeat2`, ... or
#' `_interassay` are present they are set aside and only the original run is
#' checked.
#'
#' @param data Path to the master `.xlsx`, or a data frame already read in.
#' @param sheet Sheet name when `data` is a path (default `"for master"`).
#' @param hsp60,r848 Does the study have HSP60 / R848 wells? Default `TRUE`.
#' @param il10 Does the study have IL-10 wells? Default `FALSE`.
#' @param analytes Cytokines to check. Default: detected from the
#'   `*_avg_mfi` columns.
#' @param contrasts,dose_series Advanced: replace the default checks
#'   (see [luminex_contrasts()], [luminex_dose_series()]). When given, these
#'   override `hsp60`, `r848` and `il10`.
#' @param expected_ids Optional vector of participant id numbers that should
#'   be present (e.g. everyone with a blood draw). Any not found are listed.
#' @param measure Which value to compare: `"avg_mfi"` (default) or
#'   `"avg_conc"`.
#' @param cv_threshold Replicate MFI CV (\%) above which a well is noted as
#'   high CV in the flag details (default 10).
#' @param tolerance Percent. A comparison is flagged only when the expected
#'   higher well is more than `tolerance`\% below the other. Default 0 (any
#'   reversal), matching the original script.
#' @param study,visit Labels for the report. Default: taken from the ids.
#' @return An object of class `luminex_qa` (a list of data frames).
#' @examples
#' f <- tempfile(fileext = ".xlsx")
#' luminex_example(f)
#' qa <- luminex_qa(f, il10 = TRUE)
#' qa
#' head(qa$summary)
#' @export
luminex_qa <- function(data,
                       sheet = "for master",
                       hsp60 = TRUE,
                       r848 = TRUE,
                       il10 = FALSE,
                       analytes = NULL,
                       contrasts = NULL,
                       dose_series = NULL,
                       expected_ids = NULL,
                       measure = "avg_mfi",
                       cv_threshold = 10,
                       tolerance = 0,
                       study = NULL,
                       visit = NULL) {

  if (is.null(contrasts)) contrasts <- luminex_contrasts(hsp60 = hsp60, r848 = r848, il10 = il10)
  if (is.null(dose_series)) dose_series <- luminex_dose_series(il10 = il10)

  source_label <- if (is.character(data)) basename(data) else "data frame"
  if (is.character(data)) {
    if (!file.exists(data)) stop("File not found: ", data, call. = FALSE)
    raw <- openxlsx::read.xlsx(data, sheet = sheet)
  } else {
    raw <- as.data.frame(data, stringsAsFactors = FALSE)
    sheet <- NA_character_
  }
  if (!"id" %in% names(raw)) stop("The sheet needs an `id` column.", call. = FALSE)
  raw <- raw[!is.na(raw$id) & trimws(raw$id) != "", , drop = FALSE]
  raw$id <- trimws(as.character(raw$id))
  raw$.row <- seq_len(nrow(raw)) + 1   # excel row number (header is row 1)
  if (!"plate" %in% names(raw)) raw$plate <- NA_character_

  if (is.null(analytes)) {
    analytes <- sub("_avg_mfi$", "", grep("_avg_mfi$", names(raw), value = TRUE))
  }
  if (length(analytes) == 0) stop("No `*_avg_mfi` columns found.", call. = FALSE)
  miss_cols <- setdiff(paste0(analytes, "_", measure), names(raw))
  if (length(miss_cols)) stop("Missing columns: ", paste(miss_cols, collapse = ", "), call. = FALSE)

  # ---- parse ids ------------------------------------------------------------
  ids <- .lqa_parse_ids(raw$id)
  raw <- cbind(raw, ids[, c("study", "id_num", "visit", "condition", "round", "parsed")])

  unparsed <- raw[!raw$parsed, c(".row", "id", "plate")]
  names(unparsed)[1] <- "excel_row"
  raw <- raw[raw$parsed, , drop = FALSE]

  # ---- wells switched off (e.g. il10 = FALSE) but present in the file ---------
  off <- character(0)
  if (!isTRUE(hsp60)) off <- c(off, "hsp60")
  if (!isTRUE(r848)) off <- c(off, "r848")
  is_off <- raw$condition %in% off | (!isTRUE(il10) & grepl("^il10", raw$condition))
  switched_off <- if (any(is_off)) sort(unique(raw$condition[is_off])) else character(0)
  raw <- raw[!is_off, , drop = FALSE]

  visits <- sort(unique(raw$visit))
  if (length(visits) > 1) raw$id_num <- paste0(raw$id_num, "_", raw$visit)
  if (is.null(study)) study <- .lqa_mode(raw$study)
  if (is.null(visit)) visit <- paste(visits, collapse = " & ")

  # ---- long format ----------------------------------------------------------
  long <- .lqa_long(raw, analytes, measure)
  orig <- long[long$round == "original", , drop = FALSE]
  extra <- unique(raw[raw$round != "original", c(".row", "id", "id_num", "condition", "round", "plate")])
  names(extra)[1] <- "excel_row"

  # ---- which conditions to expect -------------------------------------------
  configured <- unique(c(contrasts$higher, contrasts$lower,
                         unlist(lapply(dose_series, function(s) c(s$reference, names(s$doses))))))
  seen <- unique(orig$condition)
  expected_conditions <- intersect(configured, seen)
  never_seen <- setdiff(configured, seen)
  contrasts <- contrasts[contrasts$higher %in% expected_conditions &
                           contrasts$lower %in% expected_conditions, , drop = FALSE]
  dose_series <- Filter(function(s) sum(names(s$doses) %in% seen) >= 2 && s$reference %in% seen,
                        dose_series)
  for (nm in names(dose_series)) {
    dose_series[[nm]]$doses <- dose_series[[nm]]$doses[names(dose_series[[nm]]$doses) %in% seen]
  }

  # ---- checks -----------------------------------------------------------------
  integrity <- .lqa_integrity(raw, expected_conditions, unparsed, study, visit)
  # one value per participant x condition x analyte (first original row)
  vals <- orig[!duplicated(orig[, c("id_num", "condition", "analyte")]), , drop = FALSE]
  missing <- .lqa_missing(vals, expected_conditions, analytes, expected_ids)
  cmp <- .lqa_contrasts(vals, contrasts, tolerance, cv_threshold, extra)
  dose <- .lqa_dose(vals, dose_series)
  patterns <- .lqa_patterns(vals, cmp, contrasts, dose_series, analytes)
  plates <- .lqa_plates(orig, cmp)

  out <- list(
    settings = list(source = source_label, sheet = sheet, study = study, visit = visit,
                    hsp60 = hsp60, r848 = r848, il10 = il10,
                    analytes = analytes, measure = measure, cv_threshold = cv_threshold,
                    tolerance = tolerance, contrasts = contrasts, dose_series = dose_series,
                    expected_conditions = expected_conditions, never_seen = never_seen,
                    switched_off = switched_off, expected_ids = expected_ids, run_at = Sys.time(),
                    fhrc_version = as.character(tryCatch(utils::packageVersion("fhrc"),
                                                         error = function(e) "dev"))),
    counts = list(rows = nrow(raw) + nrow(unparsed) + sum(is_off),
                  participants = length(unique(raw$id_num)),
                  plates = length(unique(stats::na.omit(raw$plate))), extra_rows = nrow(extra)),
    data = raw, long = orig, values = vals, extra_rows = extra,
    integrity = integrity, missing = missing,
    contrasts = cmp, dose = dose, patterns = patterns, plates = plates
  )
  out$summary <- .lqa_summary(out)
  out$low_dose <- .lqa_low_dose(out)
  class(out) <- c("luminex_qa", "list")
  out
}


#' @export
print.luminex_qa <- function(x, ...) {
  s <- x$settings
  cat("f h r c | Luminex QA -", s$study, s$visit, "\n")
  cat("  source:      ", s$source, if (!is.na(s$sheet)) paste0("[", s$sheet, "]"), "\n")
  cat("  participants:", x$counts$participants, " rows:", x$counts$rows,
      " plates:", x$counts$plates, " repeat/interassay rows set aside:", x$counts$extra_rows, "\n")
  sm <- x$summary
  if (nrow(sm) == 0) {
    cat("  no problems found.\n")
  } else {
    cat("  problems listed:", nrow(sm), " participants involved:",
        length(unique(sm$participant[grepl("^[0-9]+$", sm$participant)])), "\n")
  }
  cat("  see qa$summary, or run luminex_qa_report(qa) for the full report.\n")
  invisible(x)
}


# ---- internal helpers ----------------------------------------------------------

.lqa_mode <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

.lqa_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

.lqa_parse_ids <- function(id) {
  pat <- "^([A-Za-z]+)_([0-9]{3,4})_([Vv][0-9]+)_(.+?)(?:_((?i)repeat[0-9]*|interassay))?$"
  m <- regmatches(id, regexec(pat, id, perl = TRUE))
  ok <- lengths(m) == 6
  get <- function(k) vapply(seq_along(m), function(i) if (ok[i]) m[[i]][k] else NA_character_, "")
  rnd <- tolower(get(6))
  rnd[ok & (is.na(rnd) | rnd == "")] <- "original"
  data.frame(study = get(2), id_num = get(3), visit = toupper(get(4)),
             condition = tolower(trimws(get(5))), round = rnd, parsed = ok,
             stringsAsFactors = FALSE)
}

.lqa_long <- function(raw, analytes, measure) {
  pieces <- lapply(analytes, function(a) {
    col <- function(suffix) {
      nm <- paste0(a, suffix)
      if (nm %in% names(raw)) raw[[nm]] else rep(NA, nrow(raw))
    }
    conc_raw <- as.character(col("_avg_conc"))
    data.frame(
      excel_row = raw$.row, id = raw$id, id_num = raw$id_num, condition = raw$condition,
      round = raw$round, plate = as.character(raw$plate), analyte = a,
      value = .lqa_num(col(paste0("_", measure))),
      mfi = .lqa_num(col("_avg_mfi")),
      rep1 = .lqa_num(col("_1_mfi")), rep2 = .lqa_num(col("_2_mfi")),
      cv = .lqa_num(col("_mfi_cv")),
      conc_raw = conc_raw, conc = .lqa_num(conc_raw),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, pieces)
}

.lqa_integrity <- function(raw, expected_conditions, unparsed, study, visit) {
  orig <- raw[raw$round == "original", , drop = FALSE]
  key <- paste(orig$id_num, orig$condition)
  dup_keys <- unique(key[duplicated(key)])
  duplicates <- orig[key %in% dup_keys, c(".row", "id", "id_num", "condition", "plate")]
  names(duplicates)[1] <- "excel_row"
  duplicates <- duplicates[order(duplicates$id_num, duplicates$condition), , drop = FALSE]
  if (nrow(duplicates)) {
    num_cols <- grep("_avg_mfi$", names(orig), value = TRUE)
    sig <- apply(orig[key %in% dup_keys, num_cols, drop = FALSE], 1, paste, collapse = "|")
    k2 <- key[key %in% dup_keys]
    duplicates$identical_values <- stats::ave(sig, k2, FUN = function(s) length(unique(s)) == 1)
    duplicates$identical_values <- duplicates$identical_values == "TRUE"
  }

  unexpected <- raw[!raw$condition %in% expected_conditions, c(".row", "id", "id_num", "condition", "plate")]
  names(unexpected)[1] <- "excel_row"

  vm <- raw[raw$study != study, c(".row", "id", "id_num", "study", "visit", "plate")]
  names(vm)[1] <- "excel_row"

  list(unparsed = unparsed, duplicates = duplicates, unexpected = unexpected, label_mismatch = vm)
}

.lqa_missing <- function(vals, expected_conditions, analytes, expected_ids) {
  people <- sort(unique(vals$id_num))
  out <- list()
  have <- unique(vals[, c("id_num", "condition")])
  for (p in people) {
    got <- have$condition[have$id_num == p]
    lack <- setdiff(expected_conditions, got)
    if (length(lack)) out[[length(out) + 1]] <- data.frame(
      id_num = p, condition = lack, analyte = "all", issue = "condition missing",
      stringsAsFactors = FALSE)
  }
  na <- vals[is.na(vals$value) & vals$condition %in% expected_conditions, ]
  if (nrow(na)) out[[length(out) + 1]] <- data.frame(
    id_num = na$id_num, condition = na$condition, analyte = na$analyte,
    issue = "value missing", stringsAsFactors = FALSE)
  if (!is.null(expected_ids)) {
    exp_ids <- as.character(expected_ids)
    people_num <- sub("_.*$", "", people)
    notfound <- setdiff(exp_ids, people_num)
    if (length(notfound)) out[[length(out) + 1]] <- data.frame(
      id_num = notfound, condition = "all", analyte = "all",
      issue = "expected participant not in data", stringsAsFactors = FALSE)
    extra <- people[!people_num %in% exp_ids]
    if (length(extra)) out[[length(out) + 1]] <- data.frame(
      id_num = extra, condition = "all", analyte = "all",
      issue = "in data but not in expected_ids", stringsAsFactors = FALSE)
  }
  res <- if (length(out)) do.call(rbind, out) else
    data.frame(id_num = character(0), condition = character(0), analyte = character(0),
               issue = character(0), stringsAsFactors = FALSE)
  res[order(.lqa_idnum(res$id_num), res$id_num, res$condition), , drop = FALSE]
}

# numeric part of a participant key ("1003" or "1003_V2") for sorting
.lqa_idnum <- function(x) suppressWarnings(as.numeric(sub("_.*$", "", x)))

.lqa_contrasts <- function(vals, contrasts, tolerance, cv_threshold, extra) {
  if (!nrow(contrasts)) return(data.frame())
  key <- paste(vals$id_num, vals$condition, vals$analyte)
  rep_key <- unique(paste(extra$id_num, extra$condition))
  people <- unique(vals[, c("id_num", "plate")])
  people <- people[!duplicated(people$id_num), ]
  res <- list()
  for (i in seq_len(nrow(contrasts))) {
    ct <- contrasts[i, ]
    for (a in unique(vals$analyte)) {
      hi <- vals[match(paste(people$id_num, ct$higher, a), key), ]
      lo <- vals[match(paste(people$id_num, ct$lower, a), key), ]
      res[[length(res) + 1]] <- data.frame(
        id_num = people$id_num, plate = people$plate, analyte = a,
        test = ct$test, higher = ct$higher, lower = ct$lower, tier = ct$tier,
        higher_value = hi$value, lower_value = lo$value,
        higher_cv = hi$cv, lower_cv = lo$cv, stringsAsFactors = FALSE)
    }
  }
  d <- do.call(rbind, res)
  d$diff <- d$higher_value - d$lower_value
  d$ratio <- d$higher_value / d$lower_value
  d$reversal_pct <- ifelse(d$diff < 0, 100 * (d$lower_value - d$higher_value) / d$lower_value, 0)
  d$flag <- !is.na(d$diff) & d$higher_value < d$lower_value * (1 - tolerance / 100)
  noise <- pmax(d$higher_cv, d$lower_cv, na.rm = TRUE)
  d$within_noise <- d$flag & !is.na(noise) & d$reversal_pct <= noise
  d$high_cv <- (!is.na(d$higher_cv) & d$higher_cv > cv_threshold) |
    (!is.na(d$lower_cv) & d$lower_cv > cv_threshold)
  d$repeat_exists <- paste(d$id_num, d$higher) %in% rep_key | paste(d$id_num, d$lower) %in% rep_key
  d[order(d$test, d$analyte, d$id_num), , drop = FALSE]
}

.lqa_slope <- function(x, y) {
  ok <- !is.na(y) & y > 0
  if (sum(ok) < 3) return(NA_real_)
  x <- x[ok]; ly <- log10(y[ok])
  sum((x - mean(x)) * (ly - mean(ly))) / sum((x - mean(x))^2)
}

.lqa_dose <- function(vals, dose_series) {
  if (!length(dose_series)) return(data.frame())
  key <- paste(vals$id_num, vals$condition, vals$analyte)
  people <- unique(vals[, c("id_num", "plate")])
  people <- people[!duplicated(people$id_num), ]
  res <- list()
  for (nm in names(dose_series)) {
    s <- dose_series[[nm]]
    doses <- sort(s$doses)
    for (a in unique(vals$analyte)) {
      for (j in seq_len(nrow(people))) {
        p <- people$id_num[j]
        y <- vals$value[match(paste(p, names(doses), a), key)]
        ref <- vals$value[match(paste(p, s$reference, a), key)]
        n_ok <- sum(!is.na(y) & y > 0)
        slope <- .lqa_slope(unname(doses), y)
        rho <- if (n_ok >= 3) suppressWarnings(stats::cor(unname(doses)[!is.na(y)], y[!is.na(y)],
                                                          method = "spearman")) else NA_real_
        dy <- diff(y)
        res[[length(res) + 1]] <- data.frame(
          id_num = p, plate = people$plate[j], series = nm, analyte = a,
          n_doses = n_ok, reference_value = ref,
          slope = slope, spearman = rho,
          increases = sum(dy > 0, na.rm = TRUE),
          suppression_top_pct = 100 * (1 - y[length(y)] / ref),
          values = paste(round(y), collapse = " / "),
          stringsAsFactors = FALSE)
      }
    }
  }
  d <- do.call(rbind, res)
  d$flag <- ifelse(d$n_doses < 3, "too few doses for a slope",
                   ifelse(!is.na(d$slope) & d$slope >= 0, "no inhibition (slope >= 0)", ""))
  # inhibition at the top dose, compared with everyone else on the same series x cytokine
  d$cohort_median_suppression <- stats::ave(d$suppression_top_pct, d$series, d$analyte,
                                            FUN = function(x) stats::median(x, na.rm = TRUE))
  d$suppression_z <- stats::ave(d$suppression_top_pct, d$series, d$analyte, FUN = function(x) {
    s <- stats::mad(x, na.rm = TRUE); if (!is.finite(s) || s == 0) s <- stats::sd(x, na.rm = TRUE)
    (x - stats::median(x, na.rm = TRUE)) / s
  })
  d$weak_vs_cohort <- !is.na(d$suppression_z) & d$suppression_z < -3 &
    d$suppression_top_pct < d$cohort_median_suppression / 2
  d$series_conds <- vapply(d$series, function(nm)
    paste(c(dose_series[[nm]]$reference, names(dose_series[[nm]]$doses)), collapse = ","), "")
  d
}

# count "core" violations for one participant's condition x analyte matrix
.lqa_violations <- function(m, core, dose_series) {
  v <- 0
  for (i in seq_len(nrow(core))) {
    h <- m[core$higher[i], ]; l <- m[core$lower[i], ]
    v <- v + sum(h < l, na.rm = TRUE)
  }
  for (s in dose_series) {
    doses <- sort(s$doses)
    for (a in colnames(m)) {
      y <- m[names(doses), a]
      sl <- .lqa_slope(unname(doses), y)
      if (!is.na(sl) && sl >= 0) v <- v + 1
      # a step up of more than 25% when the dose goes up is implausible
      v <- v + sum(y[-1] > 1.25 * y[-length(y)], na.rm = TRUE)
    }
  }
  v
}

.lqa_patterns <- function(vals, cmp, contrasts, dose_series, analytes) {
  empty <- data.frame(id_num = character(0), pattern = character(0), detail = character(0),
                      stringsAsFactors = FALSE)
  if (!nrow(cmp)) return(list(systematic = empty, swaps = empty, weak = empty))

  # 1. same comparison reversed in every cytokine
  f <- cmp[!is.na(cmp$diff), ]
  if (!nrow(f)) {
    empty_sys <- cbind(empty, tier = character(0), test = character(0), conds = character(0))
    return(list(systematic = empty_sys, swaps = cbind(empty, conds = character(0)),
                weak = cbind(empty, conds = character(0))))
  }
  agg <- stats::aggregate(cbind(n = 1, flagged = f$flag) ~ id_num + test + tier + higher + lower,
                          data = f, FUN = sum)
  sysm <- agg[agg$n >= 2 & agg$flagged == agg$n, ]
  systematic <- if (nrow(sysm)) data.frame(
    id_num = sysm$id_num, pattern = "reversed in every cytokine",
    detail = paste0(sysm$test, " in all ", sysm$n, " cytokines", ifelse(sysm$tier == "sensitive", " (sensitive check)", "")),
    tier = sysm$tier, test = sysm$test, conds = paste(sysm$lower, sysm$higher, sep = ","),
    stringsAsFactors = FALSE) else
      cbind(empty, tier = character(0), test = character(0), conds = character(0))

  # 2. weak response to a stimulant in every cytokine (stimulant not added?)
  stim <- contrasts[contrasts$lower == "us", , drop = FALSE]
  weak <- empty
  if (nrow(stim)) {
    w <- cmp[cmp$test %in% stim$test & !is.na(cmp$ratio) & cmp$ratio > 0, ]
    w$lf <- log2(w$ratio)
    w$z <- stats::ave(w$lf, w$test, w$analyte, FUN = function(x) {
      s <- stats::mad(x, na.rm = TRUE); if (!is.finite(s) || s == 0) s <- stats::sd(x, na.rm = TRUE)
      (x - stats::median(x, na.rm = TRUE)) / s
    })
    grp <- split(w, list(w$id_num, w$higher), drop = TRUE)
    hit <- do.call(rbind, lapply(grp, function(g) {
      if (nrow(g) >= 2 && all(g$z < -3)) data.frame(id_num = g$id_num[1], higher = g$higher[1],
                                                    fold = max(g$ratio), stringsAsFactors = FALSE)
    }))
    if (!is.null(hit) && nrow(hit)) {
      n_weak <- table(hit$id_num)
      us_like <- names(n_weak)[n_weak >= 2]
      one <- hit[!hit$id_num %in% us_like, ]
      weak <- data.frame(
        id_num = one$id_num, pattern = "stimulant possibly not added",
        detail = paste0(one$higher, " barely above US in every cytokine (at most ",
                        round(one$fold, 1), "x) - stimulant possibly not added"),
        conds = one$higher, stringsAsFactors = FALSE)
      if (length(us_like)) {
        u <- hit[hit$id_num %in% us_like, ]
        u <- stats::aggregate(higher ~ id_num, u, function(z) paste(z, collapse = ", "))
        weak <- rbind(weak, data.frame(
          id_num = u$id_num, pattern = "US well looks stimulated",
          detail = paste0("US is as high as the stimulated wells (", u$higher,
                          ") in every cytokine - US mislabelled or contaminated?"),
          conds = "us", stringsAsFactors = FALSE))
      }
    }
  }
  if (!"conds" %in% names(weak)) weak$conds <- character(0)

  # 3. would swapping two wells' labels explain the flags?
  core <- contrasts[contrasts$tier == "core", , drop = FALSE]
  swaps <- empty
  key_people <- unique(vals$id_num)
  for (p in key_people) {
    v <- vals[vals$id_num == p, ]
    conds <- unique(v$condition)
    m <- matrix(NA_real_, length(conds), length(analytes), dimnames = list(conds, analytes))
    m[cbind(v$condition, v$analyte)] <- v$value
    need <- unique(c(core$higher, core$lower, unlist(lapply(dose_series, function(s) names(s$doses)))))
    for (nn in setdiff(need, conds)) m <- rbind(m, matrix(NA_real_, 1, length(analytes), dimnames = list(nn, analytes)))
    base <- .lqa_violations(m, core, dose_series)
    if (base < 3) next
    best <- base; best_pair <- NULL
    pairs <- utils::combn(conds, 2)
    for (k in seq_len(ncol(pairs))) {
      m2 <- m; m2[pairs[, k], ] <- m[rev(pairs[, k]), ]
      sc <- .lqa_violations(m2, core, dose_series)
      if (sc < best) { best <- sc; best_pair <- pairs[, k] }
    }
    if (!is.null(best_pair) && best <= 1 && best <= base - 3) {
      swaps <- rbind(swaps, data.frame(
        id_num = p, pattern = "possible label swap",
        detail = paste0(best_pair[1], " <-> ", best_pair[2], ": swapping these two wells drops the ",
                        "violations from ", base, " to ", best, " - check the plate map"),
        conds = paste(best_pair, collapse = ","), stringsAsFactors = FALSE))
    }
  }
  if (!"conds" %in% names(swaps)) swaps$conds <- character(0)

  list(systematic = systematic, swaps = swaps, weak = weak)
}

.lqa_plates <- function(orig, cmp) {
  d <- orig[!is.na(orig$value) & orig$value > 0 & !is.na(orig$plate), ]
  if (!nrow(d)) return(list(summary = data.frame(), shift = data.frame()))
  d$lv <- log2(d$value)
  overall <- stats::aggregate(lv ~ analyte + condition, d, stats::median)
  per <- stats::aggregate(lv ~ plate + analyte + condition, d, stats::median)
  per$dev <- per$lv - overall$lv[match(paste(per$analyte, per$condition), paste(overall$analyte, overall$condition))]
  shift <- stats::aggregate(dev ~ plate, per, stats::median)
  names(shift)[2] <- "median_log2_shift"
  shift$fold <- 2^shift$median_log2_shift

  people <- stats::aggregate(id_num ~ plate, unique(orig[, c("plate", "id_num")]), length)
  names(people)[2] <- "participants"
  core <- cmp[nrow(cmp) > 0 & cmp$tier == "core", ]
  fl <- if (nrow(core) && any(!is.na(core$plate))) stats::aggregate(flag ~ plate + id_num, core, any) else data.frame(plate = character(0), flag = logical(0))
  fl <- if (nrow(fl)) stats::aggregate(flag ~ plate, fl, sum) else data.frame(plate = character(0), flag = numeric(0))

  s <- merge(people, shift, by = "plate", all.x = TRUE)
  s <- merge(s, fl, by = "plate", all.x = TRUE)
  names(s)[names(s) == "flag"] <- "participants_flagged"
  s$participants_flagged[is.na(s$participants_flagged)] <- 0
  s$plate_flag <- ifelse(abs(s$median_log2_shift) >= 1,
                         paste0("values run ", round(ifelse(s$fold < 1, 1 / s$fold, s$fold), 1), "x ",
                                ifelse(s$fold < 1, "lower", "higher"), " than other plates"), "")
  list(summary = s[order(s$plate), ], by_condition = per)
}

# issue label for a comparison: "us > hsp60" stays as is; inhibitor wells are
# grouped by inhibitor, e.g. crt_5.5 > lps -> "crt > lps", il10_.1 > lps -> "il10 > lps"
.lqa_family <- function(lower, higher) {
  ifelse(lower == "us", paste0(lower, " > ", higher), paste0(sub("_.*$", "", lower), " > ", higher))
}

# issues that are about the data file rather than the measured values
.lqa_file_issues <- c("set-up", "unreadable id", "duplicated data", "unknown condition",
                      "different study prefix", "missing data", "plate")

.lqa_summary <- function(x) {
  S <- x$settings
  rows <- list()
  add <- function(p, issue, detail, plate = NA_character_) {
    if (!length(p)) return(invisible())
    rows[[length(rows) + 1]] <<- data.frame(participant = as.character(p), issue = issue, detail = detail,
                                            plate = as.character(plate), stringsAsFactors = FALSE)
  }
  pl <- unique(x$long[, c("id_num", "plate")])
  plate_of <- function(ids) pl$plate[match(ids, pl$id_num)]
  # one row per participant x issue: join that participant's items with "; "
  add_joined <- function(ids, items, issues) {
    if (!length(ids)) return(invisible())
    key <- paste(ids, issues, sep = "\r")
    for (k in unique(key)) {
      w <- key == k
      add(ids[w][1], issues[w][1], paste(unique(items[w]), collapse = "; "), plate_of(ids[w][1]))
    }
  }

  # -- set-up ----------------------------------------------------------------
  if (length(S$switched_off)) {
    opts <- unique(ifelse(grepl("^il10", S$switched_off), "il10", S$switched_off))
    add("(file)", "set-up", paste0(paste(S$switched_off, collapse = ", "), " wells not checked (",
                                   paste0(opts, " = FALSE", collapse = ", "), ")"))
  }
  if (length(S$never_seen)) add("(file)", "set-up", paste0("no wells found for ", paste(S$never_seen, collapse = ", ")))

  # -- data entry ------------------------------------------------------------
  ig <- x$integrity
  if (nrow(ig$unparsed)) add("(row)", "unreadable id", paste0("'", ig$unparsed$id, "' (excel row ", ig$unparsed$excel_row, ")"),
                             ig$unparsed$plate)
  if (nrow(ig$duplicates)) {
    dd <- ig$duplicates[!duplicated(paste(ig$duplicates$id_num, ig$duplicates$condition)), ]
    add_joined(dd$id_num, paste0(dd$condition, " entered more than once"), rep("duplicated data", nrow(dd)))
  }
  if (nrow(ig$unexpected)) add_joined(ig$unexpected$id_num, paste0("'", ig$unexpected$condition, "'"),
                                      rep("unknown condition", nrow(ig$unexpected)))
  if (nrow(ig$label_mismatch)) add_joined(ig$label_mismatch$id_num, paste0("'", ig$label_mismatch$id, "'"),
                                          rep("different study prefix", nrow(ig$label_mismatch)))

  # -- missing ---------------------------------------------------------------
  m <- x$missing
  if (nrow(m)) {
    mi <- character(0); mt <- character(0)
    mc <- m[m$issue == "condition missing", ]
    mi <- c(mi, mc$id_num); mt <- c(mt, paste0(mc$condition, " missing"))
    mv <- m[m$issue == "value missing", ]
    if (nrow(mv)) {
      g <- stats::aggregate(analyte ~ id_num + condition, mv, function(z) paste(z, collapse = ", "))
      all_blank <- lengths(strsplit(g$analyte, ", ")) == length(S$analytes) & length(S$analytes) > 1
      mi <- c(mi, g$id_num); mt <- c(mt, paste0(g$condition, ": ", ifelse(all_blank, "all values", g$analyte), " blank"))
    }
    mo <- m[!m$issue %in% c("condition missing", "value missing"), ]
    mi <- c(mi, mo$id_num); mt <- c(mt, mo$issue)
    o <- order(.lqa_idnum(mi))
    add_joined(mi[o], mt[o], rep("missing data", length(mi)))
  }

  # -- comparisons at the regular wells (every reversal; low-dose wells are counted separately) --
  cc <- x$contrasts
  pt <- x$patterns
  bp_id <- character(0); bp_txt <- character(0); bp_issue <- character(0)
  # a stimulant that barely rises above US in every cytokine
  wk <- pt$weak[pt$weak$pattern == "stimulant possibly not added", , drop = FALSE]
  if (nrow(wk)) {
    bp_id <- c(bp_id, wk$id_num)
    bp_issue <- c(bp_issue, paste0("low ", wk$conds, ifelse(wk$conds == "lps", "-only", ""), " well"))
    bp_txt <- c(bp_txt, paste0(wk$conds, " ~ us, stimulant missing?"))
  }
  if (nrow(cc) && any(!is.na(cc$diff))) {
    f <- cc[cc$flag & cc$tier == "core", ]
    for (p in unique(f$id_num)) {
      fp <- f[f$id_num == p, ]
      for (tt in intersect(S$contrasts$test, fp$test)) {
        r <- fp[fp$test == tt, ]
        fam <- .lqa_family(r$lower[1], r$higher[1])
        cyt <- paste(r$analyte, collapse = ", ")
        bp_id <- c(bp_id, p); bp_issue <- c(bp_issue, fam)
        bp_txt <- c(bp_txt, if (r$lower[1] == "us") cyt else paste0(r$lower[1], " (", cyt, ")"))
      }
    }
  }
  # two wells whose values look exchanged
  if (nrow(pt$swaps)) for (i in seq_len(nrow(pt$swaps))) {
    p <- pt$swaps$id_num[i]
    pair <- strsplit(pt$swaps$conds[i], ",")[[1]]
    v <- x$values[x$values$id_num == p & x$values$condition %in% pair & !is.na(x$values$value) & x$values$value > 0, ]
    mlog <- tapply(log(v$value), v$condition, mean)
    hi <- names(which.max(mlog)); lo <- setdiff(pair, hi)
    tt <- paste0(hi, " > ", lo)
    k <- which(bp_id == p & bp_issue == tt)
    if (length(k)) bp_txt[k] <- paste0(bp_txt[k], ", label swap?") else {
      bp_id <- c(bp_id, p); bp_issue <- c(bp_issue, tt); bp_txt <- c(bp_txt, "label swap?")
    }
  }
  add_joined(bp_id, bp_txt, bp_issue)

  # -- dose response ---------------------------------------------------------
  ds <- x$dose
  if (nrow(ds)) {
    # a missing or unstimulated reference well is not a dose-response problem
    no_ref <- unique(c(x$missing$id_num[x$missing$condition %in% vapply(S$dose_series, `[[`, "", "reference")],
                       pt$weak$id_num))
    nd <- ds[(ds$flag == "no inhibition (slope >= 0)" | ds$weak_vs_cohort) & !ds$id_num %in% no_ref, ]
    if (nrow(nd)) {
      g <- stats::aggregate(analyte ~ id_num + series, nd, function(z) paste(unique(z), collapse = ", "))
      g <- g[order(.lqa_idnum(g$id_num)), ]
      add_joined(g$id_num, paste0(g$series, ": no substantive decrease with dose in ", g$analyte),
                 rep("no dose-response", nrow(g)))
    }
  }

  # -- plates ----------------------------------------------------------------
  ps <- x$plates$summary
  if (length(ps) && nrow(ps)) {
    pf <- ps[ps$plate_flag != "", ]
    if (nrow(pf)) add("(plate)", "plate", ifelse(pf$fold < 1, "values lower than other plates",
                                                 "values higher than other plates"), pf$plate)
  }

  empty <- data.frame(participant = character(0), issue = character(0), detail = character(0),
                      plate = character(0), stringsAsFactors = FALSE)
  if (!length(rows)) return(empty)
  td <- do.call(rbind, rows)
  first <- c("set-up", "unreadable id", "different study prefix", "duplicated data", "unknown condition", "missing data")
  last <- c("no dose-response", "plate")
  rank <- ifelse(td$issue %in% first, match(td$issue, first),
                 ifelse(td$issue %in% last, 100 + match(td$issue, last), 50))
  grp <- ifelse(td$participant == "(file)", 0, ifelse(td$participant %in% c("(row)", "(plate)"), 2, 1))
  td <- td[order(grp, .lqa_idnum(td$participant), td$participant, rank, td$issue), ]
  rownames(td) <- NULL
  td
}

# reversals at the low-dose wells (crt_7, crt_6.5, lowest IL-10): counted and
# listed separately, "just in case"
.lqa_low_dose <- function(x) {
  cc <- x$contrasts
  empty <- data.frame(participant = character(0), test = character(0), cytokines = character(0),
                      plate = character(0), stringsAsFactors = FALSE)
  if (!nrow(cc)) return(empty)
  f <- cc[cc$flag & cc$tier == "sensitive", ]
  if (!nrow(f)) return(empty)
  g <- stats::aggregate(analyte ~ id_num + test + plate, f, function(z) paste(z, collapse = ", "))
  g <- g[order(.lqa_idnum(g$id_num), g$id_num, match(g$test, x$settings$contrasts$test)), ]
  data.frame(participant = g$id_num, test = g$test, cytokines = g$analyte, plate = g$plate,
             stringsAsFactors = FALSE, row.names = NULL)
}
