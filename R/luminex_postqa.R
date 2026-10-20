# Luminex cell-culture QA after repeats (before scoring) -------------------------
#
# luminex_postqa()         checks the "final master" for what will go wrong in scoring,
#                          and builds a plan (default, plus the team's own choices)
# luminex_postqa_report()  (see luminex_postqa_report.R) renders the HTML and, once the
#                          plan is final, exports the final master with the plan applied


#' Luminex QA after repeats: what will go wrong in scoring, and the plan to fix it
#'
#' Run on the "final master" tab (after repeats and repeat selection). Values
#' are looked at the way the scoring step sees them: negatives set to 0, the
#' participant's US well subtracted, negatives set to 0 again, then ln(x + 1).
#'
#' Tagged participants are shown in two groups: inhibition slopes (problems
#' with the LPS, cortisol and IL-10 wells) and cytokine production (problems
#' with the LPS, R848 and HSP60 wells, which make up the stimulated composite:
#' each cytokine scored as below, z-scored within stimulant across all
#' participants, then averaged across cytokines). A participant is tagged when,
#' as scored:
#' \describe{
#'   \item{sus 0}{a stimulated or inhibitor well comes out as 0 in at least
#'     `dip_cytokines` cytokines, either because it is about 0 as measured
#'     (`zero_mfi` MFI or less) or because it is below US.}
#'   \item{non-zero us large cv}{the US well's replicate CV is above `us_cv`\%
#'     while US is at least `us_min` MFI (US is subtracted from every well).}
#'   \item{sus dose-response}{one dose well is more than `dip_size` below
#'     (a dip) or above (a spike) both neighbouring doses in at least
#'     `dip_cytokines` cytokines.}
#'   \item{crt > 2x lps}{an inhibitor well reads more than `inhibitor_fold`
#'     times the LPS well in at least `dip_cytokines` cytokines.}
#' }
#'
#' **Default plan** (applied in this order):
#' \enumerate{
#'   \item a well about 0 as measured: set to NA;
#'   \item two or more wells below US, or US replicates that disagree: ignore US
#'     (US treated as 0 for that participant); a single well below US: set that
#'     well to NA;
#'   \item a dose-response dip or spike: set that well to NA;
#'   \item inhibitor wells above `inhibitor_fold` x LPS: set LPS to NA; if the
#'     inhibitor wells then still rise with dose, set them to NA too.
#' }
#' Before step 2, for a participant with a US problem (wells below US, or US
#' replicates that disagree), the other US runs in the `repeats_sheet` (the
#' original run, repeats, interassay) are tried in place of the US in this
#' sheet. If one of them removes every US problem, the plan is "use other us"
#' (that run's values replace the US well) instead of ignoring US or setting
#' wells to NA. When several do, the one leaving the fewest problems is used.
#'
#' **Changing the plan.** List a participant in `na`, `ignore_us`,
#' `use_other_us` or `keep` and what you list replaces the default plan for that
#' participant (only list the participants you disagree with). `keep` means
#' leave the participant as is.
#'
#' @param data Path to the master `.xlsx`, or a data frame.
#' @param sheet Sheet name (default `"final master"`).
#' @param hsp60,r848,il10,cort Which wells the study has (as in [luminex_qa()]):
#'   `cort = 4` (default) is crt_5.5, crt_6, crt_6.5, crt_7; `cort = 3` is
#'   crt_5.5, crt_6, crt_6.5.
#' @param na Wells to set to NA, as sample ids with or without the study
#'   prefix, e.g. `c("1139_V1_crt_7", "MHS_1066_V1_r848")`.
#' @param ignore_us Participants whose US well is treated as 0, e.g.
#'   `c("2066_V2", "2067_V2")`.
#' @param use_other_us Participants whose US well takes the values of another
#'   run of it in the `repeats_sheet`, used even if US problems remain. Give the
#'   participant (`"1066_V1"`: the run leaving the fewest problems) or the
#'   participant and the sheet's row number (`c("1066_V1" = 169)`: that run).
#' @param keep Participants to leave as they are (no default plan).
#' @param repeats_sheet Where to look for other US runs: a sheet name in the
#'   same file (default `"for master"`, skipped if the file has no such sheet),
#'   or a data frame of that sheet. `NULL` to skip.
#' @param inhibitor_fold,dip_size,dip_cytokines As in [luminex_qa()].
#' @param zero_mfi A well at or below this MFI (after negatives are set to 0)
#'   counts as about 0 (default 5).
#' @param us_cv,us_min US replicate CV (\%) above which, with US at least
#'   `us_min` MFI, US replicates count as disagreeing (defaults 30 and 10).
#' @param study,visit Labels for the report. Default: taken from the ids.
#' @return An object of class `luminex_postqa`. `$plan` is the plan,
#'   `$fixed` the sheet with the plan applied (what `export = TRUE` in
#'   [luminex_postqa_report()] saves).
#' @examples
#' qa <- luminex_postqa(luminex_example())
#' qa$plan
#' # disagree with the default for 1039 and 1045:
#' qa2 <- luminex_postqa(luminex_example(), keep = "1045", na = c("1039_V1_crt_6.5", "1039_V1_crt_6"))
#' @export
luminex_postqa <- function(data,
                           sheet = "final master",
                           hsp60 = TRUE,
                           r848 = TRUE,
                           il10 = FALSE,
                           cort = 4,
                           na = NULL,
                           ignore_us = NULL,
                           use_other_us = NULL,
                           keep = NULL,
                           repeats_sheet = "for master",
                           inhibitor_fold = 2,
                           dip_size = 1,
                           dip_cytokines = 2,
                           zero_mfi = 5,
                           us_cv = 30,
                           us_min = 10,
                           study = NULL,
                           visit = NULL) {
  cort <- .lqa_check_cort(cort)
  contrasts <- luminex_contrasts(hsp60 = hsp60, r848 = r848, il10 = il10, cort = cort, inhibitor_fold = inhibitor_fold)
  dose_series <- luminex_dose_series(il10 = il10, cort = cort)

  source_label <- if (is.character(data)) basename(data) else "data frame"
  if (is.character(data)) {
    if (!file.exists(data)) stop("File not found: ", data, call. = FALSE)
    raw <- openxlsx::read.xlsx(data, sheet = sheet)
  } else {
    raw <- as.data.frame(data, stringsAsFactors = FALSE)
    sheet <- NA_character_
  }
  if (!"id" %in% names(raw)) stop("The sheet needs an `id` column.", call. = FALSE)
  original <- raw                     # the whole sheet as read, for the export
  raw <- raw[!is.na(raw$id) & trimws(raw$id) != "", , drop = FALSE]
  raw$id <- trimws(as.character(raw$id))
  raw$.row <- seq_len(nrow(raw)) + 1
  if (!"plate" %in% names(raw)) raw$plate <- NA_character_
  analytes <- sub("_avg_mfi$", "", grep("_avg_mfi$", names(raw), value = TRUE))
  if (!length(analytes)) stop("No `*_avg_mfi` columns found.", call. = FALSE)

  ids <- .lqa_parse_ids(raw$id)
  raw <- cbind(raw, ids[, c("study", "id_num", "visit", "condition", "round", "parsed")])
  unparsed <- raw[!raw$parsed, c(".row", "id", "plate")]
  names(unparsed)[1] <- "excel_row"
  raw <- raw[raw$parsed, , drop = FALSE]

  off <- character(0)
  if (!isTRUE(hsp60)) off <- c(off, "hsp60")
  if (!isTRUE(r848)) off <- c(off, "r848")
  if (cort == 3) off <- c(off, "crt_7")
  is_off <- raw$condition %in% off | (!isTRUE(il10) & grepl("^il10", raw$condition))
  switched_off <- if (any(is_off)) sort(unique(raw$condition[is_off])) else character(0)
  raw <- raw[!is_off, , drop = FALSE]

  raw$num <- raw$id_num
  visits <- sort(unique(raw$visit))
  if (length(visits) > 1) raw$id_num <- paste0(raw$id_num, "_", raw$visit)
  if (is.null(study)) study <- .lqa_mode(raw$study)
  if (is.null(visit)) visit <- paste(visits, collapse = " & ")

  configured <- unique(c(contrasts$higher, contrasts$lower,
                         unlist(lapply(dose_series, function(s) c(s$reference, names(s$doses))))))
  seen <- unique(raw$condition[raw$round == "original"])
  expected_conditions <- intersect(configured, seen)
  never_seen <- setdiff(configured, seen)
  contrasts <- contrasts[contrasts$higher %in% expected_conditions & contrasts$lower %in% expected_conditions, , drop = FALSE]
  dose_series <- Filter(function(s) sum(names(s$doses) %in% seen) >= 2 && s$reference %in% seen, dose_series)
  for (nm in names(dose_series))
    dose_series[[nm]]$doses <- dose_series[[nm]]$doses[names(dose_series[[nm]]$doses) %in% seen]
  stims <- intersect(unique(contrasts$higher[contrasts$lower == "us"]), expected_conditions)
  wells <- unique(c(stims, unlist(lapply(dose_series, function(s) c(s$reference, names(s$doses))))))

  S <- list(source = source_label, sheet = sheet, study = study, visit = visit,
            hsp60 = hsp60, r848 = r848, il10 = il10, cort = cort, analytes = analytes,
            contrasts = contrasts, dose_series = dose_series, expected_conditions = expected_conditions,
            never_seen = never_seen, switched_off = switched_off, wells = wells, stims = stims,
            inhibitor_fold = inhibitor_fold, dip_size = dip_size, dip_cytokines = dip_cytokines,
            zero_mfi = zero_mfi, us_cv = us_cv, us_min = us_min, run_at = Sys.time())

  # ---- other US runs (the "for master" sheet: original, repeats, interassay) ----------
  alt_raw <- NULL; alt_us <- NULL
  if (!is.null(repeats_sheet)) {
    ar <- NULL
    if (is.data.frame(repeats_sheet)) {
      ar <- repeats_sheet
      S$repeats_sheet <- "repeats sheet"
    } else if (is.character(data) && is.character(repeats_sheet)) {
      S$repeats_sheet <- repeats_sheet
      if (!repeats_sheet %in% openxlsx::getSheetNames(data)) {
        S$alt_note <- sprintf("No '%s' sheet in this file, so other US runs were not checked.", repeats_sheet)
      } else if (!identical(repeats_sheet, sheet)) {
        ar <- openxlsx::read.xlsx(data, sheet = repeats_sheet)
      }
    }
    if (!is.null(ar) && "id" %in% names(ar) && all(paste0(analytes, "_avg_mfi") %in% names(ar))) {
      ar <- as.data.frame(ar, stringsAsFactors = FALSE)
      ar <- ar[!is.na(ar$id) & trimws(ar$id) != "", , drop = FALSE]
      ar$id <- trimws(as.character(ar$id))
      ar$.row <- seq_len(nrow(ar)) + 1
      if (!"plate" %in% names(ar)) ar$plate <- NA_character_
      ar <- cbind(ar, .lqa_parse_ids(ar$id)[, c("study", "id_num", "visit", "condition", "round", "parsed")])
      ar <- ar[ar$parsed & ar$condition == "us" & ar$visit %in% visits, , drop = FALSE]
      if (length(visits) > 1) ar$id_num <- paste0(ar$id_num, "_", ar$visit)
      if (nrow(ar)) { alt_raw <- ar; alt_us <- .lqa_long(ar, analytes, "avg_mfi") }
    }
  }

  # ---- missing & data entry (compact) --------------------------------------------
  integrity <- .lqa_integrity(raw, expected_conditions, unparsed, study, visit)
  long <- .lqa_long(raw, analytes, "avg_mfi")
  orig <- long[long$round == "original", , drop = FALSE]
  vals <- orig[!duplicated(orig[, c("id_num", "condition", "analyte")]), , drop = FALSE]
  missing <- .lqa_missing(vals, expected_conditions, analytes, NULL)
  entry <- .lpq_entry(raw, integrity, missing, unparsed)

  # ---- participants and their wells ---------------------------------------------
  people <- unique(raw[raw$round == "original", c("id_num", "num", "visit")])
  people <- people[!duplicated(people$id_num), ]
  wid <- unique(vals[, c("id_num", "condition", "id")])
  wid <- wid[!duplicated(wid[, c("id_num", "condition")]), ]
  S$people <- people

  # ---- the plan: default, replaced by the team's choices where given ------------------
  default <- .lpq_default_plan(vals, S, wid, alt_us)
  user <- .lpq_user_plan(na, ignore_us, keep, people, wid, use_other_us, alt_us, vals, S)
  plan <- default[!default$participant %in% user$participant, , drop = FALSE]
  plan <- rbind(plan, user)
  plan <- plan[order(.lqa_idnum(plan$participant), plan$participant, plan$set_by != "you",
                     match(plan$action, c("use other us", "ignore us", "na", "keep"))), , drop = FALSE]
  rownames(plan) <- NULL

  # ---- before and after the plan -------------------------------------------------------
  vals_after <- .lpq_apply_long(vals, plan, alt_us)
  ignored <- unique(plan$participant[plan$action == "ignore us"])
  before <- .lpq_problems(vals, S)
  after <- .lpq_problems(vals_after, S, us_ignored = ignored)
  scored <- .lpq_scored(vals, vals_after, S)
  slopes <- .lpq_slopes(scored, S)
  composites <- .lpq_composites(scored, S)

  tagged <- unique(c(before$participant, plan$participant))
  tagged <- tagged[order(.lqa_idnum(tagged), tagged)]
  sections <- .lpq_sections(before, plan, vals, S)

  out <- list(settings = S, data = raw, fixed = .lpq_apply_df(original, plan, alt_raw, S$repeats_sheet),
              alt_us = alt_us, values = vals, values_after = vals_after,
              entry = entry, integrity = integrity, missing = missing,
              problems = before, problems_after = after, plan = plan, default_plan = default,
              scored = scored, slopes = slopes, composites = composites, tagged = tagged,
              tagged_inhibition = sections$inhibition, tagged_production = sections$production)
  class(out) <- c("luminex_postqa", "list")
  out
}


#' @export
print.luminex_postqa <- function(x, ...) {
  s <- x$settings
  cat("f h r c | Luminex QA after repeats -", s$study, s$visit, "\n")
  cat("  source:      ", s$source, if (!is.na(s$sheet)) paste0("[", s$sheet, "]"), "\n")
  cat("  participants:", nrow(s$people), " data entry problems:", nrow(x$entry),
      " tagged:", length(x$tagged), "\n")
  p <- x$plan
  if (nrow(p)) {
    cat("  plan:\n")
    for (i in seq_len(nrow(p))) cat("   ", p$participant[i], "|", p$action[i], "|", p$id[i],
                                    if (!is.na(p$from[i])) paste0("<- ", p$from[i], " (row ", p$from_row[i], ")"), "|", p$set_by[i], "\n")
  }
  cat("  run luminex_postqa_report() for the report; add export = TRUE once the plan is final.\n")
  invisible(x)
}


# the plan applied to the master sheet (every row and column kept, plus a `fix` column):
#   na        every cytokine value of the well (MFI, replicates, concentration, CV) -> NA
#   use other us  the US well's cytokine values replaced by those of another run of it
#                 (a row of the repeats sheet)
#   ignore us the participant's US well *_avg_mfi -> 0, so subtracting US changes nothing
#   keep      nothing
.lpq_apply_df <- function(data, plan, alt_raw = NULL, alt_sheet = "for master") {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  ids <- trimws(as.character(data$id))
  data$fix <- NA_character_
  an <- sub("_avg_mfi$", "", grep("_avg_mfi$", names(data), value = TRUE))
  val_cols <- grep(paste0("^(", paste(an, collapse = "|"), ")_"), names(data), value = TRUE)
  for (k in which(plan$action == "use other us")) {
    i <- which(ids == plan$id[k])
    j <- if (is.null(alt_raw)) integer(0) else which(alt_raw$.row %in% plan$from_row[k])
    if (!length(i) || !length(j)) next
    for (cc in intersect(val_cols, names(alt_raw))) {
      x <- alt_raw[[cc]][j[1]]
      data[[cc]][i] <- if (is.numeric(data[[cc]])) suppressWarnings(as.numeric(x)) else as.character(x)
    }
    if ("plate" %in% names(data) && "plate" %in% names(alt_raw)) data$plate[i] <- as.character(alt_raw$plate[j[1]])
    data$fix[i] <- paste0("us values taken from '", plan$from[k], "' (", alt_sheet, " row ", plan$from_row[k], ")")
  }
  for (k in which(plan$action == "ignore us")) {
    i <- which(ids == plan$id[k])
    data[i, paste0(an, "_avg_mfi")] <- 0
    data$fix[i] <- "us ignored (avg mfi set to 0)"
  }
  for (k in which(plan$action == "na")) {
    i <- which(ids == plan$id[k])
    data[i, val_cols] <- NA
    data$fix[i] <- "set to NA"
  }
  data
}


# ---- internal helpers ---------------------------------------------------------------

.lpq_empty_plan <- function() {
  data.frame(participant = character(0), action = character(0), id = character(0), from = character(0),
             from_row = integer(0), set_by = character(0), why = character(0), stringsAsFactors = FALSE)
}

.lpq_row <- function(p, action, id, why, set_by = "default", from = NA_character_, from_row = NA_integer_) {
  data.frame(participant = p, action = action, id = id, from = from, from_row = as.integer(from_row),
             set_by = set_by, why = why, stringsAsFactors = FALSE)
}

# a participant's US rows replaced by another run's (alt: long table of that run)
.lpq_put_us <- function(vals, us_id, alt_rows) {
  cols <- intersect(c("value", "mfi", "rep1", "rep2", "cv", "conc_raw", "conc", "plate"), names(vals))
  i <- which(vals$id == us_id)
  j <- match(vals$analyte[i], alt_rows$analyte)
  for (cc in cols) vals[[cc]][i] <- alt_rows[[cc]][j]
  vals
}

# another run of the US well that removes every US problem, or NULL
.lpq_other_us <- function(p, vals, S, alt_us, us_id, must_resolve = TRUE) {
  if (is.null(alt_us) || is.na(us_id)) return(NULL)
  cand <- alt_us[alt_us$id_num == p, ]
  if (!nrow(cand)) return(NULL)
  v0 <- vals[vals$id_num == p, ]
  cur <- v0[v0$condition == "us", ]
  best <- NULL
  for (rw in unique(cand$excel_row)) {
    cc <- cand[cand$excel_row == rw, ]
    if (all(is.na(cc$value))) next
    a <- cc$value[match(cur$analyte, cc$analyte)]
    if (all((is.na(a) & is.na(cur$value)) | (!is.na(a) & !is.na(cur$value) & abs(a - cur$value) < 1e-9))) next  # the run already used
    pr <- .lpq_problems(.lpq_put_us(v0, us_id, cc), S)
    if (must_resolve && any(pr$type %in% c("below_us", "us_cv"))) next
    if (is.null(best) || nrow(pr) < best$n)
      best <- list(id = cc$id[1], row = rw, plate = cc$plate[1], round = cc$round[1], n = nrow(pr))
  }
  best
}

# the same plan as .lpq_apply_df(), on the long values table
.lpq_apply_long <- function(vals, plan, alt_us = NULL) {
  if (!nrow(plan)) return(vals)
  for (k in which(plan$action == "use other us")) {
    if (is.null(alt_us)) next
    a <- alt_us[alt_us$excel_row %in% plan$from_row[k], ]
    if (nrow(a)) vals <- .lpq_put_us(vals, plan$id[k], a)
  }
  cols <- intersect(c("value", "mfi", "rep1", "rep2", "cv", "conc_raw", "conc"), names(vals))
  i <- vals$id %in% plan$id[plan$action == "ignore us"]
  vals$value[i] <- 0; vals$mfi[i] <- 0
  i <- vals$id %in% plan$id[plan$action == "na"]
  for (cc in cols) vals[[cc]][i] <- NA
  vals
}

# one matrix of values per participant: analytes x conditions
.lpq_matrix <- function(v, analytes, conds, col = "value") {
  m <- matrix(NA_real_, length(analytes), length(conds), dimnames = list(analytes, conds))
  r <- v[v$condition %in% conds & v$analyte %in% analytes, ]
  if (nrow(r)) m[cbind(r$analyte, r$condition)] <- r[[col]]
  m
}

.lpq_score <- function(m, us) log(pmax(pmax(m, 0) - pmax(us, 0), 0) + 1)

# problems as the scoring step will see them
.lpq_problems <- function(vals, S, us_ignored = character(0)) {
  out <- list()
  addp <- function(p, type, well, cyt, series = NA_character_, extra = NA_character_) {
    out[[length(out) + 1]] <<- data.frame(participant = p, type = type, well = well, series = series,
                                          cytokines = paste(cyt, collapse = ", "), extra = extra,
                                          stringsAsFactors = FALSE)
  }
  an <- S$analytes
  k <- min(S$dip_cytokines, length(an))
  for (p in unique(vals$id_num)) {
    v <- vals[vals$id_num == p, ]
    conds <- unique(c("us", S$wells))
    m <- .lpq_matrix(v, an, conds)
    us <- if (p %in% us_ignored) stats::setNames(rep(0, length(an)), an) else pmax(m[, "us"], 0)
    flagged <- character(0)
    for (w in S$wells) {
      x <- pmax(m[, w], 0)
      if (all(is.na(x))) next
      zero <- !is.na(x) & x <= S$zero_mfi
      if (sum(zero) >= k) { addp(p, "zero", w, an[zero]); flagged <- c(flagged, w); next }
      below <- !is.na(x) & !is.na(us) & !zero & x <= us
      if (sum(below) >= k) { addp(p, "below_us", w, an[below]); flagged <- c(flagged, w) }
    }
    if (!p %in% us_ignored) {
      cv <- .lpq_matrix(v, an, "us", "cv")[, 1]
      hi <- !is.na(cv) & !is.na(m[, "us"]) & cv > S$us_cv & m[, "us"] >= S$us_min
      if (any(hi)) addp(p, "us_cv", "us", an[hi], extra = paste0(an[hi], " CV ", round(cv[hi]), "%", collapse = ", "))
    }
    for (nm in names(S$dose_series)) {
      s <- S$dose_series[[nm]]
      sc_conds <- c(s$reference, names(sort(s$doses)))
      sc <- .lpq_score(m[, sc_conds, drop = FALSE], us)
      if (length(sc_conds) >= 3) for (j in 2:(length(sc_conds) - 1)) {
        w <- sc_conds[j]
        if (w %in% flagged) next
        y1 <- sc[, j - 1]; y2 <- sc[, j]; y3 <- sc[, j + 1]
        ok <- !is.na(y1) & !is.na(y2) & !is.na(y3)
        dip <- ok & y2 < y1 - S$dip_size & y2 < y3 - S$dip_size
        spike <- ok & y2 > y1 + S$dip_size & y2 > y3 + S$dip_size
        if (sum(dip) >= k) addp(p, "dip", w, an[dip], series = nm)
        if (sum(spike) >= k) addp(p, "spike", w, an[spike], series = nm)
      }
      ref <- pmax(m[, s$reference], 0)
      for (w in names(sort(s$doses))) {
        x <- pmax(m[, w], 0)
        hi <- !is.na(x) & !is.na(ref) & x > S$inhibitor_fold * ref
        if (sum(hi) >= k) addp(p, "above_ref", w, an[hi], series = nm)
      }
    }
  }
  if (!length(out)) return(data.frame(participant = character(0), type = character(0), well = character(0),
                                      series = character(0), cytokines = character(0), extra = character(0),
                                      stringsAsFactors = FALSE))
  do.call(rbind, out)
}

# problems -> "issue: detail" lines for the report
.lpq_issue_lines <- function(pr, S) {
  if (!nrow(pr)) return(data.frame(issue = character(0), detail = character(0), stringsAsFactors = FALSE))
  out <- list()
  # wells with the same reason and cytokines are listed together: "hsp60, r848 (below us: il1b, il6, tnfa)"
  grouped <- function(wells, label) {
    g <- split(wells, factor(label, levels = unique(label)))
    paste(vapply(names(g), function(l) paste0(paste(g[[l]], collapse = ", "), " (", l, ")"), ""), collapse = "; ")
  }
  z <- pr[pr$type %in% c("zero", "below_us"), ]
  if (nrow(z)) out[[length(out) + 1]] <- data.frame(issue = "sus 0", detail = grouped(
    z$well, paste0(ifelse(z$type == "zero", "about 0 as measured", "below us"), ": ", z$cytokines)), stringsAsFactors = FALSE)
  u <- pr[pr$type == "us_cv", ]
  if (nrow(u)) out[[length(out) + 1]] <- data.frame(issue = "non-zero us large cv", detail = u$extra[1],
                                                    stringsAsFactors = FALSE)
  d <- pr[pr$type %in% c("dip", "spike"), ]
  if (nrow(d)) out[[length(out) + 1]] <- data.frame(issue = "sus dose-response", detail = paste(
    paste0(d$series, ": ", d$type, " at ", d$well, " (", d$cytokines, ")"), collapse = "; "), stringsAsFactors = FALSE)
  a <- pr[pr$type == "above_ref", ]
  for (nm in unique(a$series)) {
    r <- a[a$series == nm, ]
    ref <- S$dose_series[[nm]]$reference
    fam <- .lqa_family(sub("_.*$", "", r$well[1]), ref, S$inhibitor_fold)
    out[[length(out) + 1]] <- data.frame(issue = fam, detail = grouped(r$well, r$cytokines), stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

.lpq_default_plan <- function(vals, S, wid, alt_us = NULL) {
  plan <- .lpq_empty_plan()
  idof <- function(p, cnd) wid$id[wid$id_num == p & wid$condition == cnd][1]
  add <- function(p, action, cnd, why, from = NA_character_, from_row = NA_integer_) {
    i <- idof(p, cnd)
    if (is.na(i) || any(plan$participant == p & plan$action == action & plan$id == i)) return(invisible())
    plan <<- rbind(plan, .lpq_row(p, action, i, why, from = from, from_row = from_row))
  }
  applied <- function() .lpq_apply_long(vals, plan, alt_us)
  # 1-2: wells about 0, wells below US, US replicates that disagree
  pr <- .lpq_problems(vals, S)
  for (p in unique(pr$participant)) {
    r <- pr[pr$participant == p, ]
    for (i in which(r$type == "zero")) add(p, "na", r$well[i], paste0("about 0 as measured (", r$cytokines[i], ")"))
    below <- r[r$type == "below_us", ]
    cvp <- r[r$type == "us_cv", ]
    if (nrow(below) || nrow(cvp)) {
      other <- .lpq_other_us(p, vals, S, alt_us, idof(p, "us"))
      if (!is.null(other)) {
        why <- c(if (nrow(below)) paste0(paste(below$well, collapse = ", "), " below us"),
                 if (nrow(cvp)) paste0("non-zero us large cv (", cvp$extra[1], ")"))
        where <- paste0(S$repeats_sheet, " row ", other$row,
                        if (!is.na(other$plate) && nzchar(other$plate)) paste0(", ", sub("\\.xlsx$", "", other$plate)))
        add(p, "use other us", "us", paste0(paste(why, collapse = "; "), "; the us from '",
                                            sub("^[A-Za-z]+_", "", other$id), "' (", where, ") resolves this"),
            from = other$id, from_row = other$row)
        next
      }
    }
    if (nrow(below) >= 2 || nrow(cvp)) {
      why <- c(if (nrow(below) >= 2) paste0(nrow(below), " wells below us (", paste(below$well, collapse = ", "), ")"),
               if (nrow(cvp)) paste0("non-zero us large cv (", cvp$extra[1], ")"))
      add(p, "ignore us", "us", paste(why, collapse = "; "))
    } else if (nrow(below) == 1) {
      add(p, "na", below$well, paste0("below us (", below$cytokines, ")"))
    }
  }
  ign <- function() unique(plan$participant[plan$action == "ignore us"])
  # 3: dips and spikes on the scored scale
  for (pass in 1:3) {
    pr <- .lpq_problems(applied(), S, us_ignored = ign())
    d <- pr[pr$type %in% c("dip", "spike"), ]
    if (!nrow(d)) break
    for (i in seq_len(nrow(d))) add(d$participant[i], "na", d$well[i], paste0(d$type[i], " at ", d$well[i], " (", d$cytokines[i], ")"))
  }
  # 4: inhibitor wells above k x LPS -> LPS NA; if the inhibitor wells still rise with dose -> those NA too
  v1 <- applied()
  pr <- .lpq_problems(v1, S, us_ignored = ign())
  a <- pr[pr$type == "above_ref", ]
  for (p in unique(a$participant)) {
    r <- a[a$participant == p, ]
    for (nm in unique(r$series)) {
      s <- S$dose_series[[nm]]
      rr <- r[r$series == nm, ]
      grp <- split(rr$well, factor(rr$cytokines, levels = unique(rr$cytokines)))
      add(p, "na", s$reference, paste(vapply(names(grp), function(cy)
        paste0(paste(grp[[cy]], collapse = ", "), " > ", S$inhibitor_fold, "x ", s$reference, " (", cy, ")"), ""), collapse = "; "))
    }
  }
  v2 <- applied()
  k <- min(S$dip_cytokines, length(S$analytes))
  for (p in unique(a$participant)) for (nm in unique(a$series[a$participant == p])) {
    s <- S$dose_series[[nm]]
    doses <- names(sort(s$doses))
    v <- v2[v2$id_num == p, ]
    m <- .lpq_matrix(v, S$analytes, c("us", doses))
    us <- if (p %in% ign()) 0 * m[, "us"] else pmax(m[, "us"], 0)
    sc <- .lpq_score(m[, doses, drop = FALSE], us)
    up <- apply(sc, 1, function(y) { ok <- !is.na(y); if (sum(ok) < 2) return(FALSE)
      x <- seq_along(y)[ok]; y <- y[ok]; sum((x - mean(x)) * (y - mean(y))) > 0 })
    if (sum(up) >= k) for (w in doses)
      add(p, "na", w, paste0(nm, " wells still rise with dose without ", s$reference, " (", paste(S$analytes[up], collapse = ", "), ")"))
  }
  plan
}

# the team's choices: replace the default plan for the participants they name
.lpq_user_plan <- function(na, ignore_us, keep, people, wid, use_other_us = NULL, alt_us = NULL,
                           vals = NULL, S = NULL) {
  plan <- .lpq_empty_plan()
  norm <- function(x) tolower(sub("^[A-Za-z]+_", "", trimws(x)))
  well_norm <- norm(wid$id)
  find_well <- function(x) {
    i <- which(well_norm == norm(x))
    if (!length(i)) stop("Well not found: '", x, "'. Use the sample id, e.g. 1139_V1_crt_7.", call. = FALSE)
    i[1]
  }
  find_person <- function(x) {
    y <- norm(x)
    i <- which(tolower(people$id_num) == y | tolower(paste0(people$num, "_", people$visit)) == y)
    if (!length(i)) {
      j <- which(tolower(people$num) == y)
      if (length(j) > 1) stop("'", x, "' has more than one visit; write e.g. ", people$id_num[j[1]], call. = FALSE)
      i <- j
    }
    if (!length(i)) stop("Participant not found: '", x, "'.", call. = FALSE)
    people$id_num[i[1]]
  }
  for (x in na) { i <- find_well(x); plan <- rbind(plan, .lpq_row(wid$id_num[i], "na", wid$id[i], "", "you")) }
  for (x in ignore_us) {
    p <- find_person(x)
    u <- wid$id[wid$id_num == p & wid$condition == "us"]
    if (!length(u)) stop("No us well for ", x, call. = FALSE)
    plan <- rbind(plan, .lpq_row(p, "ignore us", u[1], "", "you"))
  }
  if (length(use_other_us)) {
    sheet <- if (is.null(S$repeats_sheet)) "for master" else S$repeats_sheet
    if (is.null(alt_us)) stop("use_other_us: no '", sheet, "' sheet with us rows to take another run from.", call. = FALSE)
    nm <- names(use_other_us); if (is.null(nm)) nm <- rep("", length(use_other_us))
    for (k in seq_along(use_other_us)) {
      who <- if (nzchar(nm[k])) nm[k] else as.character(use_other_us[[k]])
      row <- if (nzchar(nm[k])) suppressWarnings(as.integer(use_other_us[[k]])) else NA_integer_
      p <- find_person(who)
      u <- wid$id[wid$id_num == p & wid$condition == "us"]
      if (!length(u)) stop("No us well for ", who, call. = FALSE)
      if (is.na(row)) {
        o <- .lpq_other_us(p, vals, S, alt_us, u[1], must_resolve = FALSE)
        if (is.null(o)) stop("use_other_us: no other us run for ", who, " in '", sheet, "'.", call. = FALSE)
        row <- o$row; oid <- o$id
      } else {
        a <- alt_us[alt_us$excel_row == row & alt_us$id_num == p, ]
        if (!nrow(a)) stop("use_other_us: row ", row, " of '", sheet, "' is not a us well of ", who, ".", call. = FALSE)
        oid <- a$id[1]
      }
      plan <- rbind(plan, .lpq_row(p, "use other us", u[1], "", "you", from = oid, from_row = row))
    }
  }
  for (x in keep) {
    p <- find_person(x)
    plan <- rbind(plan, .lpq_row(p, "keep", "", "", "you"))
  }
  plan[!duplicated(plan[, c("participant", "action", "id")]), , drop = FALSE]
}

# scored values (as in the scoring step) before and after the plan, for the charts
.lpq_scored <- function(vals, vals_after, S) {
  one <- function(v, ignored) {
    us <- v[v$condition == "us", c("id_num", "analyte", "value")]
    names(us)[3] <- "us"
    w <- v[v$condition %in% S$wells, c("id_num", "condition", "analyte", "value")]
    w <- merge(w, us, by = c("id_num", "analyte"), all.x = TRUE)
    w$us[w$id_num %in% ignored] <- 0
    w$score <- log(pmax(pmax(w$value, 0) - pmax(w$us, 0), 0) + 1)
    w[, c("id_num", "condition", "analyte", "score")]
  }
  b <- one(vals, character(0)); names(b)[4] <- "before"
  a <- one(vals_after, character(0)); names(a)[4] <- "after"
  merge(b, a, by = c("id_num", "condition", "analyte"), all = TRUE)
}

# stimulated composite per participant and stimulant, as in the scoring script:
# scored value z-scored within stimulant and cytokine across all participants
# (visits pooled), then averaged across cytokines; without and with the plan
.lpq_composites <- function(scored, S) {
  d <- scored[scored$condition %in% S$stims, , drop = FALSE]
  if (!nrow(d)) return(data.frame(id_num = character(0), condition = character(0), before = numeric(0), after = numeric(0)))
  zf <- function(x) {
    m <- mean(x, na.rm = TRUE); s <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) rep(NA_real_, length(x)) else (x - m) / s
  }
  d$zb <- stats::ave(d$before, d$condition, d$analyte, FUN = zf)
  d$za <- stats::ave(d$after, d$condition, d$analyte, FUN = zf)
  mn <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  key <- paste(d$id_num, d$condition, sep = "\r")
  out <- data.frame(id_num = d$id_num[!duplicated(key)], condition = d$condition[!duplicated(key)], stringsAsFactors = FALSE)
  k <- paste(out$id_num, out$condition, sep = "\r")
  out$before <- as.numeric(tapply(d$zb, key, mn)[k])
  out$after <- as.numeric(tapply(d$za, key, mn)[k])
  out
}

# which tagged participants go under inhibition slopes and which under cytokine production
.lpq_sections <- function(pr, plan, vals, S) {
  series <- unique(unlist(lapply(S$dose_series, function(s) c(s$reference, names(s$doses)))))
  stim <- S$stims
  well <- pr$well
  inh <- pr$participant[pr$type %in% c("dip", "spike", "above_ref", "us_cv") |
                          (pr$type %in% c("zero", "below_us") & well %in% series)]
  prod <- pr$participant[pr$type == "us_cv" | (pr$type %in% c("zero", "below_us") & well %in% stim)]
  cond <- vals$condition[match(plan$id, vals$id)]
  us_act <- plan$action %in% c("ignore us", "use other us")
  inh <- c(inh, plan$participant[us_act | (plan$action == "na" & cond %in% series)])
  prod <- c(prod, plan$participant[us_act | (plan$action == "na" & cond %in% stim)])
  rest <- setdiff(plan$participant, c(inh, prod))                # e.g. only "keep": list with the slopes
  inh <- unique(c(inh, rest)); prod <- unique(prod)
  list(inhibition = inh[order(.lqa_idnum(inh), inh)], production = prod[order(.lqa_idnum(prod), prod)])
}

# the problems that belong to each group, for the bullets
.lpq_section_problems <- function(pr, S, section) {
  series <- unique(unlist(lapply(S$dose_series, function(s) c(s$reference, names(s$doses)))))
  if (section == "inhibition")
    pr[pr$type %in% c("dip", "spike", "above_ref", "us_cv") | (pr$type %in% c("zero", "below_us") & pr$well %in% series), , drop = FALSE]
  else
    pr[pr$type == "us_cv" | (pr$type %in% c("zero", "below_us") & pr$well %in% S$stims), , drop = FALSE]
}

.lpq_slopes <- function(scored, S) {
  out <- list()
  for (nm in names(S$dose_series)) {
    s <- S$dose_series[[nm]]
    conds <- c(s$reference, names(sort(s$doses)))
    d <- scored[scored$condition %in% conds, ]
    d$step <- match(d$condition, conds) - 1
    for (g in split(d, list(d$id_num, d$analyte), drop = TRUE)) {
      sl <- function(y) { ok <- !is.na(y); if (sum(ok) < 3) return(NA_real_)
        x <- g$step[ok]; y <- y[ok]; sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2) }
      out[[length(out) + 1]] <- data.frame(id_num = g$id_num[1], series = nm, analyte = g$analyte[1],
                                           before = sl(g$before), after = sl(g$after), stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(data.frame())
  do.call(rbind, out)
}

# missing & data entry, one row per participant (or per unreadable row)
.lpq_entry <- function(raw, integrity, missing, unparsed) {
  rows <- list()
  add <- function(id, problem) if (length(id)) rows[[length(rows) + 1]] <<- data.frame(id = id, problem = problem, stringsAsFactors = FALSE)
  ig <- integrity
  if (nrow(unparsed)) add(unparsed$id, paste0("id could not be read (excel row ", unparsed$excel_row, ")"))
  if (nrow(ig$label_mismatch)) add(ig$label_mismatch$id_num, paste0("different study prefix: '", ig$label_mismatch$id, "'"))
  if (nrow(ig$duplicates)) {
    d <- ig$duplicates
    g <- stats::aggregate(excel_row ~ id_num + condition, d, function(z) paste(z, collapse = ", "))
    add(g$id_num, paste0(g$condition, " entered more than once (excel rows ", g$excel_row, ")"))
  }
  if (nrow(ig$unexpected)) add(ig$unexpected$id_num, paste0("unknown condition '", ig$unexpected$condition, "' (excel row ", ig$unexpected$excel_row, ")"))
  ex <- raw[raw$round != "original", ]
  if (nrow(ex)) add(ex$id_num, paste0(ex$round, " row still in sheet: '", ex$id, "' (excel row ", ex$.row, ")"))
  m <- missing
  if (nrow(m)) add(m$id_num, ifelse(m$issue == "condition missing", paste0(m$condition, " missing"),
                                    paste0(m$condition, ": ", m$analyte, " blank")))
  if (!length(rows)) return(data.frame(id = character(0), problem = character(0), stringsAsFactors = FALSE))
  r <- do.call(rbind, rows)
  g <- stats::aggregate(problem ~ id, r, function(z) paste(unique(z), collapse = "; "))
  g[order(.lqa_idnum(g$id), g$id), ]
}
