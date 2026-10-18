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
#' A participant is tagged when, as scored:
#' \describe{
#'   \item{scores 0}{a stimulated or inhibitor well comes out as 0 in at least
#'     `dip_cytokines` cytokines, either because it is about 0 as measured
#'     (`zero_mfi` MFI or less) or because it is below US.}
#'   \item{us replicates disagree}{the US well's replicate CV is above `us_cv`\%
#'     while US is at least `us_min` MFI (US is subtracted from every well).}
#'   \item{dose-response dip/spike}{one dose well is more than `dip_size` below
#'     (above) both neighbouring doses in at least `dip_cytokines` cytokines.}
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
#'
#' **Changing the plan.** List a participant in `na`, `ignore_us` or `keep`
#' and what you list replaces the default plan for that participant (only list
#' the participants you disagree with). `keep` means leave the participant as is.
#'
#' @param data Path to the master `.xlsx`, or a data frame.
#' @param sheet Sheet name (default `"final master"`).
#' @param hsp60,r848,il10 Which wells the study has (as in [luminex_qa()]).
#' @param na Wells to set to NA, as sample ids with or without the study
#'   prefix, e.g. `c("1139_V1_crt_7", "MHS_1066_V1_r848")`.
#' @param ignore_us Participants whose US well is treated as 0, e.g.
#'   `c("2066_V2", "2067_V2")`.
#' @param keep Participants to leave as they are (no default plan).
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
                           na = NULL,
                           ignore_us = NULL,
                           keep = NULL,
                           inhibitor_fold = 2,
                           dip_size = 1,
                           dip_cytokines = 2,
                           zero_mfi = 5,
                           us_cv = 30,
                           us_min = 10,
                           study = NULL,
                           visit = NULL) {
  contrasts <- luminex_contrasts(hsp60 = hsp60, r848 = r848, il10 = il10, inhibitor_fold = inhibitor_fold)
  dose_series <- luminex_dose_series(il10 = il10)

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
            hsp60 = hsp60, r848 = r848, il10 = il10, analytes = analytes,
            contrasts = contrasts, dose_series = dose_series, expected_conditions = expected_conditions,
            never_seen = never_seen, switched_off = switched_off, wells = wells, stims = stims,
            inhibitor_fold = inhibitor_fold, dip_size = dip_size, dip_cytokines = dip_cytokines,
            zero_mfi = zero_mfi, us_cv = us_cv, us_min = us_min, run_at = Sys.time())

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
  default <- .lpq_default_plan(vals, S, wid)
  user <- .lpq_user_plan(na, ignore_us, keep, people, wid)
  plan <- default[!default$participant %in% user$participant, , drop = FALSE]
  plan <- rbind(plan, user)
  plan <- plan[order(.lqa_idnum(plan$participant), plan$participant, plan$set_by != "you",
                     match(plan$action, c("ignore us", "na", "keep"))), , drop = FALSE]
  rownames(plan) <- NULL

  # ---- before and after the plan -------------------------------------------------------
  vals_after <- .lpq_apply_long(vals, plan)
  ignored <- unique(plan$participant[plan$action == "ignore us"])
  before <- .lpq_problems(vals, S)
  after <- .lpq_problems(vals_after, S, us_ignored = ignored)
  scored <- .lpq_scored(vals, vals_after, S)
  slopes <- .lpq_slopes(scored, S)

  tagged <- unique(c(before$participant, plan$participant))
  tagged <- tagged[order(.lqa_idnum(tagged), tagged)]

  out <- list(settings = S, data = raw, fixed = .lpq_apply_df(original, plan), values = vals, values_after = vals_after,
              entry = entry, integrity = integrity, missing = missing,
              problems = before, problems_after = after, plan = plan, default_plan = default,
              scored = scored, slopes = slopes, tagged = tagged)
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
    for (i in seq_len(nrow(p))) cat("   ", p$participant[i], "|", p$action[i], "|", p$id[i], "|", p$set_by[i], "\n")
  }
  cat("  run luminex_postqa_report() for the report; add export = TRUE once the plan is final.\n")
  invisible(x)
}


# the plan applied to the master sheet (every row and column kept, plus a `fix` column):
#   na        every cytokine value of the well (MFI, replicates, concentration, CV) -> NA
#   ignore us the participant's US well *_avg_mfi -> 0, so subtracting US changes nothing
#   keep      nothing
.lpq_apply_df <- function(data, plan) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  ids <- trimws(as.character(data$id))
  data$fix <- NA_character_
  an <- sub("_avg_mfi$", "", grep("_avg_mfi$", names(data), value = TRUE))
  val_cols <- grep(paste0("^(", paste(an, collapse = "|"), ")_"), names(data), value = TRUE)
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
  data.frame(participant = character(0), action = character(0), id = character(0),
             set_by = character(0), why = character(0), stringsAsFactors = FALSE)
}

.lpq_row <- function(p, action, id, why, set_by = "default") {
  data.frame(participant = p, action = action, id = id, set_by = set_by, why = why, stringsAsFactors = FALSE)
}

# the same plan as .lpq_apply_df(), on the long values table
.lpq_apply_long <- function(vals, plan) {
  if (!nrow(plan)) return(vals)
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
  if (nrow(z)) out[[length(out) + 1]] <- data.frame(issue = "scores 0", detail = grouped(
    z$well, paste0(ifelse(z$type == "zero", "about 0 as measured", "below us"), ": ", z$cytokines)), stringsAsFactors = FALSE)
  u <- pr[pr$type == "us_cv", ]
  if (nrow(u)) out[[length(out) + 1]] <- data.frame(issue = "us replicates disagree", detail = u$extra[1],
                                                    stringsAsFactors = FALSE)
  d <- pr[pr$type %in% c("dip", "spike"), ]
  if (nrow(d)) out[[length(out) + 1]] <- data.frame(issue = "dose-response dip/spike", detail = paste(
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

.lpq_default_plan <- function(vals, S, wid) {
  plan <- .lpq_empty_plan()
  idof <- function(p, cnd) wid$id[wid$id_num == p & wid$condition == cnd][1]
  add <- function(p, action, cnd, why) {
    i <- idof(p, cnd)
    if (is.na(i) || any(plan$participant == p & plan$action == action & plan$id == i)) return(invisible())
    plan <<- rbind(plan, .lpq_row(p, action, i, why))
  }
  # 1-2: wells about 0, wells below US, US replicates that disagree
  pr <- .lpq_problems(vals, S)
  for (p in unique(pr$participant)) {
    r <- pr[pr$participant == p, ]
    for (i in which(r$type == "zero")) add(p, "na", r$well[i], paste0("about 0 as measured (", r$cytokines[i], ")"))
    below <- r[r$type == "below_us", ]
    cvp <- r[r$type == "us_cv", ]
    if (nrow(below) >= 2 || nrow(cvp)) {
      why <- c(if (nrow(below) >= 2) paste0(nrow(below), " wells below us (", paste(below$well, collapse = ", "), ")"),
               if (nrow(cvp)) paste0("us replicates disagree (", cvp$extra[1], ")"))
      add(p, "ignore us", "us", paste(why, collapse = "; "))
    } else if (nrow(below) == 1) {
      add(p, "na", below$well, paste0("below us (", below$cytokines, ")"))
    }
  }
  ign <- function() unique(plan$participant[plan$action == "ignore us"])
  # 3: dips and spikes on the scored scale
  for (pass in 1:3) {
    pr <- .lpq_problems(.lpq_apply_long(vals, plan), S, us_ignored = ign())
    d <- pr[pr$type %in% c("dip", "spike"), ]
    if (!nrow(d)) break
    for (i in seq_len(nrow(d))) add(d$participant[i], "na", d$well[i], paste0(d$type[i], " at ", d$well[i], " (", d$cytokines[i], ")"))
  }
  # 4: inhibitor wells above k x LPS -> LPS NA; if the inhibitor wells still rise with dose -> those NA too
  v1 <- .lpq_apply_long(vals, plan)
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
  v2 <- .lpq_apply_long(vals, plan)
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
.lpq_user_plan <- function(na, ignore_us, keep, people, wid) {
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
