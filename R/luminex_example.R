#' Simulate a fake Luminex cell-culture master file (for training and testing)
#'
#' Builds a master sheet in exactly the layout produced by [luminex()] (one row
#' per well-condition, columns such as `il6_avg_mfi`, `il6_mfi_cv`, `plate`),
#' with a handful of problems deliberately planted so that every check in
#' [luminex_qa()] has something to find. Use it to try out the QA report
#' before running it on real data, or to train new staff.
#'
#' Planted problems (participant id numbers):
#' \describe{
#'   \item{1003}{US and R848 labels swapped}
#'   \item{1007}{LPS not added to the LPS-only well}
#'   \item{1011}{crt_6 well missing}
#'   \item{1015}{hsp60 row entered twice}
#'   \item{1019}{lps typed as "lsp"}
#'   \item{1023}{LPS-only well reads low, so the cortisol wells read more than 2x LPS}
#'   \item{1027}{small IL-1b reversal (hsp60 slightly below US), within noise}
#'   \item{1035}{TNF-a value missing for the r848 well}
#'   \item{1043}{crt_5.5 and crt_7 labels swapped (shows as a dip at crt_7)}
#'   \item{1044-1046}{US and LPS wells swapped for three participants next to
#'     each other on plate 12 (listed under Plates)}
#'   \item{plate 09}{every value on the plate reads about 2.5x low}
#' }
#'
#' @param file Optional path to an `.xlsx` file to write. If `NULL` (default)
#'   the data frame is only returned.
#' @param n Number of participants (default 48, starting at id 1000).
#' @param study Study prefix for the ids (default `"MHS"`).
#' @param il10 Logical. Include IL-10 inhibition wells (`il10_.02`, `il10_.1`,
#'   `il10_.5`)? Default `FALSE`. If `TRUE`, run [luminex_qa()] with
#'   `il10 = TRUE` too.
#' @param repeats Logical. Add `_repeat` and `_interassay` rows? Default
#'   `FALSE` (the QA report is run before repeats).
#' @param seed Random seed.
#'
#' @return A data frame (invisibly if `file` is given).
#' @examples
#' fake <- luminex_example(study = "OTR", il10 = TRUE)
#' head(fake)
#' @export
luminex_example <- function(file = NULL, n = 48, study = "MHS", il10 = FALSE, repeats = FALSE,
                            seed = 2026) {
  set.seed(seed)
  analytes <- c("il1b", "il6", "tnfa")

  # typical fold-change over US (on the MFI scale) for each condition
  fold <- c(us = 1, hsp60 = 6, r848 = 60, lps = 120,
            crt_7 = 120 * 0.85, crt_6.5 = 120 * 0.65,
            crt_6 = 120 * 0.40, crt_5.5 = 120 * 0.22)
  if (isTRUE(il10)) {
    fold <- c(fold, il10_.02 = 120 * 0.80, il10_.1 = 120 * 0.50,
              il10_.5 = 120 * 0.25)
  }
  conds <- names(fold)
  base_us <- c(il1b = 25, il6 = 60, tnfa = 40)

  ids <- 1000 + seq_len(n) - 1
  plate_of <- sprintf(paste(study, "V1 Plate %02d_20260814.xlsx"), (seq_len(n) - 1) %/% 4 + 1)
  names(plate_of) <- ids

  rows <- list()
  for (i in seq_along(ids)) {
    p <- ids[i]
    person <- stats::rnorm(length(analytes), 0, 0.35)   # person-level response
    names(person) <- analytes
    for (cnd in conds) {
      f <- fold[[cnd]]
      if (p == 1023 && cnd == "lps") f <- f * 0.15         # LPS-only well reads low
      vals <- sapply(analytes, function(a) {
        m <- base_us[[a]] * exp(person[[a]]) * f * exp(stats::rnorm(1, 0, 0.12))
        if (grepl("_9_|Plate 09", plate_of[[as.character(p)]])) m <- m / 2.5
        m
      })
      rows[[length(rows) + 1]] <- data.frame(
        id_num = p, condition = cnd, analyte = analytes, mfi = vals,
        stringsAsFactors = FALSE)
    }
  }
  long <- do.call(rbind, rows)

  # ---- planted problems on the true values ----
  swap <- function(d, idn, a, b) {
    ia <- d$id_num == idn & d$condition == a
    ib <- d$id_num == idn & d$condition == b
    d$condition[ia] <- "__tmp__"; d$condition[ib] <- a; d$condition[d$condition == "__tmp__"] <- b
    d
  }
  long <- swap(long, 1003, "us", "r848")
  long <- swap(long, 1043, "crt_5.5", "crt_7")
  for (idn in 1044:1046) long <- swap(long, idn, "us", "lps")
  sel <- long$id_num == 1007 & long$condition == "lps"
  us7 <- long$mfi[long$id_num == 1007 & long$condition == "us"]
  long$mfi[sel] <- us7 * stats::runif(3, 0.9, 1.3)
  sel <- long$id_num == 1027 & long$condition == "hsp60" & long$analyte == "il1b"
  long$mfi[sel] <- long$mfi[long$id_num == 1027 & long$condition == "us" & long$analyte == "il1b"] * 0.96
  sel <- long$id_num == 1031 & long$condition == "lps" & long$analyte == "il6"
  long$mfi[sel] <- 31000

  # ---- replicate wells, CVs, concentrations ----
  make_row <- function(idn, cnd, suffix = "", noise_boost = 1) {
    d <- long[long$id_num == idn & long$condition == cnd, ]
    out <- list(id = paste0(study, "_", idn, "_V1_", cnd, suffix))
    for (a in analytes) {
      m <- d$mfi[d$analyte == a]
      sdv <- if (stats::runif(1) < 0.07) 0.18 else 0.04
      r <- m * exp(stats::rnorm(2, 0, sdv * noise_boost))
      avg <- mean(r)
      conc <- function(x) 0.02 * x^1.05
      c1 <- conc(r[1]); c2 <- conc(r[2]); ca <- conc(avg)
      oor <- avg > 25000
      out[[paste0(a, "_1_mfi")]] <- round(r[1], 1)
      out[[paste0(a, "_2_mfi")]] <- round(r[2], 1)
      out[[paste0(a, "_1_conc")]] <- if (oor) "OOR >" else round(c1, 3)
      out[[paste0(a, "_2_conc")]] <- if (oor) "OOR >" else round(c2, 3)
      out[[paste0(a, "_avg_mfi")]] <- round(avg, 1)
      out[[paste0(a, "_avg_conc")]] <- if (oor) "OOR >" else round(ca, 3)
      out[[paste0(a, "_mfi_cv")]] <- round(abs(100 * stats::sd(r) * sqrt(1 / 2) / avg), 5)
    }
    out$plate <- plate_of[[as.character(idn)]]
    out
  }

  recs <- list()
  for (p in ids) for (cnd in conds) recs[[length(recs) + 1]] <- make_row(p, cnd)

  if (isTRUE(repeats)) {
    for (k in seq(1, n, by = 9)) {
      recs[[length(recs) + 1]] <- make_row(ids[k], "lps", "_repeat", 0.5)
    }
    for (k in seq(2, n, by = 8)) {
      recs[[length(recs) + 1]] <- make_row(ids[k], "hsp60", "_interassay")
    }
  }

  dat <- dplyr::bind_rows(lapply(recs, function(r) {
    as.data.frame(lapply(r, function(v) as.character(v)), stringsAsFactors = FALSE)
  }))

  # ---- planted bookkeeping problems ----
  dat <- dat[dat$id != paste0(study, "_1011_V1_crt_6"), ]
  dat <- rbind(dat, dat[dat$id == paste0(study, "_1015_V1_hsp60"), ])
  dat$id[dat$id == paste0(study, "_1019_V1_lps")] <- paste0(study, "_1019_V1_lsp")
  dat$tnfa_avg_mfi[dat$id == paste0(study, "_1035_V1_r848")] <- NA
  dat$tnfa_1_mfi[dat$id == paste0(study, "_1035_V1_r848")] <- NA
  dat$tnfa_2_mfi[dat$id == paste0(study, "_1035_V1_r848")] <- NA
  dat$tnfa_mfi_cv[dat$id == paste0(study, "_1035_V1_r848")] <- NA
  dat$tnfa_avg_conc[dat$id == paste0(study, "_1035_V1_r848")] <- NA

  # numeric columns back to numeric where possible (conc columns stay mixed)
  num_cols <- grep("_mfi$|_mfi_cv$", names(dat), value = TRUE)
  for (cc in num_cols) dat[[cc]] <- suppressWarnings(as.numeric(dat[[cc]]))

  col_order <- c("id", "il1b_mfi_cv", "il6_mfi_cv", "tnfa_mfi_cv",
                 "il1b_1_mfi", "il1b_2_mfi", "il1b_1_conc", "il1b_2_conc", "il1b_avg_mfi", "il1b_avg_conc",
                 "il6_1_mfi", "il6_2_mfi", "il6_1_conc", "il6_2_conc", "il6_avg_mfi", "il6_avg_conc",
                 "tnfa_1_mfi", "tnfa_2_mfi", "tnfa_1_conc", "tnfa_2_conc", "tnfa_avg_mfi", "tnfa_avg_conc",
                 "plate")
  dat <- dat[order(dat$id), col_order]
  rownames(dat) <- NULL

  if (!is.null(file)) {
    openxlsx::write.xlsx(list("for master" = dat), file, rowNames = FALSE)
    return(invisible(dat))
  }
  dat
}
