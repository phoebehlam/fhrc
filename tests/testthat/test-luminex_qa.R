# Every planted problem in luminex_example() must be caught, stated plainly,
# and no other participant should get a data / bench-error / dose-response row.

qa <- luminex_qa(luminex_example(study = "OTR", il10 = TRUE, repeats = TRUE), il10 = TRUE)
sm <- qa$summary
has <- function(id, issue, pattern = ".") {
  any(sm$participant == id & sm$issue == issue & grepl(pattern, sm$detail, fixed = !identical(pattern, ".")))
}

test_that("ids are parsed: any study letters, 3-4 digit ids, repeat / interassay suffixes", {
  p <- fhrc:::.lqa_parse_ids(c("MHS_1000_V1_crt_5.5", "OTR_101_V3_hsp60_interassay",
                               "OTR_1371_V3_us_repeat2", "OTR_1001_V1_il10_.02",
                               "OTR_10011_V1_lps", "OTR_1001_1_lps", "garbage"))
  expect_equal(p$study[1:4], c("MHS", "OTR", "OTR", "OTR"))
  expect_equal(p$id_num[1:4], c("1000", "101", "1371", "1001"))
  expect_equal(p$condition, c("crt_5.5", "hsp60", "us", "il10_.02", NA, NA, NA))
  expect_equal(p$round, c("original", "interassay", "repeat2", "original", NA, NA, NA))
  expect_equal(p$parsed, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(qa$counts$extra_rows, 12)
})

test_that("planted swapped wells are stated as the broken order only", {
  expect_true(has("1003", "us > r848", "il1b, il6, tnfa"))
  expect_true(has("1003", "us > hsp60", "il1b, il6, tnfa"))
  # crt_5.5 / crt_7 swapped: shows up as a dip at crt_7, no wrong-order pair is listed
  expect_true(has("1043", "dose-response dip/spike", "cortisol: dip at crt_7 (il1b, il6, tnfa)"))
  expect_false(any(grepl("^crt_[0-9.]+ > crt", c(sm$issue, qa$low_dose$test))))
})

test_that("missing stimulant is stated plainly; 'similar to us' is gone", {
  expect_false(any(grepl("similar|stimulant missing|low lps", c(sm$issue, sm$detail))))
  expect_true(has("1007", "crt > 2x lps", "crt_5.5 (il1b, il6, tnfa)"))
  expect_true(has("1007", "il10 > 2x lps", "il10_.5 (il1b, il6, tnfa)"))
  expect_false(any(grepl("swap", c(sm$detail, qa$low_dose$cytokines))))
})

test_that("inhibitor wells are tagged only above 2x LPS", {
  expect_true(has("1023", "crt > 2x lps", "crt_6 (il1b, il6, tnfa)"))
  cc <- qa$contrasts[qa$contrasts$lower != "us" & !is.na(qa$contrasts$diff), ]
  expect_equal(cc$flag, cc$lower_value > 2 * cc$higher_value)
  q <- luminex_qa(luminex_example(study = "OTR", il10 = TRUE), il10 = TRUE, inhibitor_fold = 1)
  expect_true(any(q$summary$issue == "crt > lps"))
})

test_that("data entry and missing data problems are stated plainly", {
  expect_true(has("1011", "missing data", "crt_6 missing"))
  expect_true(has("1015", "duplicated data", "hsp60 entered more than once"))
  expect_true(has("1019", "unknown condition", "'lsp'"))
  expect_true(has("1019", "missing data", "lps missing"))
  expect_true(has("1035", "missing data", "r848: tnfa blank"))
})

test_that("small reversals are found; plates with a run of the same problem are listed", {
  expect_true(has("1027", "us > hsp60", "il1b"))
  expect_false(any(sm$issue == "plate"))
  pr <- qa$plates
  expect_true(all(grepl("Plate 12", pr$plate)))
  expect_true(any(pr$issue == "us > lps" & pr$participants == "1044, 1045, 1046" & pr$n == 3))
  # two in a row is not enough
  d <- luminex_example(study = "OTR", il10 = TRUE)
  d <- d[!grepl("_1046_", d$id), ]
  expect_false(any(luminex_qa(d, il10 = TRUE)$plates$issue == "us > lps"))
})

test_that("summary states problems only: no percentages, guesses or attributions", {
  expect_false(any(grepl("%", sm$detail)))
  expect_false(any(grepl("did you mean|explains|swap|repeat run|noise|CV|\\?", c(sm$detail, qa$low_dose$cytokines))))
  expect_false("priority" %in% names(sm))
})

test_that("no unplanted participant gets a data, bench-error or dose-response row", {
  planted <- c("1003", "1007", "1011", "1015", "1019", "1023", "1035", "1039", "1043", "1044", "1045", "1046")
  serious <- sm$issue %in% c("duplicated data", "unknown condition", "missing data", "us > r848", "us > lps",
                             "crt > 2x lps", "il10 > 2x lps", "dose-response dip/spike")
  got <- unique(sm$participant[serious & grepl("^[0-9]+$", sm$participant)])
  expect_setequal(got, planted)
})

test_that("comparison flags match a plain hand calculation", {
  v <- qa$values
  get <- function(cond) v[v$condition == cond & v$analyte == "il6", c("id_num", "value")]
  h <- merge(get("hsp60"), get("us"), by = "id_num", suffixes = c("_hsp60", "_us"))
  by_hand <- sort(h$id_num[h$value_hsp60 < h$value_us])
  by_fn <- sort(qa$contrasts$id_num[qa$contrasts$test == "us > hsp60" &
                                       qa$contrasts$analyte == "il6" & qa$contrasts$flag])
  expect_equal(by_fn, by_hand)
})

test_that("two visits in one file are fine: no visit flag, participants kept apart by visit", {
  d1 <- luminex_example(n = 8, repeats = FALSE)
  d2 <- d1; d2$id <- sub("_V1_", "_V2_", d2$id)
  q <- luminex_qa(rbind(d1, d2))
  expect_equal(q$counts$participants, 16)
  expect_true(all(grepl("_V[12]$", unique(q$values$id_num))))
  expect_false(any(grepl("visit", q$summary$detail)))
  expect_equal(nrow(q$integrity$duplicates), 0)
  expect_equal(q$settings$visit, "V1 & V2")
})

test_that("dose-response uses ln(MFI + 1) and tags dips / spikes seen in 2+ cytokines", {
  # by hand: ln(max(well, 0) + 1), no US subtraction
  v <- qa$values
  get <- function(cond, a) v$value[v$id_num == "1005" & v$condition == cond & v$analyte == a]
  dp <- qa$dose_points
  expect_equal(dp$score[dp$id_num == "1005" & dp$series == "cortisol" & dp$analyte == "il6" & dp$step == 0],
               log(max(get("lps", "il6"), 0) + 1))
  expect_equal(dp$score[dp$id_num == "1005" & dp$series == "cortisol" & dp$analyte == "il6" & dp$step == 4],
               log(max(get("crt_5.5", "il6"), 0) + 1))
  expect_equal(dp$condition[dp$id_num == "1005" & dp$series == "cortisol" & dp$analyte == "il6"],
               c("lps", "crt_7", "crt_6.5", "crt_6", "crt_5.5"))
  expect_false(any(grepl("slope|no substantive|goes up", sm$detail)))
  dip_test <- function(cond, analytes = c("il1b", "il6")) {
    d <- luminex_example(study = "OTR", il10 = TRUE)
    for (a in analytes) {
      col <- paste0(a, "_avg_mfi")
      d[[col]][d$id == paste0("OTR_1010_V1_", cond)] <- d[[col]][d$id == "OTR_1010_V1_us"] + 2
    }
    luminex_qa(d, il10 = TRUE)$dose_issues
  }
  # a sudden dip in two cytokines is tagged, including at a low-dose well
  di <- dip_test("crt_6")
  expect_true(any(di$id_num == "1010" & grepl("dip at crt_6 (il1b, il6)", di$text, fixed = TRUE)))
  di <- dip_test("crt_7")
  expect_true(any(di$id_num == "1010" & grepl("dip at crt_7 (il1b, il6)", di$text, fixed = TRUE)))
  # ... but not when it is in one cytokine only
  di <- dip_test("crt_6", "il1b")
  expect_false(any(di$id_num == "1010"))
  # a spike in two cytokines is tagged
  d <- luminex_example(study = "OTR", il10 = TRUE)
  k <- d$id == "OTR_1010_V1_crt_6.5"
  d$tnfa_avg_mfi[k] <- d$tnfa_avg_mfi[k] * 20
  d$il6_avg_mfi[k] <- d$il6_avg_mfi[k] * 20
  di <- luminex_qa(d, il10 = TRUE)$dose_issues
  expect_true(any(di$id_num == "1010" & grepl("spike at crt_6.5 (il6, tnfa)", di$text, fixed = TRUE)))
  # a rise at the last dose (crt_5.5) is not a dip or spike
  d <- luminex_example(study = "OTR", il10 = TRUE)
  k <- d$id == "OTR_1010_V1_crt_5.5"
  d$il6_avg_mfi[k] <- d$il6_avg_mfi[d$id == "OTR_1010_V1_crt_6"] * 1.4
  di <- luminex_qa(d, il10 = TRUE)$dose_issues
  expect_false(any(di$id_num == "1010" & di$series == "cortisol"))
})

test_that("il10 / hsp60 / r848 switches work", {
  q <- luminex_qa(luminex_example(il10 = FALSE))
  expect_false("il10" %in% names(q$settings$dose_series))
  expect_false(any(grepl("il10", q$contrasts$test)))
  expect_false(any(grepl("il10", q$summary$detail)))

  q <- luminex_qa(luminex_example(il10 = TRUE), il10 = FALSE)
  expect_true("il10_.5" %in% q$settings$switched_off)
  expect_false(any(q$summary$participant == "(file)"))
  expect_false(any(grepl("il10", q$contrasts$test)))
  expect_false(any(q$integrity$unexpected$condition %in% c("il10_.02", "il10_.1", "il10_.5")))

  q <- luminex_qa(luminex_example(il10 = FALSE), il10 = TRUE)
  expect_true("il10_.5" %in% q$settings$never_seen)
  expect_false(any(q$summary$participant == "(file)"))
  expect_false(any(grepl("il10", q$missing$condition)))

  q <- luminex_qa(luminex_example(), hsp60 = FALSE, r848 = FALSE)
  expect_false(any(q$contrasts$higher %in% c("hsp60", "r848")))
  expect_false(any(c("hsp60", "r848") %in% q$settings$expected_conditions))
})

test_that("a sheet with ids but no values does not crash", {
  f <- test_path("fake_one_row_master.xlsx")
  for (sh in c("for master", "final master")) {
    q <- luminex_qa(f, sheet = sh)
    expect_s3_class(q, "luminex_qa")
    expect_equal(q$counts$participants, 1)
    expect_true(all(q$missing$issue == "value missing"))
  }
})

test_that("expected_ids reports people not in the data", {
  q <- luminex_qa(luminex_example(), expected_ids = c(1000:1047, 1099))
  expect_true(any(q$missing$id_num == "1099" & q$missing$issue == "expected participant not in data"))
})

test_that("every reversal at the regular wells is in the summary; low-dose ones are kept separate", {
  core <- qa$contrasts[qa$contrasts$flag & qa$contrasts$tier == "core", ]
  # every flagged participant x regular check shows up under its issue label
  for (i in seq_len(nrow(core))) {
    fam <- fhrc:::.lqa_family(core$lower[i], core$higher[i], core$fold[i])
    expect_true(any(sm$participant == core$id_num[i] & sm$issue == fam & grepl(core$analyte[i], sm$detail, fixed = TRUE)),
                info = paste(core$id_num[i], core$test[i], core$analyte[i]))
  }
  low <- qa$contrasts[qa$contrasts$flag & qa$contrasts$tier == "sensitive", ]
  expect_true(all(unique(low$id_num) %in% qa$low_dose$participant))
  expect_false(any(grepl("crt_7|crt_6.5|il10_.02", sm$detail[sm$issue != "dose-response dip/spike"])))
  # every low-dose item involves a low-dose well
  expect_true(all(grepl("crt_7|crt_6\\.5|il10_\\.02", qa$low_dose$test)))
})

test_that("messy real-world values do not crash any setting (zeros, negatives, blanks, text)", {
  set.seed(7)
  base <- luminex_example(il10 = TRUE)
  mfi_cols <- grep("_avg_mfi$", names(base), value = TRUE)
  us <- grepl("_us$", base$id)
  messy <- list(
    zeros_in_us   = within(base, il1b_avg_mfi[us & runif(nrow(base)) < 0.7] <- 0),
    all_us_zero   = within(base, il1b_avg_mfi[us] <- 0),
    negative_us   = within(base, il6_avg_mfi[us & runif(nrow(base)) < 0.6] <- -4.2),
    blank_us      = within(base, tnfa_avg_mfi[us] <- NA),
    one_us_value  = within(base, { il1b_avg_mfi[us] <- NA; il1b_avg_mfi[which(us)[1]] <- 30 }),
    zero_lps      = within(base, il6_avg_mfi[grepl("_lps$", id)] <- 0),
    text_values   = { b <- base; b$il6_avg_mfi <- as.character(b$il6_avg_mfi); b$il6_avg_mfi[1:20] <- "NaN"; b },
    identical_us  = within(base, il1b_avg_mfi[us] <- 25),
    two_people    = base[grepl("_100[01]_", base$id), ]
  )
  settings <- expand.grid(hsp60 = c(TRUE, FALSE), r848 = c(TRUE, FALSE), il10 = c(TRUE, FALSE))
  for (nm in names(messy)) for (k in seq_len(nrow(settings))) {
    st <- settings[k, ]
    q <- luminex_qa(messy[[nm]], hsp60 = st$hsp60, r848 = st$r848, il10 = st$il10)
    expect_s3_class(q, "luminex_qa")
    if (length(q$dose) && nrow(q$dose)) expect_false(anyNA(q$dose$issue))
  }
})

test_that("cortisol and IL-10 wells above LPS are one category each in the summary", {
  # crt_6 well swapped with the LPS well: lands in "crt > 2x lps"
  d <- luminex_example(study = "OTR", il10 = TRUE)
  a <- d$id == "OTR_1012_V1_lps"; b <- d$id == "OTR_1012_V1_crt_6"
  d$id[a] <- "OTR_1012_V1_crt_6"; d$id[b] <- "OTR_1012_V1_lps"
  q <- luminex_qa(d, il10 = TRUE)
  expect_false(any(grepl("^(crt|il10)_[0-9.]+ > ", q$summary$issue)))
  expect_true(any(q$summary$participant == "1012" & q$summary$issue == "crt > 2x lps" &
                    grepl("crt_6 (", q$summary$detail, fixed = TRUE)))
  expect_false(any(grepl("[A-Z]", q$summary$issue)))      # all lower case
})

test_that("cortisol wells rising with dose (but under 2x LPS) are not tagged", {
  d <- luminex_example(study = "OTR")
  f <- c(crt_7 = 1.1, crt_6.5 = 1.3, crt_6 = 1.6, crt_5.5 = 1.9)
  for (cnd in names(f)) for (a in c("il1b", "il6", "tnfa")) {
    col <- paste0(a, "_avg_mfi")
    d[[col]][d$id == paste0("OTR_1010_V1_", cnd)] <- d[[col]][d$id == "OTR_1010_V1_lps"] * f[[cnd]]
  }
  q <- luminex_qa(d)
  expect_false(any(q$summary$participant == "1010"))
  expect_true(any(q$low_dose$participant == "1010") == FALSE)
})
