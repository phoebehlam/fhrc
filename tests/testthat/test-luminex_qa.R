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

test_that("planted label swaps are stated as the broken pattern plus 'label swap?'", {
  expect_true(has("1003", "us > r848", "label swap?"))
  expect_true(has("1003", "us > hsp60", "il1b, il6, tnfa"))
  expect_true(has("1043", "crt_5.5 > crt_7", "label swap?"))
})

test_that("missing stimulant is stated, not mistaken for a swap", {
  expect_true(has("1007", "low lps-only well", "lps ~ us, stimulant missing?"))
  expect_true(has("1007", "crt > lps", "crt_5.5 (il1b, il6, tnfa)"))
  expect_true(has("1007", "il10 > lps", "il10_.5 (il1b, il6, tnfa)"))
  expect_false(any(sm$participant == "1007" & grepl("label swap", sm$detail)))
})

test_that("data entry and missing data problems are stated plainly", {
  expect_true(has("1011", "missing data", "crt_6 missing"))
  expect_true(has("1015", "duplicated data", "hsp60 entered more than once"))
  expect_true(has("1019", "unknown condition", "'lsp'"))
  expect_true(has("1019", "missing data", "lps missing"))
  expect_true(has("1035", "missing data", "r848: tnfa blank"))
})

test_that("dose-response, pattern and plate problems are found", {
  expect_true(has("1023", "no dose-response", "cortisol: no substantive decrease with dose in"))
  expect_true(has("1027", "us > hsp60", "il1b"))
  expect_true(any(sm$issue == "plate" & grepl("Plate 09", sm$plate)))
})

test_that("summary states problems only: no percentages, guesses or attributions", {
  expect_false(any(grepl("%", sm$detail)))
  expect_false(any(grepl("did you mean|explains|swapping|repeat run|noise|CV", sm$detail)))
  expect_false("priority" %in% names(sm))
})

test_that("no unplanted participant gets a data, bench-error or dose-response row", {
  planted <- c("1003", "1007", "1011", "1015", "1019", "1023", "1035", "1043")
  serious <- sm$issue %in% c("duplicated data", "unknown condition", "missing data", "no dose-response") |
    grepl("\\?", sm$detail)
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

test_that("a missing LPS well is not a dose-response problem", {
  expect_false(has("1019", "no dose-response"))
  expect_false(has("1007", "no dose-response"))
})

test_that("il10 / hsp60 / r848 switches work", {
  q <- luminex_qa(luminex_example(il10 = FALSE))
  expect_false("il10" %in% names(q$settings$dose_series))
  expect_false(any(grepl("il10", q$contrasts$test)))
  expect_false(any(grepl("il10", q$summary$detail)))

  q <- luminex_qa(luminex_example(il10 = TRUE), il10 = FALSE)
  expect_true(any(q$summary$issue == "set-up" & grepl("il10 = FALSE", q$summary$detail)))
  expect_false(any(grepl("il10", q$contrasts$test)))
  expect_false(any(q$integrity$unexpected$condition %in% c("il10_.02", "il10_.1", "il10_.5")))

  q <- luminex_qa(luminex_example(il10 = FALSE), il10 = TRUE)
  expect_true(any(q$summary$issue == "set-up" & grepl("il10_.5", q$summary$detail)))
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
    fam <- fhrc:::.lqa_family(core$lower[i], core$higher[i])
    expect_true(any(sm$participant == core$id_num[i] & sm$issue == fam & grepl(core$analyte[i], sm$detail, fixed = TRUE)),
                info = paste(core$id_num[i], core$test[i], core$analyte[i]))
  }
  low <- qa$contrasts[qa$contrasts$flag & qa$contrasts$tier == "sensitive", ]
  expect_setequal(unique(qa$low_dose$participant), unique(low$id_num))
  expect_false(any(grepl("crt_7|crt_6.5|il10_.02", sm$detail)))
  expect_true(all(qa$low_dose$test %in% c("crt_7 > lps", "crt_6.5 > lps", "il10_.02 > lps")))
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
    expect_false(anyNA(q$dose$weak_vs_cohort))
  }
})
