# Pre-scoring QA (after repeats): problems as the scoring step sees them, the
# default plan, the team's overrides, and the exported data with the plan applied.

d <- luminex_example(study = "MHS")
qa <- luminex_postqa(d)
plan_has <- function(plan, p, action, well = NULL) {
  any(plan$participant == p & plan$action == action &
        (is.null(well) | grepl(paste0("_", well, "$"), plan$id)))
}

test_that("default plan handles each planted problem, and nobody else", {
  pl <- qa$plan
  expect_true(plan_has(pl, "1003", "ignore us"))            # US/R848 swapped: several wells below US
  expect_true(plan_has(pl, "1007", "na", "lps"))            # LPS not added: cortisol wells > 2x LPS
  expect_true(plan_has(pl, "1023", "na", "lps"))
  expect_true(plan_has(pl, "1039", "na", "crt_6.5"))        # dip
  for (p in c("1044", "1045", "1046")) {                    # US and LPS swapped
    expect_true(plan_has(pl, p, "ignore us"))
    expect_true(plan_has(pl, p, "na", "lps"))
  }
  expect_setequal(unique(pl$participant), c("1003", "1007", "1023", "1039", "1044", "1045", "1046"))
  expect_setequal(unique(pl$action), c("ignore us", "na"))
  expect_true(all(pl$set_by == "default"))
  expect_equal(nrow(qa$problems_after), 0)
})

test_that("missing & data entry is one compact row per participant", {
  e <- qa$entry
  expect_equal(anyDuplicated(e$id), 0)
  expect_true(any(e$id == "1011" & grepl("crt_6 missing", e$problem)))
  expect_true(any(e$id == "1015" & grepl("hsp60 entered more than once", e$problem)))
  expect_true(any(e$id == "1019" & grepl("unknown condition 'lsp'", e$problem) & grepl("lps missing", e$problem)))
  expect_true(any(e$id == "1035" & grepl("r848: tnfa blank", e$problem)))
  q <- luminex_postqa(luminex_example(study = "MHS", repeats = TRUE))
  expect_true(any(grepl("repeat row still in sheet", q$entry$problem)))
})

test_that("scored values follow the scoring script", {
  v <- qa$values
  get <- function(p, cnd, a) v$value[v$id_num == p & v$condition == cnd & v$analyte == a]
  s <- qa$scored
  by_hand <- log(max(max(get("1005", "crt_6", "il6"), 0) - max(get("1005", "us", "il6"), 0), 0) + 1)
  expect_equal(s$before[s$id_num == "1005" & s$condition == "crt_6" & s$analyte == "il6"], by_hand)
  by_hand <- log(max(get("1003", "crt_6", "il6"), 0) + 1)    # after "ignore us", US is 0
  expect_equal(s$after[s$id_num == "1003" & s$condition == "crt_6" & s$analyte == "il6"], by_hand)
})

test_that("what the team lists replaces the default plan for that participant only", {
  q <- luminex_postqa(d, na = c("1044_V1_lps", "MHS_1039_V1_crt_6.5", "1039_V1_crt_6"), keep = "1045")
  pl <- q$plan
  expect_equal(pl$action[pl$participant == "1044"], "na")    # listed: the default "ignore us" is gone
  expect_true(all(pl$set_by[pl$participant %in% c("1039", "1044", "1045")] == "you"))
  expect_equal(pl$action[pl$participant == "1045"], "keep")
  expect_true(plan_has(pl, "1046", "ignore us"))              # not listed: default stays
  expect_equal(sum(pl$participant == "1039"), 2)
  expect_true(any(q$problems_after$participant == "1044"))    # still wells below US
  expect_true(any(q$problems_after$participant == "1045"))    # kept as is
  expect_false(any(q$problems_after$participant == "1039"))
  expect_true(all(c("1044", "1045") %in% q$tagged))
  expect_error(luminex_postqa(d, swap = list(c("1044_V1_us", "1044_V1_lps"))), "unused argument")
})

test_that("the fixed data is the whole sheet with the plan applied", {
  f <- qa$fixed
  expect_equal(nrow(f), nrow(d))
  expect_equal(f$id, d$id)
  expect_equal(setdiff(names(f), names(d)), "fix")
  rest <- is.na(f$fix)
  expect_equal(f[rest, names(d)], d[rest, names(d)], ignore_attr = TRUE)
  cyt <- c("il1b_avg_mfi", "il6_avg_mfi", "tnfa_avg_mfi")
  expect_true(all(is.na(f[f$id == "MHS_1039_V1_crt_6.5", grep("^(il1b|il6|tnfa)_", names(d))])))
  expect_true(all(f[f$id == "MHS_1003_V1_us", cyt] == 0))
  expect_equal(f$il1b_1_mfi[f$id == "MHS_1003_V1_us"], d$il1b_1_mfi[d$id == "MHS_1003_V1_us"])
  expect_equal(f$fix[f$id == "MHS_1039_V1_crt_6.5"], "set to NA")
  # re-reading the fixed data gives the values the report showed after the plan
  q2 <- luminex_postqa(f[, names(d)], keep = qa$tagged)
  va <- qa$values_after[order(qa$values_after$id, qa$values_after$analyte), "value"]
  vb <- q2$values[order(q2$values$id, q2$values$analyte), "value"]
  expect_equal(va, vb)
})

test_that("ids are accepted with or without the study prefix; mistakes are explained", {
  q1 <- luminex_postqa(d, na = "MHS_1039_V1_crt_6.5")
  q2 <- luminex_postqa(d, na = "1039_V1_crt_6.5")
  expect_equal(q1$plan, q2$plan)
  expect_error(luminex_postqa(d, na = "1039_V1_crt_9"), "Well not found")
  expect_error(luminex_postqa(d, ignore_us = "9999"), "Participant not found")
  d2 <- rbind(d, transform(d[grepl("_1005_", d$id), ], id = sub("_V1_", "_V2_", id)))
  expect_error(luminex_postqa(d2, ignore_us = "1005"), "more than one visit")
  expect_silent(luminex_postqa(d2, ignore_us = "1005_V2"))
})

test_that("messy values do not crash", {
  us <- grepl("_us$", d$id)
  for (m in list(within(d, il1b_avg_mfi[us] <- 0), within(d, il6_avg_mfi[us] <- -3),
                 within(d, tnfa_avg_mfi[us] <- NA), within(d, il6_avg_mfi[grepl("_lps$", id)] <- 0),
                 d[grepl("_100[01]_", d$id), ])) {
    expect_s3_class(luminex_postqa(m), "luminex_postqa")
    expect_s3_class(luminex_postqa(m, hsp60 = FALSE, r848 = FALSE, il10 = TRUE), "luminex_postqa")
  }
  expect_s3_class(luminex_postqa(luminex_example(il10 = TRUE), il10 = TRUE), "luminex_postqa")
})

test_that("report renders; data is exported only with export = TRUE and never over the master", {
  skip_if_not(rmarkdown::pandoc_available())
  dir <- tempfile("pq"); dir.create(dir)
  master <- file.path(dir, "MHS master.xlsx")
  openxlsx::write.xlsx(list("final master" = d), master, rowNames = FALSE)
  out <- luminex_postqa_report(master, open = FALSE)
  expect_true(file.exists(out[["report"]]))
  expect_false("data" %in% names(out))
  out <- luminex_postqa_report(master, export = TRUE, open = FALSE)
  expect_true(file.exists(out[["data"]]))
  expect_equal(openxlsx::getSheetNames(out[["data"]]), "final master")
  back <- openxlsx::read.xlsx(out[["data"]], sheet = "final master")
  expect_equal(nrow(back), nrow(d))
  expect_true(all(is.na(back[back$id == "MHS_1039_V1_crt_6.5", c("il1b_avg_mfi", "il6_avg_mfi", "tnfa_avg_mfi")])))
  expect_error(luminex_postqa_report(master, export = TRUE, export_file = "MHS master.xlsx", open = FALSE), "overwrite")
  expect_equal(openxlsx::read.xlsx(master, sheet = "final master")$il1b_avg_mfi, d$il1b_avg_mfi)
})

test_that("another US run from the 'for master' sheet is used only when it removes the US problems", {
  fm <- luminex_example(study = "MHS")                       # "for master": original runs
  cyt <- grep("^(il1b|il6|tnfa)_", names(fm), value = TRUE)
  # 1020: a repeat of US read high, and that run was picked into the final master
  rp <- fm[fm$id == "MHS_1020_V1_us", ]; rp$id <- "MHS_1020_V1_us_repeat"
  for (a in c("il1b", "il6", "tnfa")) rp[[paste0(a, "_avg_mfi")]] <- fm[[paste0(a, "_avg_mfi")]][fm$id == "MHS_1020_V1_lps"] * 0.9
  # 1046 (US and LPS swapped): its other US run reads just as high, so it does not help
  r46 <- fm[fm$id == "MHS_1046_V1_us", ]; r46$id <- "MHS_1046_V1_us_repeat"; r46$il6_avg_mfi <- r46$il6_avg_mfi * 1.05
  fm <- rbind(fm, rp, r46)
  fin <- fm[!grepl("_repeat$", fm$id), ]
  fin[fin$id == "MHS_1020_V1_us", cyt] <- rp[, cyt]

  q <- luminex_postqa(fin, repeats_sheet = fm)
  pl <- q$plan
  expect_equal(pl$action[pl$participant == "1020"], "use other us")
  expect_equal(pl$from[pl$participant == "1020"], "MHS_1020_V1_us")
  expect_equal(fm$il1b_avg_mfi[pl$from_row[pl$participant == "1020"] - 1], fm$il1b_avg_mfi[fm$id == "MHS_1020_V1_us"])
  expect_false(any(q$problems_after$participant == "1020"))
  expect_false(any(pl$action[pl$participant == "1046"] == "use other us"))   # does not resolve: default stays
  expect_true(plan_has(pl, "1046", "ignore us"))
  # the exported sheet carries the other run's values
  f <- q$fixed
  expect_equal(f$il6_avg_mfi[f$id == "MHS_1020_V1_us"], fm$il6_avg_mfi[fm$id == "MHS_1020_V1_us"])
  expect_true(grepl("us values taken from 'MHS_1020_V1_us'", f$fix[f$id == "MHS_1020_V1_us"]))
  # without the other runs, the default is to ignore US
  q0 <- luminex_postqa(fin, repeats_sheet = NULL)
  expect_equal(q0$plan$action[q0$plan$participant == "1020"], "ignore us")
  # the team can still overrule it
  q1 <- luminex_postqa(fin, repeats_sheet = fm, ignore_us = "1020")
  expect_equal(q1$plan$action[q1$plan$participant == "1020"], "ignore us")
  # the run already in the final master is not offered as "other"
  q2 <- luminex_postqa(fin, repeats_sheet = rbind(fin, r46))
  expect_false(any(q2$plan$action == "use other us"))
})

test_that("from a file: the 'for master' sheet is read when present, and its absence is noted", {
  dir <- tempfile("pq2"); dir.create(dir)
  f1 <- file.path(dir, "with.xlsx"); f2 <- file.path(dir, "without.xlsx")
  openxlsx::write.xlsx(list("for master" = d, "final master" = d), f1, rowNames = FALSE)
  openxlsx::write.xlsx(list("final master" = d), f2, rowNames = FALSE)
  expect_null(luminex_postqa(f1)$settings$alt_note)
  expect_false(is.null(luminex_postqa(f1)$alt_us))
  expect_match(luminex_postqa(f2)$settings$alt_note, "No 'for master' sheet")
})

test_that("use_other_us: the team can ask for another US run, by participant or by row", {
  fm <- luminex_example(study = "MHS")
  # 1046 (US and LPS swapped) has two other US runs that do not resolve its problems
  r1 <- fm[fm$id == "MHS_1046_V1_us", ]; r1$id <- "MHS_1046_V1_us_repeat"; r1$il6_avg_mfi <- r1$il6_avg_mfi * 1.05
  r2 <- r1; r2$id <- "MHS_1046_V1_us_repeat2"; r2$il6_avg_mfi <- r2$il6_avg_mfi * 0.5
  fm <- rbind(fm, r1, r2)
  fin <- fm[!grepl("_repeat", fm$id), ]
  row_r1 <- which(fm$id == "MHS_1046_V1_us_repeat") + 1
  # not proposed by default (does not resolve)
  expect_false(any(luminex_postqa(fin, repeats_sheet = fm)$plan$action == "use other us"))
  # by participant: the run that leaves the fewest problems
  q <- luminex_postqa(fin, repeats_sheet = fm, use_other_us = "1046")
  pl <- q$plan[q$plan$participant == "1046", ]
  expect_equal(pl$action, "use other us")
  expect_equal(pl$set_by, "you")
  expect_true(pl$from %in% c("MHS_1046_V1_us_repeat", "MHS_1046_V1_us_repeat2"))
  # by row: exactly that run
  q <- luminex_postqa(fin, repeats_sheet = fm, use_other_us = c("1046" = row_r1))
  pl <- q$plan[q$plan$participant == "1046", ]
  expect_equal(pl$from, "MHS_1046_V1_us_repeat")
  expect_equal(pl$from_row, row_r1)
  expect_equal(q$fixed$il6_avg_mfi[q$fixed$id == "MHS_1046_V1_us"], r1$il6_avg_mfi)
  expect_true(any(q$problems_after$participant == "1046"))      # used even though problems remain
  expect_true(plan_has(q$plan, "1045", "ignore us"))             # others keep the default
  # mistakes are explained
  expect_error(luminex_postqa(fin, repeats_sheet = fm, use_other_us = c("1046" = 3)), "is not a us well")
  expect_error(luminex_postqa(fin, repeats_sheet = fm, use_other_us = "1003"), "no other us run")
  expect_error(luminex_postqa(fin, repeats_sheet = NULL, use_other_us = "1046"), "no 'for master' sheet")
})

test_that("stimulated composite follows the scoring script; tagged participants are split into two groups", {
  # composite: scored value z-scored within stimulant and cytokine across everyone, then averaged over cytokines
  s <- qa$scored[qa$scored$condition == "r848", ]
  z <- function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
  s$z <- stats::ave(s$before, s$analyte, FUN = z)
  by_hand <- mean(s$z[s$id_num == "1005"], na.rm = TRUE)
  cp <- qa$composites
  expect_equal(cp$before[cp$id_num == "1005" & cp$condition == "r848"], by_hand)
  expect_setequal(unique(cp$condition), c("lps", "r848", "hsp60"))
  # 1039 (dip at crt_6.5) is only an inhibition-slope problem; 1003 (hsp60 and r848 below US) is in both
  expect_true("1039" %in% qa$tagged_inhibition)
  expect_false("1039" %in% qa$tagged_production)
  expect_true(all(c("1003", "1044") %in% qa$tagged_production))
  # one well below US that is r848 or hsp60 only: production only
  d2 <- d
  k <- d2$id == "MHS_1010_V1_hsp60"
  for (a in c("il1b", "il6", "tnfa")) d2[[paste0(a, "_avg_mfi")]][k] <- d2[[paste0(a, "_avg_mfi")]][d2$id == "MHS_1010_V1_us"] * 0.8
  q <- luminex_postqa(d2)
  expect_true(plan_has(q$plan, "1010", "na", "hsp60"))
  expect_true("1010" %in% q$tagged_production)
  expect_false("1010" %in% q$tagged_inhibition)
  # labels
  expect_true(any(fhrc:::.lpq_issue_lines(qa$problems[qa$problems$participant == "1039", ], qa$settings)$issue == "sus dose-response"))
})
