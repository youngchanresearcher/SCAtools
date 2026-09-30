# The three result layers.
#
# nsca_results() presents necessity, sufficiency and their conjunction as three
# tables. The risk this file guards against is the third table being read as a
# third, independent experiment. It is not: p_nsca_iut is max(p_nec, p_suf) and can
# only pass when both components pass, so it carries no evidence the components
# did not already carry. The tests below pin that arithmetic relationship, and
# check that the two component tables really are the engines' own output rather
# than a reconstruction that could drift from them.

three_layer_data <- function(n = 50, noise = 0.10, seed = 21) {
  set.seed(seed)
  x <- sort(runif(n))
  data.frame(X = x, Y = pmin(pmax(x + rnorm(n, 0, noise), 0), 1))
}

test_that("the necessity layer is the engine's own necessity analysis", {
  dat <- three_layer_data()
  fit <- nsca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh")
  nec <- nsca_results(fit)$necessity
  direct <- NCA::nca_analysis(dat, "X", "Y", corner = 1, ceilings = "ce_fdh")

  for (param in c("Effect size", "Ceiling zone", "Ceiling accuracy", "Fit",
                  "Slope", "Intercept")) {
    column <- c("Effect size" = "effect_size", "Ceiling zone" = "empty_zone_area",
                "Ceiling accuracy" = "frontier_accuracy", "Fit" = "fit",
                "Slope" = "slope", "Intercept" = "intercept")[[param]]
    expect_equal(
      nec[[column]],
      as.numeric(NCA::nca_extract(direct, "X", "ce_fdh", param)),
      info = param
    )
  }
  expect_identical(nec$analysis, "necessity")
  expect_identical(nec$empty_corner, 1L)
  expect_match(nec$statement, "necessary for")
  expect_false(grepl("sufficient", nec$statement))
})

test_that("the sufficiency layer is SCAtools' own table", {
  dat <- three_layer_data()
  fit <- nsca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh")
  suf <- nsca_results(fit)$sufficiency
  direct <- SCAtools::sca_table(
    SCAtools::sca_analysis(dat, "X", "Y", direction = "HH", ceilings = "ce_fdh")
  )
  expect_equal(suf$effect_size, direct$effect_size)
  expect_equal(suf$empty_zone_area, direct$empty_zone_area)
  expect_equal(suf$fit, direct$fit)
  expect_identical(suf$analysis, "sufficiency")
  expect_identical(suf$empty_corner, 4L)
  expect_match(suf$statement, "sufficient for")
})

test_that("the two component layers share a column layout", {
  dat <- three_layer_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = c("ce_fdh", "cr_fdh"))
  tables <- nsca_results(fit)
  expect_named(tables,
               c("necessity", "sufficiency", "necessary_and_sufficient"))
  expect_identical(names(tables$necessity), names(tables$sufficiency))
  expect_identical(nrow(tables$necessity), nrow(tables$sufficiency))
  # One row per condition and frontier, on every layer.
  expect_equal(nrow(tables$necessary_and_sufficient), 2L)
  expect_setequal(tables$necessity$frontier, c("ce_fdh", "cr_fdh"))
})

test_that("the joint layer adds no evidence of its own", {
  # p_nsca_iut is a conjunction, not a third experiment. It must equal the larger
  # of the two component p-values exactly, and can never be the smaller.
  dat <- three_layer_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = c("ce_fdh", "cr_fdh"),
                       test.rep = 200)
  joint <- nsca_table(fit)

  expect_equal(joint$p_nsca_iut, pmax(joint$p_nec, joint$p_suf))
  expect_true(all(joint$p_nsca_iut >= joint$p_nec - 1e-12))
  expect_true(all(joint$p_nsca_iut >= joint$p_suf - 1e-12))
  expect_equal(joint$weakest_effect, pmin(joint$d_nec, joint$d_suf))
  expect_true(all(joint$weakest_effect <= joint$d_nec + 1e-12))
  expect_true(all(joint$weakest_effect <= joint$d_suf + 1e-12))
  # The second joint index is a different aggregation of the same two numbers,
  # so it too adds no evidence: it is bounded by the components' own sum.
  expect_equal(joint$balanced_joint_effect, 2 * sqrt(joint$d_nec * joint$d_suf))
  expect_true(all(joint$balanced_joint_effect <= joint$d_nec + joint$d_suf + 1e-12))
})

test_that("the significance flags agree with the p-values and with each other", {
  dat <- three_layer_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", test.rep = 200)
  joint <- nsca_table(fit)
  alpha <- 0.05

  expect_identical(joint$necessity_significant, joint$p_nec <= alpha)
  expect_identical(joint$sufficiency_significant, joint$p_suf <= alpha)
  expect_identical(
    joint$nsca_significant,
    joint$necessity_significant & joint$sufficiency_significant
  )
  # The joint-support verdict and the flag cannot disagree.
  expect_identical(joint$nsca_significant, joint$joint_support_status == "supported")
})

test_that("the alpha used by the flags is the one that was requested", {
  dat <- three_layer_data(noise = 0.35)
  strict <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                                     test.rep = 200, test.p_threshold = 0.001))
  lenient <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                                      test.rep = 200, test.p_threshold = 0.5))
  expect_identical(strict$nsca_significant,
                   strict$p_nec <= 0.001 && strict$p_suf <= 0.001)
  expect_identical(lenient$nsca_significant,
                   lenient$p_nec <= 0.5 && lenient$p_suf <= 0.5)
})

test_that("without a permutation test nothing claims significance", {
  dat <- three_layer_data()
  joint <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh"))
  expect_true(is.na(joint$p_nsca_iut))
  expect_true(is.na(joint$necessity_significant))
  expect_true(is.na(joint$sufficiency_significant))
  expect_true(is.na(joint$nsca_significant))
  expect_identical(joint$joint_support_status, "not tested")
})

test_that("the verdict names which component failed", {
  # A relation with a populated upper-left corner has little necessity while
  # sufficiency survives; the verdict must say so rather than only "no".
  set.seed(3)
  n <- 60
  x <- sort(runif(n))
  lopsided <- data.frame(X = x, Y = pmin(x + runif(n, 0, 0.7), 1))
  joint <- nsca_table(nsca_analysis(lopsided, "X", "Y", ceilings = "ce_fdh",
                                    test.rep = 200))
  expect_true(joint$joint_support_status %in%
    c("supported", "necessity fails", "sufficiency fails", "neither component"))
  if (joint$joint_support_status == "necessity fails") {
    expect_gt(joint$p_nec, 0.05)
    expect_lte(joint$p_suf, 0.05)
  }
  if (joint$joint_support_status == "sufficiency fails") {
    expect_lte(joint$p_nec, 0.05)
    expect_gt(joint$p_suf, 0.05)
  }
})

test_that("printing shows all three layers and the inference caveat", {
  dat <- three_layer_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh", test.rep = 100)
  printed <- paste(capture.output(print(fit)), collapse = "\n")

  expect_match(printed, "1\\. Necessary Condition Analysis")
  expect_match(printed, "2\\. Sufficiency Condition Analysis")
  expect_match(printed, "3\\. Necessary and Sufficient Condition Analysis")
  # The caveat is the point of the layer separation and must not be dropped.
  expect_match(printed, "random-pairing")
  expect_match(printed, "max\\(p_nec, p_suf\\)")

  # All three joint summaries are shown together, with what each measures and
  # the statement that no benchmarks exist. Printing one of them alone would
  # reintroduce exactly the framing this version removed.
  expect_match(printed, "min\\(d_nec, d_suf\\)")
  expect_match(printed, "2\\*sqrt\\(d_nec\\*d_suf\\)")
  expect_match(printed, "1 - admissible_region_share")
  expect_match(printed, "non-compensatory")
  expect_match(printed, "partially compensatory")
  expect_match(printed, "benchmarks")

  summarised <- paste(capture.output(print(summary(fit))), collapse = "\n")
  expect_match(summarised,
               "not a direct test of a causal necessary-and-sufficient relation")
  expect_match(summarised, "magnitude benchmarks")
})

test_that("summary carries the three layers as data, not only as text", {
  dat <- three_layer_data()
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
  s <- summary(fit)
  expect_s3_class(s, "summary_nsca_result")
  for (layer in c("necessity", "sufficiency", "necessary_and_sufficient")) {
    expect_s3_class(s[[layer]], "data.frame")
  }
  expect_equal(s$results, s$necessary_and_sufficient)
})

test_that("equivalence classes group the directions that mirror each other", {
  dat <- three_layer_data()
  classes <- vapply(
    c("HH", "LH", "HL", "LL"),
    function(dir) {
      yy <- if (dir %in% c("HH", "LL")) dat$Y else 1 - dat$Y
      nsca_table(nsca_analysis(data.frame(X = dat$X, Y = yy), "X", "Y",
                               direction = dir, ceilings = "ce_fdh"))$equivalence_class
    },
    character(1L)
  )
  expect_identical(unname(classes), c("HH/LL", "LH/HL", "LH/HL", "HH/LL"))
  expect_identical(nsca_direction_map()$equivalence_class,
                   c("HH/LL", "LH/HL", "LH/HL", "HH/LL"))
})
