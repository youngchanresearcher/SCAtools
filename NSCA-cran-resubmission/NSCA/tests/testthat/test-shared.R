# The shared permutation sequence.
#
# The reason this exists is narrow and worth stating precisely: an
# intersection-union rule combines two p-values and needs no assumption about
# how they co-vary, but min(d_nec, d_suf) is a statistic OF both components,
# and its null distribution depends on exactly that co-variation. Two separate
# permutation runs cannot supply it. These tests check that one sequence really
# does drive both sides, that the p-values it produces are proper permutation
# p-values, and that it leaves the caller's random stream alone.

shared_data <- function(n = 40, noise = 0.08, seed = 6) {
  set.seed(seed)
  x <- sort(runif(n))
  data.frame(X = x, Y = pmin(pmax(x + rnorm(n, 0, noise), 0), 1))
}

test_that("without the shared sequence there is no weakest-effect p-value", {
  engine <- nsca_table(nsca_analysis(shared_data(), "X", "Y",
                                     ceilings = "ce_fdh", test.rep = 100))
  expect_identical(engine$p_source, "engine")
  expect_true(is.na(engine$p_weakest_perm))
  expect_true(is.finite(engine$p_nsca_iut))
})

test_that("the shared sequence drives both engines and reports all four", {
  fit <- nsca_analysis(shared_data(), "X", "Y", ceilings = "ce_fdh",
                       shared.test.rep = 39, shared.seed = 1)
  row <- nsca_table(fit)

  expect_identical(row$p_source, "shared")
  for (column in c("p_nec", "p_suf", "p_nsca_iut", "p_weakest_perm")) {
    expect_true(is.finite(row[[column]]), info = column)
    expect_gt(row[[column]], 0)
    expect_lte(row[[column]], 1)
  }
  # Every p-value is a count over B + 1, so it must be a multiple of 1/40.
  for (column in c("p_nec", "p_suf", "p_weakest_perm")) {
    expect_equal(row[[column]] * 40, round(row[[column]] * 40),
                 tolerance = 1e-9, info = column)
    # The Phipson-Smyth correction means no p-value can be zero.
    expect_gte(row[[column]], 1 / 40)
  }
  expect_equal(row$p_nsca_iut, max(row$p_nec, row$p_suf))

  # The stored draws are the evidence behind those numbers.
  draws <- attr(fit, "nsca")$shared
  expect_named(draws, c("necessity", "sufficiency", "weakest"))
  expect_equal(dim(draws$necessity), c(39L, 1L, 1L))
  expect_equal(draws$weakest[, 1L, 1L],
               pmin(draws$necessity[, 1L, 1L], draws$sufficiency[, 1L, 1L]))
})

test_that("a strong relation beats random pairing on the minimum", {
  # A tight diagonal has both empty zones large at once, which random pairing
  # essentially never produces. If this ever failed, the null draws would not
  # be coming from a genuine shuffle.
  fit <- nsca_analysis(shared_data(noise = 0.05), "X", "Y",
                       ceilings = "ce_fdh", shared.test.rep = 39,
                       shared.seed = 2)
  row <- nsca_table(fit)
  draws <- attr(fit, "nsca")$shared$weakest[, 1L, 1L]

  expect_gt(row$weakest_effect, 0.25)
  expect_lt(stats::median(draws, na.rm = TRUE), row$weakest_effect)
  expect_equal(row$p_weakest_perm, 1 / 40, tolerance = 1e-9)
})

test_that("a recorded draw is exactly what that one shuffle produces", {
  # The strongest available check that the sequence is shared: reproduce the
  # first shuffle from the seed, fit both engines on it directly, and require
  # the stored draws to match. If the two sides were shuffled separately, at
  # most one of these two could agree.
  dat <- shared_data(n = 30)
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                       shared.test.rep = 3, shared.seed = 21)
  draws <- attr(fit, "nsca")$shared

  set.seed(21)
  shuffled <- dat
  shuffled$Y <- dat$Y[sample.int(nrow(dat))]
  nec <- NCA::nca_analysis(
    data = shuffled, x = "X", y = "Y", ceilings = "ce_fdh", corner = 1,
    scope = NULL, bottleneck.x = "actual", bottleneck.y = "actual",
    cutoff = 0, qr.tau = 0.95, effect_aggregation = 1, test.rep = 0
  )
  suf <- SCAtools::sca_analysis(
    data = shuffled, x = "X", y = "Y", direction = "HH", ceilings = "ce_fdh",
    scope = NULL, cutoff = 0, qr.tau = 0.95, test.rep = 0
  )
  expect_equal(
    draws$necessity[1L, 1L, 1L],
    as.numeric(NCA::nca_extract(nec, "X", "ce_fdh", "Effect size"))
  )
  expect_equal(
    draws$sufficiency[1L, 1L, 1L],
    as.numeric(SCAtools::sca_extract(suf, "X", "ce_fdh", "Effect size"))
  )
  expect_equal(draws$weakest[1L, 1L, 1L],
               min(draws$necessity[1L, 1L, 1L], draws$sufficiency[1L, 1L, 1L]))
})

test_that("the seed reproduces the sequence and the caller's stream survives", {
  dat <- shared_data()
  first <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                                    shared.test.rep = 15, shared.seed = 7))
  second <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                                     shared.test.rep = 15, shared.seed = 7))
  expect_equal(first$p_weakest_perm, second$p_weakest_perm)
  expect_equal(first$p_nec, second$p_nec)

  # Different seeds must actually give a different sequence, or the seed
  # argument is decorative.
  third <- nsca_table(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                                    shared.test.rep = 15, shared.seed = 8))
  draws7 <- attr(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                               shared.test.rep = 15, shared.seed = 7),
                 "nsca")$shared$weakest[, 1L, 1L]
  draws8 <- attr(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                               shared.test.rep = 15, shared.seed = 8),
                 "nsca")$shared$weakest[, 1L, 1L]
  expect_false(isTRUE(all.equal(draws7, draws8)))
  expect_true(is.finite(third$p_weakest_perm))
})

test_that("the seeded block restores the caller's random stream", {
  # Tested at the unit level rather than end to end, because the engines are
  # free to draw random numbers of their own and no end-to-end assertion could
  # separate that from a reseeding bug here.
  set.seed(99)
  expected <- runif(3)
  set.seed(99)
  invisible(NSCA:::.nsca_with_seed(7, runif(50)))
  expect_equal(runif(3), expected)

  # With no seed the block is a pass-through and consumes the caller's stream
  # in the ordinary way.
  set.seed(99)
  drawn <- NSCA:::.nsca_with_seed(NULL, runif(3))
  set.seed(99)
  expect_equal(drawn, runif(3))

  # The seed really does fix what the block draws.
  a <- NSCA:::.nsca_with_seed(5, runif(4))
  b <- NSCA:::.nsca_with_seed(5, runif(4))
  expect_equal(a, b)
})

test_that("the shared sequence is validated and stays optional", {
  dat <- shared_data()
  expect_error(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                             shared.test.rep = -1), "non-negative")
  expect_error(nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                             shared.test.rep = "many"), "non-negative")
  zero <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                        shared.test.rep = 0)
  expect_null(attr(zero, "nsca")$shared)
  expect_identical(nsca_table(zero)$p_source, "none")
})

test_that("the shared sequence covers every condition and frontier", {
  dat <- shared_data()
  two <- data.frame(X1 = dat$X, X2 = runif(nrow(dat)), Y = dat$Y)
  fit <- nsca_analysis(two, c("X1", "X2"), "Y",
                       ceilings = c("ce_fdh", "cr_fdh"),
                       shared.test.rep = 12, shared.seed = 11)
  draws <- attr(fit, "nsca")$shared
  expect_equal(dim(draws$weakest), c(12L, 2L, 2L))
  expect_identical(dimnames(draws$weakest)[[2L]], c("X1", "X2"))
  expect_identical(dimnames(draws$weakest)[[3L]], c("ce_fdh", "cr_fdh"))

  table <- nsca_table(fit)
  expect_equal(nrow(table), 4L)
  expect_true(all(table$p_source == "shared"))
  expect_true(all(is.finite(table$p_weakest_perm)))
  # The unrelated condition should not look necessary and sufficient. Comparing
  # the effect sizes rather than the p-values keeps this deterministic: with
  # only 12 replications the smallest reachable p-value is 1/13, which an
  # unrelated condition can reach by chance often enough to make a p-value
  # comparison flaky.
  weakest <- function(name) {
    table$weakest_effect[table$condition == name & table$ceiling == "ce_fdh"]
  }
  expect_gt(weakest("X1"), weakest("X2"))
  expect_lt(weakest("X2"), 0.15)
})

test_that("a seed leaves no random stream behind when there was none", {
  # Built first: shared_data() sets a seed of its own.
  dat <- shared_data()
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) {
    saved <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", saved, envir = globalenv()), add = TRUE)
    rm(".Random.seed", envir = globalenv())
  }
  nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh",
                shared.test.rep = 3, shared.seed = 1)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})
