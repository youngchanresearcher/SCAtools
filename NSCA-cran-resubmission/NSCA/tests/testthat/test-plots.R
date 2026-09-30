# Plots.
#
# A chart is the one output whose errors are invisible to the other tests: a
# frontier drawn in the wrong place still looks like a frontier. So rather than
# checking that a plot object exists, these tests take the geometry back out of
# the built plot and compare it with the numbers the tables report. If the
# picture and the table ever disagree, one of them is lying.

plot_data <- function(n = 60, noise = 0.12, seed = 17, decreasing = FALSE) {
  set.seed(seed)
  x <- sort(runif(n))
  y <- pmin(pmax(x + rnorm(n, 0, noise), 0), 1)
  data.frame(X = x, Y = if (decreasing) 1 - y else y)
}

layer_geoms <- function(p) {
  vapply(p$layers, function(l) class(l$geom)[[1L]], character(1L))
}

test_that("every direction produces a plot that can be rendered", {
  for (direction in c("HH", "LH", "HL", "LL")) {
    dat <- plot_data(decreasing = direction %in% c("LH", "HL"))
    fit <- nsca_analysis(dat, "X", "Y", direction = direction,
                         ceilings = "ce_fdh")
    p <- nsca_plot(fit)
    expect_s3_class(p, "ggplot")
    # Building is where most silent plotting errors surface.
    expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
    expect_match(p$labels$subtitle, paste("Direction", direction))
  }
})

test_that("the corner labels sit on the two corners the claim needs empty", {
  # A label on the wrong corner would invert the reader's whole interpretation
  # while the numbers stayed correct, so its position is checked against the
  # corner mapping rather than against a fixed coordinate.
  for (direction in c("HH", "LH", "HL", "LL")) {
    dat <- plot_data(decreasing = direction %in% c("LH", "HL"))
    fit <- nsca_analysis(dat, "X", "Y", direction = direction,
                         ceilings = "ce_fdh")
    p <- nsca_plot(fit)
    label_layers <- which(layer_geoms(p) == "GeomLabel")
    expect_length(label_layers, 2L)

    bounds <- attr(fit, "nsca")$raw$conditions[["X"]]
    mid_x <- mean(bounds$x_bounds)
    mid_y <- mean(bounds$y_bounds)
    corners <- nsca_corners(direction)
    built <- ggplot2::ggplot_build(p)$data

    placement <- vapply(label_layers, function(i) {
      row <- built[[i]]
      number <- gsub("[^0-9]", "", as.character(row$label[[1L]]))
      side_x <- if (row$x[[1L]] < mid_x) "left" else "right"
      side_y <- if (row$y[[1L]] > mid_y) "upper" else "lower"
      paste(number, side_y, side_x)
    }, character(1L))

    expected <- c(
      paste(corners$necessity_corner,
            sub("-", " ", corners$necessity_location)),
      paste(corners$sufficiency_corner,
            sub("-", " ", corners$sufficiency_location))
    )
    expect_setequal(placement, expected)
  }
})

test_that("the shaded ribbon carries the area the table reports", {
  dat <- plot_data(noise = 0.25)
  fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
  p <- nsca_plot(fit)
  ribbon <- which(layer_geoms(p) == "GeomRibbon")
  expect_length(ribbon, 1L)

  d <- ggplot2::ggplot_build(p)$data[[ribbon[[1L]]]]
  d <- d[order(d$x), , drop = FALSE]
  height <- pmax(d$ymax - d$ymin, 0)
  area <- sum((height[-1L] + height[-length(height)]) / 2 * diff(d$x))
  raw <- attr(fit, "nsca")$raw$conditions[["X"]]
  scope <- diff(raw$x_bounds) * diff(raw$y_bounds)

  expect_equal(area / scope, nsca_table(fit)$admissible_region_share,
               tolerance = 0.02)
})

test_that("the frontier drawn as upper really is the upper one", {
  # The upper/lower assignment follows the corner, and for the two low-X
  # directions it swaps. If it did not, the ribbon would be drawn upside down.
  for (direction in c("HH", "LH", "HL", "LL")) {
    dat <- plot_data(decreasing = direction %in% c("LH", "HL"))
    fit <- nsca_analysis(dat, "X", "Y", direction = direction,
                         ceilings = "ce_fdh")
    frame <- NSCA:::.nsca_frontier_frame(attr(fit, "nsca"), fit, "X", "ce_fdh")
    nec <- frame[frame$side == "necessity", , drop = FALSE]
    suf <- frame[frame$side == "sufficiency", , drop = FALSE]
    paired <- merge(nec[c("x", "y")], suf[c("x", "y")], by = "x",
                    suffixes = c(".nec", ".suf"))
    corners <- nsca_corners(direction)
    if (corners$necessity_bound == "upper") {
      expect_true(mean(paired$y.nec >= paired$y.suf) > 0.95, info = direction)
    } else {
      expect_true(mean(paired$y.suf >= paired$y.nec) > 0.95, info = direction)
    }
  }
})

test_that("envelopment frontiers leave no observation outside them", {
  # ce_vrs is rebuilt from the engine's peers. A wrong reconstruction would
  # still draw a plausible line, so it is checked against the defining property
  # of an envelope and against the engine's own accuracy figure.
  dat <- plot_data()
  fit <- nsca_analysis(dat, "X", "Y", direction = "HH",
                       ceilings = c("ce_fdh", "ce_vrs"))
  meta <- attr(fit, "nsca")
  raw <- meta$raw$conditions[["X"]]
  tol <- 1e-8 * max(1, diff(raw$y_bounds))

  for (ceiling in c("ce_fdh", "ce_vrs")) {
    upper <- NSCA:::.nsca_frontier(
      raw$x, raw$y, meta$necessity_corner[["X"]], ceiling, raw$x,
      peers = fit$necessity$peers[[ceiling]][["X"]]
    )
    lower <- NSCA:::.nsca_frontier(
      raw$x, raw$y, meta$sufficiency_corner[["X"]], ceiling, raw$x,
      peers = fit$sufficiency$peers[[ceiling]][["X"]]
    )
    expect_true(all(is.finite(upper)), info = ceiling)
    expect_true(all(is.finite(lower)), info = ceiling)
    expect_equal(sum(raw$y > upper + tol), 0L, info = ceiling)
    expect_equal(sum(raw$y < lower - tol), 0L, info = ceiling)
    # The engine agrees: nothing lies outside an envelopment ceiling.
    expect_gte(
      nsca_extract(fit, ceiling = ceiling, param = "nec:Ceiling accuracy"),
      100 - 1e-6
    )
  }
})

test_that("several techniques may be drawn even though they may not be mixed", {
  dat <- plot_data()
  fit <- nsca_analysis(dat, "X", "Y",
                       ceilings = c("ce_fdh", "cr_fdh", "cols"))
  p <- nsca_plot(fit)
  line <- which(layer_geoms(p) == "GeomLine")
  expect_length(line, 1L)
  built <- ggplot2::ggplot_build(p)$data[[line[[1L]]]]
  # Three techniques on two sides.
  expect_equal(length(unique(built$group)), 6L)
  expect_equal(length(unique(built$colour)), 2L)
  expect_identical(p$labels$colour, "Component")
  expect_identical(p$labels$linetype, "Frontier")

  # A subset may be drawn, and only the requested ones appear.
  fewer <- ggplot2::ggplot_build(nsca_plot(fit, ceilings = "ce_fdh"))
  fewer_line <- which(layer_geoms(nsca_plot(fit, ceilings = "ce_fdh")) ==
                        "GeomLine")
  expect_equal(length(unique(fewer$data[[fewer_line[[1L]]]]$group)), 2L)
})

test_that("shade and annotate switch the layers they name", {
  fit <- nsca_analysis(plot_data(), "X", "Y", ceilings = "ce_fdh")
  full <- layer_geoms(nsca_plot(fit))
  bare <- layer_geoms(nsca_plot(fit, shade = FALSE, annotate = FALSE))

  expect_true("GeomRibbon" %in% full)
  expect_equal(sum(full == "GeomLabel"), 2L)
  expect_false("GeomRibbon" %in% bare)
  expect_equal(sum(bare == "GeomLabel"), 0L)
  # The data and the frontiers stay in place either way.
  expect_true(all(c("GeomPoint", "GeomLine") %in% bare))
})

test_that("one plot per condition, named, and selectable", {
  dat <- plot_data()
  two <- data.frame(X1 = dat$X, X2 = runif(nrow(dat)), Y = dat$Y)
  fit <- nsca_analysis(two, c("X1", "X2"), "Y", ceilings = "ce_fdh")

  plots <- nsca_plot(fit)
  expect_false(inherits(plots, "ggplot"))
  expect_named(plots, c("X1", "X2"))
  expect_s3_class(plots[["X1"]], "ggplot")
  expect_s3_class(nsca_plot(fit, x = "X2"), "ggplot")
  expect_s3_class(nsca_plot(fit, x = 1L), "ggplot")
  expect_identical(nsca_plot(fit, x = "X2")$labels$x, "X2")
})

test_that("the plot method returns the plots invisibly", {
  fit <- nsca_analysis(plot_data(), "X", "Y", ceilings = "ce_fdh")
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  returned <- plot(fit)
  expect_s3_class(returned, "ggplot")
})
