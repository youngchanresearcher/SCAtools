# Plotting.
#
# Several frontier techniques may be drawn on one chart, because comparing them
# by eye is exactly what a chart is for. The restriction to a single technique
# applies to the computation only: there, the two sides are compared
# numerically, and a comparison between differently measured quantities is not
# interpretable. Here nothing is compared, so every requested technique is
# drawn on both sides and labelled.

.nsca_select_conditions <- function(model, x) {
  available <- .nsca_metadata(model)$conditions
  if (is.null(x)) {
    return(available)
  }
  if (is.numeric(x)) {
    x <- available[x]
  }
  unknown <- setdiff(x, available)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown condition(s): %s.", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  x
}

.nsca_frontier_frame <- function(metadata, model, condition, ceilings) {
  raw <- metadata$raw$conditions[[condition]]
  grid <- seq(raw$x_bounds[[1L]], raw$x_bounds[[2L]],
              length.out = .nsca_grid_size)
  nec_corner <- unname(metadata$necessity_corner[[condition]])
  suf_corner <- unname(metadata$sufficiency_corner[[condition]])

  frames <- list()
  index <- 0L
  for (ceiling in ceilings) {
    sides <- list(
      necessity = .nsca_frontier(
        raw$x, raw$y, nec_corner, ceiling, grid,
        slope = .nsca_nca_param(model$necessity, condition, ceiling, "Slope"),
        intercept = .nsca_nca_param(model$necessity, condition, ceiling, "Intercept"),
        peers = model$necessity$peers[[ceiling]][[condition]]
      ),
      sufficiency = .nsca_frontier(
        raw$x, raw$y, suf_corner, ceiling, grid,
        slope = .nsca_sca_param(model$sufficiency, condition, ceiling, "Slope"),
        intercept = .nsca_sca_param(model$sufficiency, condition, ceiling, "Intercept"),
        peers = model$sufficiency$peers[[ceiling]][[condition]]
      )
    )
    for (side in names(sides)) {
      values <- sides[[side]]
      if (!any(is.finite(values))) {
        next
      }
      index <- index + 1L
      frames[[index]] <- data.frame(
        x = grid, y = values, side = side, ceiling = ceiling,
        series = paste(side, ceiling, sep = " / "),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(frames) == 0L) {
    return(data.frame(x = numeric(), y = numeric(), side = character(),
                      ceiling = character(), series = character()))
  }
  out <- do.call(rbind, frames)
  out[is.finite(out$y), , drop = FALSE]
}

.nsca_plot_one <- function(model, condition, ceilings, shade, annotate) {
  metadata <- .nsca_metadata(model)
  raw <- metadata$raw$conditions[[condition]]
  points <- data.frame(x = raw$x, y = raw$y)
  lines <- .nsca_frontier_frame(metadata, model, condition, ceilings)
  xb <- raw$x_bounds
  yb <- raw$y_bounds

  chart <- ggplot2::ggplot(points, ggplot2::aes(x = x, y = y))

  if (shade && nrow(lines) > 0L) {
    # Shade the admissible region under the first technique: the region
    # neither component rules out. Its area is admissible_region_share.
    first <- lines[lines$ceiling == ceilings[[1L]], , drop = FALSE]
    nec <- first[first$side == "necessity", c("x", "y"), drop = FALSE]
    suf <- first[first$side == "sufficiency", c("x", "y"), drop = FALSE]
    if (nrow(nec) > 0L && nrow(suf) > 0L) {
      upper_is_nec <- identical(
        .nsca_corner_side(unname(metadata$necessity_corner[[condition]])), "upper"
      )
      upper <- if (upper_is_nec) nec else suf
      lower <- if (upper_is_nec) suf else nec
      # Join on the grid rather than assuming the two sides kept the same rows.
      # A frontier that is undefined somewhere loses those rows, and pairing by
      # position would then shift one frontier against the other silently.
      paired <- merge(upper, lower, by = "x", suffixes = c(".up", ".lo"))
      paired <- paired[order(paired$x), , drop = FALSE]
      ribbon <- data.frame(
        x = paired$x,
        ymax = pmin(pmax(paired$y.up, yb[[1L]]), yb[[2L]]),
        ymin = pmin(pmax(paired$y.lo, yb[[1L]]), yb[[2L]])
      )
      ribbon$ymin <- pmin(ribbon$ymin, ribbon$ymax)
      if (nrow(ribbon) > 1L) {
        chart <- chart + ggplot2::geom_ribbon(
          data = ribbon,
          mapping = ggplot2::aes(x = x, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE, fill = "#EAF2FB", colour = NA
        )
      }
    }
  }

  chart <- chart +
    ggplot2::geom_point(shape = 21, colour = "#174A7E", fill = "white",
                        size = 2) +
    ggplot2::geom_vline(xintercept = xb, linetype = "dashed",
                        colour = "grey65") +
    ggplot2::geom_hline(yintercept = yb, linetype = "dashed",
                        colour = "grey65")

  if (nrow(lines) > 0L) {
    chart <- chart + ggplot2::geom_line(
      data = lines,
      mapping = ggplot2::aes(x = x, y = y, colour = side, linetype = ceiling),
      inherit.aes = FALSE, linewidth = 0.8
    )
  }

  # The central-tendency line, when one was requested. It is drawn outside the
  # component legend on purpose: it belongs to neither side, and colouring it
  # as a component would put an average effect and an empty-space frontier in
  # the same key.
  if (isTRUE("ols" %in% metadata$reference)) {
    line <- nsca_reference(model, x = condition)
    if (nrow(line) == 1L && is.finite(line$slope) &&
        is.finite(line$intercept)) {
      chart <- chart + ggplot2::geom_abline(
        intercept = line$intercept, slope = line$slope,
        colour = "grey40", linetype = "dotted", linewidth = 0.7
      )
    }
  }

  if (annotate) {
    nec_corner <- unname(metadata$necessity_corner[[condition]])
    suf_corner <- unname(metadata$sufficiency_corner[[condition]])
    dx <- diff(xb)
    dy <- diff(yb)
    place <- function(corner, label) {
      ggplot2::annotate(
        "label",
        x = if (corner %in% c(1L, 3L)) xb[[1L]] + 0.03 * dx else xb[[2L]] - 0.03 * dx,
        y = if (corner %in% c(1L, 2L)) yb[[2L]] - 0.03 * dy else yb[[1L]] + 0.03 * dy,
        label = label,
        hjust = if (corner %in% c(1L, 3L)) 0 else 1,
        vjust = if (corner %in% c(1L, 2L)) 1 else 0,
        size = 3, fill = "white"
      )
    }
    chart <- chart +
      place(nec_corner, sprintf("empty if necessary\ncorner %d", nec_corner)) +
      place(suf_corner, sprintf("empty if sufficient\ncorner %d", suf_corner))
  }

  chart +
    ggplot2::coord_cartesian(xlim = xb, ylim = yb) +
    ggplot2::labs(
      x = condition,
      y = metadata$outcome,
      title = paste0("NSCA: ", metadata$statement[[condition]]),
      subtitle = sprintf(
        "Direction %s; necessity corner %d, sufficiency corner %d. The shaded band is what neither claim rules out.",
        unname(metadata$direction[[condition]]),
        unname(metadata$necessity_corner[[condition]]),
        unname(metadata$sufficiency_corner[[condition]])
      ),
      colour = "Component",
      linetype = "Frontier",
      caption = if (isTRUE("ols" %in% metadata$reference)) {
        paste0(
          "Dotted line: OLS central tendency, drawn for comparison only. ",
          "Empty-space patterns are empirical; a causal ",
          "necessary-and-sufficient claim needs a design that supports it."
        )
      } else {
        "Empty-space patterns are empirical; a causal necessary-and-sufficient claim needs a design that supports it."
      }
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "grey35", fill = NA),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9)
    )
}

#' Plot both frontiers
#'
#' Draws the scatter plot with the necessity and sufficiency frontiers and
#' shades the admissible region between them. Several frontier techniques may
#' be drawn at once; unlike the computation, a chart compares nothing
#' numerically, so mixing techniques here is safe and often informative.
#'
#' @param model An object returned by [nsca_analysis()].
#' @param x Optional condition names or positions.
#' @param ceilings Frontier techniques to draw. Defaults to all that were
#'   estimated.
#' @param shade Shade the admissible region under the first technique.
#' @param annotate Label the two corners that the claim requires to be empty.
#' @param ... Passed on to `nsca_plot()` by the `plot()` method.
#' @return A `ggplot` for one condition, or a named list of plots.
#' @seealso [nsca_analysis()], [nsca_table()]
#' @export
#' @examples
#' set.seed(1)
#' x <- sort(runif(60))
#' dat <- data.frame(X = x, Y = pmin(pmax(x + rnorm(60, 0, 0.1), 0), 1))
#' fit <- nsca_analysis(dat, "X", "Y", ceilings = "ce_fdh")
#' nsca_plot(fit)
#' plot(fit)
nsca_plot <- function(model, x = NULL, ceilings = NULL, shade = TRUE,
                      annotate = TRUE) {
  metadata <- .nsca_metadata(model)
  conditions <- .nsca_select_conditions(model, x)
  if (is.null(ceilings)) {
    ceilings <- metadata$ceilings
  }
  unknown <- setdiff(ceilings, metadata$ceilings)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "Frontier(s) not estimated: %s. Available: %s.",
        paste(unknown, collapse = ", "), paste(metadata$ceilings, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  plots <- lapply(
    conditions,
    function(condition) .nsca_plot_one(model, condition, ceilings, shade, annotate)
  )
  names(plots) <- conditions
  if (length(plots) == 1L) plots[[1L]] else plots
}

#' @rdname nsca_plot
#' @export
plot.nsca_result <- function(x, ...) {
  plots <- nsca_plot(x, ...)
  if (inherits(plots, "ggplot")) {
    print(plots)
  } else {
    invisible(lapply(plots, print))
  }
  invisible(plots)
}
