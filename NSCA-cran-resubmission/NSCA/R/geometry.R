# Geometry shared by the two frontiers.
#
# The admissible region is computed directly, by integrating the vertical gap
# the two fitted frontiers, rather than algebraically as 1 - d_nec - d_suf + O.
# The two routes are equal in theory: the empty spaces and the admissible region tile
# the scope, so inclusion and exclusion give the identity exactly. They are not
# equal in practice, because the algebraic route uses the effect sizes the
# engines computed from their own internal frontiers while the geometric route
# uses the frontiers this package reconstructs.
#
# That is precisely why the geometric route is the one used. The difference
# between the two is reported as reconstruction_error, and it is the only
# available check that the reconstruction still matches what the engine
# actually fitted. A wrongly reconstructed frontier still looks like a
# frontier on a plot and still produces plausible areas; only the identity
# refuses to close.

.nsca_grid_size <- 2001L

.nsca_numeric_column <- function(column) {
  suppressWarnings(as.numeric(as.character(column)))
}

# The scope the engine will use: the observed range, widened by any theoretical
# scope supplied by the caller.
.nsca_scope_pair <- function(scope, index, offset) {
  if (is.null(scope)) {
    return(c(NA_real_, NA_real_))
  }
  segment <- if (is.list(scope)) {
    if (index <= length(scope)) scope[[index]] else scope[[1L]]
  } else {
    values <- as.numeric(scope)
    if (length(values) == 4L) {
      values
    } else if (length(values) >= index * 4L) {
      values[((index - 1L) * 4L + 1L):(index * 4L)]
    } else {
      return(c(NA_real_, NA_real_))
    }
  }
  segment <- as.numeric(segment)
  if (length(segment) < offset + 2L) {
    return(c(NA_real_, NA_real_))
  }
  segment[c(offset + 1L, offset + 2L)]
}

.nsca_bounds <- function(observed, scope_pair) {
  observed <- observed[is.finite(observed)]
  if (length(observed) == 0L) {
    stop("A variable has no usable observations.", call. = FALSE)
  }
  c(
    min(c(observed, scope_pair), na.rm = TRUE),
    max(c(observed, scope_pair), na.rm = TRUE)
  )
}

# How much of the declared scope the observations actually span, per axis.
#
# Every NSCA quantity is an area measured against the scope, so as this falls
# towards zero the numbers stop describing the data and start describing the
# scope. The limiting case is instructive rather than exotic: a constant
# outcome placed at the centre of an imposed unit scope leaves the whole upper
# half empty and the whole lower half empty, so d_nec = d_suf = 0.5, the band
# closes, and every joint index reports perfect joint support for data that
# carries no information whatsoever. Moving the constant to 0.8 changes the
# answer again, which is the tell: nothing about the data moved.
#
# Two different pictures are called a "horizontal line" and they land in
# opposite places. A flat *frontier* over a filled scope means no empty zone at
# all and every index is zero, correctly. A flat *data cloud* means the
# indices are at their maximum, wrongly. Only the second is flagged here.
.nsca_degenerate_tolerance <- 1e-8
.nsca_thin_coverage <- 0.05

.nsca_axis_coverage <- function(values, bounds) {
  span <- suppressWarnings(diff(as.numeric(bounds)))
  values <- values[is.finite(values)]
  if (length(span) != 1L || !is.finite(span) || span <= 0 ||
      length(values) == 0L) {
    return(NA_real_)
  }
  diff(range(values)) / span
}

# Complete pairs only, matching what the engine fits on.
#
# The engines fit each condition on its own complete (x, y) pairs, so each
# condition's outcome bounds are taken from those pairs and from its own scope
# segment. Taking them from every observed outcome instead would widen the
# scope whenever a row has an outcome but no condition value, and the
# admissible region would then be measured against a different box from the
# one d_nec and d_suf were: the area identity fails to close, and
# geometry_acceptable, and with it joint_support, turns FALSE for no reason in
# the data. The top-level y_bounds stays the overall range, which is what the
# constant-outcome check needs.
.nsca_raw_data <- function(data, x_names, y_name, scope) {
  frame <- as.data.frame(data)
  y_raw <- .nsca_numeric_column(frame[[y_name]])
  y_bounds <- .nsca_bounds(y_raw, .nsca_scope_pair(scope, 1L, 2L))

  conditions <- vector("list", length(x_names))
  names(conditions) <- x_names
  for (i in seq_along(x_names)) {
    x_raw <- .nsca_numeric_column(frame[[x_names[[i]]]])
    keep <- is.finite(x_raw) & is.finite(y_raw)
    conditions[[i]] <- list(
      x = x_raw[keep],
      y = y_raw[keep],
      x_bounds = .nsca_bounds(x_raw[keep], .nsca_scope_pair(scope, i, 0L)),
      y_bounds = .nsca_bounds(y_raw[keep], .nsca_scope_pair(scope, i, 2L))
    )
  }
  list(outcome = y_raw[is.finite(y_raw)], y_bounds = y_bounds,
       conditions = conditions)
}

# The exact free-disposal-hull frontier for one empty corner, evaluated on a
# grid. Which running extreme is taken depends only on which corner is empty.
.nsca_step_frontier <- function(x, y, corner, grid) {
  vapply(
    grid,
    function(t) {
      subset <- if (corner %in% c(1L, 3L)) y[x <= t] else y[x >= t]
      if (length(subset) == 0L) {
        return(NA_real_)
      }
      if (corner %in% c(1L, 2L)) max(subset) else min(subset)
    },
    numeric(1L)
  )
}

# Piecewise-linear frontier through the peers, for ce_vrs.
.nsca_peer_frontier <- function(peers, grid) {
  if (is.null(peers) || !is.matrix(peers) || nrow(peers) < 1L) {
    return(rep(NA_real_, length(grid)))
  }
  peers <- peers[stats::complete.cases(peers), , drop = FALSE]
  if (nrow(peers) < 1L) {
    return(rep(NA_real_, length(grid)))
  }
  if (nrow(peers) == 1L) {
    return(rep(peers[1L, 2L], length(grid)))
  }
  ordered <- peers[order(peers[, 1L]), , drop = FALSE]
  keep <- !duplicated(ordered[, 1L])
  stats::approx(
    ordered[keep, 1L], ordered[keep, 2L], xout = grid, rule = 2
  )$y
}

#' Frontier values on a grid
#'
#' @param x,y Condition and outcome values for one condition.
#' @param corner The empty corner this frontier bounds.
#' @param ceiling Frontier technique.
#' @param grid Condition values at which to evaluate.
#' @param slope,intercept Line coefficients, for regression-type frontiers.
#' @param peers Peer matrix, used by `ce_vrs`.
#' @return A numeric vector of frontier values, or `NA` where undefined.
#' @noRd
.nsca_frontier <- function(x, y, corner, ceiling, grid,
                           slope = NA_real_, intercept = NA_real_,
                           peers = NULL) {
  if (identical(ceiling, "ce_fdh")) {
    return(.nsca_step_frontier(x, y, corner, grid))
  }
  if (identical(ceiling, "ce_vrs")) {
    return(.nsca_peer_frontier(peers, grid))
  }
  if (is.finite(slope) && is.finite(intercept)) {
    return(intercept + slope * grid)
  }
  rep(NA_real_, length(grid))
}

# A step frontier overlaps itself by construction. Between two consecutive
# observations the upper staircase holds the lower observation's value while
# the lower staircase already holds the upper one, so the two empty zones
# overlap in every tread. The overlap is about one step, of order 1/n, and is
# discretization rather than a modelling failure. Only smooth frontiers can
# cross in a way that says something is wrong.
.nsca_step_ceilings <- "ce_fdh"

#' Undetermined band and overlap between two frontiers
#'
#' @param upper,lower Frontier values on a common grid.
#' @param grid The grid.
#' @param y_bounds Outcome axis bounds, used to clip to the scope.
#' @return A list with the band area, the overlap area, and how far the
#'   frontiers were inverted at worst.
#' @noRd
.nsca_band <- function(upper, lower, grid, y_bounds) {
  usable <- is.finite(upper) & is.finite(lower)
  if (!any(usable)) {
    return(list(
      area = NA_real_, overlap = NA_real_, depth = NA_real_, crossed = NA
    ))
  }
  upper <- pmin(pmax(upper, y_bounds[[1L]]), y_bounds[[2L]])
  lower <- pmin(pmax(lower, y_bounds[[1L]]), y_bounds[[2L]])
  gap <- upper - lower
  gap[!usable] <- 0
  band <- gap
  band[band < 0] <- 0
  overlap <- -gap
  overlap[overlap < 0] <- 0
  list(
    area = .nsca_integrate(band, grid),
    overlap = .nsca_integrate(overlap, grid),
    depth = max(c(0, overlap)),
    crossed = any(gap[usable] < -.Machine$double.eps^0.5)
  )
}

# Trapezoidal integration, written out because utils::trapz does not exist and
# base R gained a trapezoid helper only recently.
.nsca_integrate <- function(values, grid) {
  n <- length(grid)
  if (n < 2L) {
    return(0)
  }
  sum((values[-1L] + values[-n]) / 2 * diff(grid))
}
