# The family of joint indices.
#
# min(), the geometric mean and the arithmetic mean are not three rival
# proposals. They are the power means M_p at p = -Inf, 0 and 1, so choosing
# among them is choosing how far a strong component may compensate for a weak
# one, not choosing between a right and a wrong formula. Nothing in the
# geometry of two empty zones fixes p. The package therefore computes two
# points on the family, names the parameter, and leaves the family open.
#
# The normalisation is the one piece that is not a matter of taste. For any
# p <= 1, M_p(d_nec, d_suf) <= M_1 = (d_nec + d_suf)/2, and the two empty zones
# are disjoint subsets of one scope box, so d_nec + d_suf <= 1 and M_p <= 0.5.
# Doubling therefore maps the index onto [0, 1] without adding an assumption,
# and reaches 1 exactly when d_nec = d_suf = 0.5, which is the single-line case
# where the two frontiers coincide and the band closes.

.nsca_power_mean <- function(a, b, p) {
  size <- length(a)
  if (size == 0L) {
    return(numeric(0L))
  }
  if (is.infinite(p)) {
    return(if (p < 0) pmin(a, b) else pmax(a, b))
  }
  if (p == 0) {
    return(sqrt(a * b))
  }
  if (p < 0) {
    # A zero component annihilates any negative-order mean. Computing it
    # directly would raise zero to a negative power and return NaN, which would
    # look like a missing value rather than the zero it is.
    out <- rep(0, size)
    live <- a > 0 & b > 0
    out[live] <- ((a[live]^p + b[live]^p) / 2)^(1 / p)
    return(out)
  }
  ((a^p + b^p) / 2)^(1 / p)
}

#' Joint strength of the two components
#'
#' Combines a necessity effect size and a sufficiency effect size into one
#' joint index, with an explicit choice of how much the stronger component may
#' compensate for the weaker one.
#'
#' The index is the power mean
#' \eqn{M_p = ((d_{nec}^p + d_{suf}^p)/2)^{1/p}}, optionally doubled so that it
#' spans `[0, 1]`. The parameter `p` is the degree of compensation:
#'
#' \describe{
#'   \item{`p = -Inf`}{`min(d_nec, d_suf)`. Fully non-compensatory: no amount
#'   of strength on one side raises the index if the other side is weak. This
#'   is the `weakest_effect` column of [nsca_table()], reported there
#'   unnormalised so that it stays on the components' own scale.}
#'   \item{`p = -1`}{The harmonic mean. Less compensatory than the geometric
#'   mean, still zero as soon as either component is zero.}
#'   \item{`p = 0`}{The geometric mean. **Partially** compensatory: a larger
#'   component does raise the index, but at a diminishing rate, and the index
#'   still collapses to zero if either component does. This is the
#'   `balanced_joint_effect` column of [nsca_table()].}
#'   \item{`p = 1`}{The arithmetic mean, that is, half the sum. Fully
#'   compensatory: one strong component alone can carry the index, so a
#'   necessary-only relation scores as highly as a necessary-and-sufficient
#'   one. This is
#'   why the sum is a poor conjunction summary, but it is the same family, not
#'   a different kind of quantity.}
#' }
#'
#' The geometric mean is the middle course, and describing it as a
#' non-compensatory "AND" would overstate it. What it does is penalise
#' asymmetry: at an equal sum of 0.80, `(0.40, 0.40)` gives 0.80 and
#' `(0.70, 0.10)` gives 0.529.
#'
#' No magnitude benchmarks are supplied. Conventions for a single NCA effect
#' size do not transfer, because this index has a different scale and a
#' different null: under independence both components carry a positive
#' finite-sample bias that a product of the two amplifies rather than cancels,
#' and its size depends on `n`, the frontier technique and the scope. Calibrate
#' by simulation on the design at hand before attaching words to values.
#'
#' @param d_nec,d_suf Necessity and sufficiency effect sizes. Vectors are
#'   recycled to a common length.
#' @param p Degree of the power mean; the degree of compensation. Defaults to
#'   `0`, the geometric mean.
#' @param normalize Multiply by two so that the index spans `[0, 1]`. The
#'   guarantee rests on `M_p <= (d_nec + d_suf)/2 <= 0.5` and therefore holds
#'   only for `p <= 1`.
#' @return A numeric vector, `NA` where either component is missing or
#'   negative.
#' @seealso [nsca_table()], [nsca_analysis()]
#' @export
#' @examples
#' nsca_joint(0.40, 0.40)              # balanced
#' nsca_joint(0.70, 0.10)              # same sum, penalised for asymmetry
#' nsca_joint(0.40, 0.40, p = -Inf)    # the minimum, normalised
#' nsca_joint(0.40, 0.40, p = -Inf, normalize = FALSE)   # weakest_effect
nsca_joint <- function(d_nec, d_suf, p = 0, normalize = TRUE) {
  d_nec <- suppressWarnings(as.numeric(d_nec))
  d_suf <- suppressWarnings(as.numeric(d_suf))
  if (length(d_nec) == 0L || length(d_suf) == 0L) {
    return(numeric(0L))
  }
  if (!is.numeric(p) || length(p) != 1L || is.na(p)) {
    stop("'p' must be a single number, possibly -Inf or Inf.", call. = FALSE)
  }
  if (isTRUE(normalize) && p > 1) {
    warning(
      paste0(
        "Doubling maps the index onto [0, 1] only because M_p <= ",
        "(d_nec + d_suf)/2 <= 0.5, which holds for p <= 1. At p > 1 the ",
        "normalised value can exceed 1."
      ),
      call. = FALSE
    )
  }

  size <- max(length(d_nec), length(d_suf))
  d_nec <- rep_len(d_nec, size)
  d_suf <- rep_len(d_suf, size)

  out <- rep(NA_real_, size)
  usable <- is.finite(d_nec) & is.finite(d_suf) & d_nec >= 0 & d_suf >= 0
  if (any(usable)) {
    out[usable] <- .nsca_power_mean(d_nec[usable], d_suf[usable], p)
  }
  if (isTRUE(normalize)) {
    out <- 2 * out
  }
  out
}
