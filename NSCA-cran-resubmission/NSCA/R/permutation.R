# The shared permutation sequence.
#
# The two component p-values that the engines produce come from two separate
# permutation runs with two separate random draws. That is fine while the two
# tests are only ever combined by an intersection-union rule, which needs no
# assumption about their joint distribution. It stops being fine the moment a
# statistic of both components is tested, because
#
#   W = min(d_nec, d_suf)
#
# has a null distribution that depends on how the two components co-vary under
# random pairing, and that dependence is not free: the two empty zones are
# disjoint subsets of one scope box, so d_nec + d_suf is bounded and the two
# cannot both be large by accident. Drawing them from independent permutations
# throws that constraint away and produces a null distribution for the minimum
# that no dataset could have generated.
#
# So this file drives the permutation itself. One shuffle of the outcome breaks
# the X-Y pairing for every condition at once; both engines then estimate on
# that same shuffled frame; and d_nec, d_suf and their minimum are read off
# together. All three p-values come from one sequence and are mutually
# consistent, which is what a joint statistic requires.
#
# The estimators are the engines' own. Nothing here re-implements a frontier,
# so the null draws are on exactly the same scale as the observed values they
# are compared with. The cost is two full engine fits per replication, which is
# why this is off by default.

# Phipson and Smyth (2010): a permutation p-value must count the observed
# arrangement itself, or it can report zero for something that happened once.
.nsca_permutation_p <- function(draws, observed) {
  draws <- draws[is.finite(draws)]
  if (!is.finite(observed) || length(draws) == 0L) {
    return(NA_real_)
  }
  (1 + sum(draws >= observed)) / (length(draws) + 1)
}

# Preserve the caller's random stream. A seed given here is for reproducing
# this test, not for silently reseeding the session.
.nsca_with_seed <- function(seed, expr) {
  if (is.null(seed)) {
    return(force(expr))
  }
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_seed) {
    previous <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", previous, envir = globalenv()), add = TRUE)
  } else {
    # No stream existed yet. Leaving the one set.seed() creates would make every
    # later draw in the session a function of this seed.
    on.exit(
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      },
      add = TRUE
    )
  }
  set.seed(seed)
  force(expr)
}

#' Null draws for both components from one permutation sequence
#'
#' @param frame The complete data frame, as supplied to [nsca_analysis()].
#' @param x_names,y_name Condition and outcome column names.
#' @param direction Validated direction vector, one per condition.
#' @param ceilings Frontier techniques.
#' @param scope Theoretical scope, passed through unchanged.
#' @param necessity_corner Unnamed corner vector, one per condition.
#' @param cutoff,qr.tau Engine settings, kept identical to the observed fit.
#' @param reps Number of permutations.
#' @param seed Optional seed for reproducibility.
#' @return A list of three arrays with dimensions
#'   `reps x conditions x ceilings`, holding the necessity, sufficiency and
#'   weakest-effect draws.
#' @noRd
.nsca_shared_null <- function(frame, x_names, y_name, direction, ceilings,
                              scope, necessity_corner, cutoff, qr.tau,
                              reps, seed = NULL) {
  observations <- nrow(frame)
  shape <- c(reps, length(x_names), length(ceilings))
  labels <- list(NULL, x_names, ceilings)
  necessity <- array(NA_real_, dim = shape, dimnames = labels)
  sufficiency <- array(NA_real_, dim = shape, dimnames = labels)

  .nsca_with_seed(seed, {
    for (b in seq_len(reps)) {
      shuffled <- frame
      shuffled[[y_name]] <- frame[[y_name]][sample.int(observations)]

      # A permutation of the outcome leaves both marginals unchanged, so the
      # empirical scope of the shuffled frame is identical to the observed
      # one. The areas are therefore measured against the same denominator
      # without having to pin the scope down by hand.
      nec_fit <- tryCatch(
        suppressWarnings(NCA::nca_analysis(
          data = shuffled, x = x_names, y = y_name, ceilings = ceilings,
          corner = necessity_corner, scope = scope,
          bottleneck.x = "actual", bottleneck.y = "actual",
          cutoff = cutoff, qr.tau = qr.tau, effect_aggregation = 1,
          test.rep = 0
        )),
        error = function(e) NULL
      )
      suf_fit <- tryCatch(
        suppressWarnings(SCAtools::sca_analysis(
          data = shuffled, x = x_names, y = y_name, direction = direction,
          ceilings = ceilings, scope = scope, cutoff = cutoff,
          qr.tau = qr.tau, test.rep = 0
        )),
        error = function(e) NULL
      )
      if (is.null(nec_fit) || is.null(suf_fit)) {
        next
      }

      for (i in seq_along(x_names)) {
        for (k in seq_along(ceilings)) {
          necessity[b, i, k] <- .nsca_nca_param(
            nec_fit, x_names[[i]], ceilings[[k]], "Effect size"
          )
          sufficiency[b, i, k] <- .nsca_sca_param(
            suf_fit, x_names[[i]], ceilings[[k]], "Effect size"
          )
        }
      }
    }
  })

  weakest <- array(
    suppressWarnings(pmin(necessity, sufficiency)),
    dim = shape, dimnames = labels
  )
  list(necessity = necessity, sufficiency = sufficiency, weakest = weakest)
}
