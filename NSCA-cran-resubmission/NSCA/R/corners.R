# Direction handling for necessary-and-sufficient statements.
#
# A necessary-and-sufficient statement fixes two empty corners at once. Writing the
# sufficiency claim as X -> Y and the necessity claim as Y -> X, and applying
# contraposition to each, puts the two empty regions on opposite corners of the
# same scatter plot. The necessity corner is therefore always the diagonal
# opposite of the sufficiency corner, in every direction.

.nsca_directions <- c("HH", "LH", "HL", "LL")
.nsca_direction_number <- stats::setNames(seq_along(.nsca_directions),
                                          .nsca_directions)
.nsca_equivalence_class <- c(
  HH = "HH/LL", LH = "LH/HL", HL = "LH/HL", LL = "HH/LL"
)

# Corner numbering follows SCAtools: 1 upper-left, 2 upper-right,
# 3 lower-left, 4 lower-right.
.nsca_sufficiency_corner <- c(HH = 4L, LH = 3L, HL = 2L, LL = 1L)
.nsca_necessity_corner <- c(HH = 1L, LH = 2L, HL = 3L, LL = 4L)
.nsca_corner_location <- c(
  `1` = "upper-left", `2` = "upper-right",
  `3` = "lower-left", `4` = "lower-right"
)

# An empty corner at the top means the frontier bounds the data from above.
.nsca_corner_side <- function(corner) {
  ifelse(corner %in% c(1L, 2L), "upper", "lower")
}

# Vocabulary of condition analysis in degree, section 3.2 and Table 1.
#
# A boundary is the theoretical line separating an expected empty space from
# the compatible region; a frontier is that boundary estimated from data;
# ceiling and floor name the direction in which Y is bounded. A boundary that
# bounds from above is a ceiling, one that bounds from below a floor, so the
# type follows the empty corner and nothing else.
#
# Table 1 read through these corners: necessity uses a ceiling exactly when the
# outcome level is high (HH, LH) and a floor when it is low (HL, LL);
# sufficiency, sitting on the diagonally opposite corner, is the reverse.
.nsca_corner_boundary <- c(
  `1` = "ceiling", `2` = "ceiling", `3` = "floor", `4` = "floor"
)

# What each expected empty space contains, in the wording of Table 1.
.nsca_corner_empty_space <- c(
  `1` = "Low X with high Y",
  `2` = "High X with high Y",
  `3` = "Low X with low Y",
  `4` = "High X with low Y"
)

# The direction stated as a relationship between X and Y, as in Table 1. This
# is a claim about the theorised relationship, not about the shape of the
# estimated frontier, which may be a step function or a straight line under any
# of the four types.
.nsca_relationship <- c(
  HH = "Higher X for higher Y",
  LH = "Lower X for higher Y",
  HL = "Higher X for lower Y",
  LL = "Lower X for lower Y"
)

.nsca_boundary_type <- function(corner) {
  unname(.nsca_corner_boundary[as.character(corner)])
}

# Fill the variable names into the Table 1 wording.
.nsca_empty_space_text <- function(corner, x = "X", y = "Y") {
  text <- unname(.nsca_corner_empty_space[as.character(corner)])
  text <- sub("X", x, text, fixed = TRUE)
  sub("Y", y, text, fixed = TRUE)
}

.nsca_relationship_text <- function(direction, x = "X", y = "Y") {
  text <- unname(.nsca_relationship[direction])
  text <- sub("X", x, text, fixed = TRUE)
  sub("Y", y, text, fixed = TRUE)
}

.nsca_validate_direction <- function(direction, n = length(direction)) {
  if (is.factor(direction)) {
    direction <- as.character(direction)
  }
  if (!is.character(direction) || length(direction) == 0L || anyNA(direction)) {
    stop("'direction' must contain one or more of: HH, LH, HL, LL.",
         call. = FALSE)
  }
  direction <- toupper(trimws(direction))
  if (length(direction) == 1L && n > 1L) {
    direction <- rep(direction, n)
  }
  if (length(direction) != n) {
    stop("'direction' must have length 1 or the same length as 'x'.",
         call. = FALSE)
  }
  invalid <- setdiff(unique(direction), .nsca_directions)
  if (length(invalid) > 0L) {
    stop(
      sprintf(
        "Invalid direction(s): %s. Use HH, LH, HL, or LL; the first letter refers to X and the second to Y.",
        paste(invalid, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  direction
}

.nsca_level <- function(letter) ifelse(letter == "H", "High", "Low")

.nsca_statement <- function(direction, x = "X", y = "Y") {
  direction <- .nsca_validate_direction(direction)
  sprintf(
    "%s %s is necessary and sufficient for %s %s",
    .nsca_level(substr(direction, 1L, 1L)), x,
    .nsca_level(substr(direction, 2L, 2L)), y
  )
}

.nsca_necessity_statement <- function(direction, x = "X", y = "Y") {
  direction <- .nsca_validate_direction(direction)
  sprintf(
    "%s %s is necessary for %s %s",
    .nsca_level(substr(direction, 1L, 1L)), x,
    .nsca_level(substr(direction, 2L, 2L)), y
  )
}

.nsca_sufficiency_statement <- function(direction, x = "X", y = "Y") {
  direction <- .nsca_validate_direction(direction)
  sprintf(
    "%s %s is sufficient for %s %s",
    .nsca_level(substr(direction, 1L, 1L)), x,
    .nsca_level(substr(direction, 2L, 2L)), y
  )
}

#' Empty corners of a necessary-and-sufficient statement
#'
#' Returns the two physically empty scatter-plot corners implied by a
#' necessary-and-sufficient statement. The necessity corner is always the
#' diagonal opposite of the sufficiency corner.
#'
#' @param direction One or more of `"HH"`, `"LH"`, `"HL"`, `"LL"`. The first
#'   letter refers to the level of the condition and the second to the level of
#'   the outcome.
#' @return A data frame with the necessity and sufficiency corners and the side
#'   of the point cloud each frontier bounds.
#' @export
#' @examples
#' nsca_corners("HH")
#' nsca_corners(c("HH", "LL"))
nsca_corners <- function(direction = "HH") {
  direction <- .nsca_validate_direction(direction)
  necessity <- unname(.nsca_necessity_corner[direction])
  sufficiency <- unname(.nsca_sufficiency_corner[direction])
  data.frame(
    direction = direction,
    nsca_number = unname(.nsca_direction_number[direction]),
    relationship = unname(.nsca_relationship[direction]),
    necessity_corner = necessity,
    necessity_location = unname(.nsca_corner_location[as.character(necessity)]),
    necessity_empty_space = unname(
      .nsca_corner_empty_space[as.character(necessity)]
    ),
    necessity_boundary = .nsca_boundary_type(necessity),
    necessity_bound = .nsca_corner_side(necessity),
    sufficiency_corner = sufficiency,
    sufficiency_location = unname(.nsca_corner_location[as.character(sufficiency)]),
    sufficiency_empty_space = unname(
      .nsca_corner_empty_space[as.character(sufficiency)]
    ),
    sufficiency_boundary = .nsca_boundary_type(sufficiency),
    sufficiency_bound = .nsca_corner_side(sufficiency),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Complete mapping between directions, corners and component statements
#'
#' Reproduces Table 1 of the condition analysis in degree framework, which sets
#' out the main directional types of the X-Y relationship together with the
#' expected empty space and boundary each implies for necessity and for
#' sufficiency, and adds the corner numbering the frontier engines use.
#'
#' The direction is a claim about the theorised relationship, not about the
#' shape of the estimated frontier, which may be a step function or a straight
#' line under any of the four types. Theory may instead posit a curvilinear or
#' U-shaped relationship, for which the expected empty space is no longer a
#' single corner; those are outside the scope of this package.
#'
#' @return A data frame describing all four necessary-and-sufficient
#'   directions. The `biconditional` column keeps its name: it is the joint
#'   statement written out, and the short logical word is the more usable
#'   column name.
#' @seealso [nsca_corners()], [nsca_terms()]
#' @export
#' @examples
#' nsca_direction_map()
nsca_direction_map <- function() {
  direction <- .nsca_directions
  corners <- nsca_corners(direction)
  data.frame(
    nsca_number = unname(.nsca_direction_number[direction]),
    direction = direction,
    relationship = unname(.nsca_relationship[direction]),
    equivalence_class = unname(.nsca_equivalence_class[direction]),
    # Column name kept: `biconditional` is the concise logical term for the
    # joint statement. The prose everywhere else says necessary-and-sufficient.
    biconditional = .nsca_statement(direction),
    sufficiency_component = .nsca_sufficiency_statement(direction),
    necessity_component = .nsca_necessity_statement(direction),
    necessity_empty_space = corners$necessity_empty_space,
    necessity_boundary = corners$necessity_boundary,
    sufficiency_empty_space = corners$sufficiency_empty_space,
    sufficiency_boundary = corners$sufficiency_boundary,
    necessity_corner = corners$necessity_corner,
    sufficiency_corner = corners$sufficiency_corner,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
