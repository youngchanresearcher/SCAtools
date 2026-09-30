library(NSCA)

# Reproduces the visual check supplied with NSCA 0.1.0. The important repair is
# that X2 is not sorted: sorting both X2 and an outcome already ordered by X1
# creates a strong monotone relation and is not a valid independence check.
set.seed(11)
n <- 70L
x <- sort(runif(n))
base_y <- pmin(pmax(x + rnorm(n, 0, 0.11), 0), 1)

# All four logical directions use diagonally opposite physical corners.
fits <- lapply(
  c("HH", "LH", "HL", "LL"),
  function(direction) {
    y <- if (direction %in% c("HH", "LL")) base_y else 1 - base_y
    nsca_analysis(
      data.frame(X = x, Y = y), "X", "Y",
      direction = direction, ceilings = "ce_fdh"
    )
  }
)
names(fits) <- c("HH", "LH", "HL", "LL")
stopifnot(vapply(fits, function(fit) inherits(nsca_plot(fit), "ggplot"), TRUE))

# X2 is now genuinely unrelated to Y and should leave a much wider band.
two <- nsca_analysis(
  data.frame(X1 = x, X2 = runif(n), Y = base_y),
  c("X1", "X2"), "Y", ceilings = "ce_fdh"
)
joint <- nsca_table(two)
band_x1 <- joint$admissible_region_share[joint$condition == "X1"]
band_x2 <- joint$admissible_region_share[joint$condition == "X2"]
stopifnot(band_x2 > 0.4, band_x2 > band_x1 + 0.2)

# Each fitted model exposes NCA, SCA and joint NSCA tables.
stopifnot(identical(
  names(nsca_results(fits$HH)),
  c("necessity", "sufficiency", "necessary_and_sufficient")
))
