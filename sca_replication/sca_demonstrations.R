# =====================================================================
# Mean Effects Are Not Sufficiency Effects
# Reproduction script for the four comparative demonstrations
#
# Base R only, no packages required.  The four data files
# config_I.csv, config_II.csv, config_III.csv and config_IV.csv
# must be unzipped next to this script.
#
#   Rscript sca_demonstrations.R
#
# or, inside RStudio / the R console:
#
#   setwd("<the folder holding the CSVs>")
#   source("sca_demonstrations.R")
#
# It re-computes every number in Table 3 and redraws Figures 1 to 4.
# The CE-FDH frontier, the CR-FDH approximation, the effect size and the
# permutation test are implemented here directly from their definitions,
# so this script is an independent check on the reported values rather
# than a second call to the same code.  The equivalent SCAtools calls are
# noted at the foot of the file.
# =====================================================================

SCOPE <- c(xmin = 0, xmax = 100, ymin = 0, ymax = 100)
NPERM <- 5000

# ---------------------------------------------------------------------
# 0.  Locate the data
# ---------------------------------------------------------------------
# Looks in the working directory, in this script's own folder, and in a
# sca_replication/ subfolder of either, so that source() and Rscript both
# work without a manual setwd().

script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) return(dirname(normalizePath(f[1])))
  if (!is.null(sys.frames()) && !is.null(sys.frame(1)$ofile))
    return(dirname(normalizePath(sys.frame(1)$ofile)))
  NULL
}

DATA_DIR <- local({
  cand <- c(getwd(), file.path(getwd(), "sca_replication"))
  sd <- try(script_dir(), silent = TRUE)
  if (!inherits(sd, "try-error") && !is.null(sd))
    cand <- c(cand, sd, file.path(sd, "sca_replication"))
  ok <- cand[vapply(cand, function(p) file.exists(file.path(p, "config_I.csv")), logical(1))]
  if (!length(ok)) {
    stop("Cannot find config_I.csv.\n",
         "  Working directory: ", getwd(), "\n",
         "  Looked in:\n    ", paste(unique(cand), collapse = "\n    "), "\n",
         "  Unzip sca_replication.zip and setwd() to the folder that holds the four CSVs.",
         call. = FALSE)
  }
  ok[1]
})
message("Reading data from: ", DATA_DIR)

## Figures are written to figures_R/ so the shipped PNGs stay untouched
## and the two sets can be compared side by side.
OUT_DIR <- file.path(DATA_DIR, "figures_R")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
message("Writing figures to: ", OUT_DIR)

## png() needs a working device.  cairo is the safe choice on headless Linux.
open_png <- function(file, ...) {
  ok <- tryCatch({ png(file, ..., type = "cairo"); TRUE },
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) png(file, ...)
}

# ---------------------------------------------------------------------
# 1.  Estimators
# ---------------------------------------------------------------------

## CE-FDH sufficiency frontier (free disposal hull, fitted BELOW the data).
## The frontier is the largest non-decreasing f with y_i >= f(x_i) for every
## observation, which is f(x) = min{ y_i : x_i >= x }: a reverse running
## minimum over x.  No observation can lie below it, so frontier accuracy is
## 1.00 by construction.  Peers are the observations at which f steps up.
ce_fdh_floor <- function(x, y) {
  o   <- order(x)
  xs  <- x[o]; ys <- y[o]
  run <- rev(cummin(rev(ys)))                  # min of y over x_i >= x
  peer <- which(ys <= run + 1e-12)
  list(xs = xs, floor = run, px = xs[peer], py = ys[peer])
}

## Effect size d: the share of the declared scope that the frontier leaves empty.
d_step <- function(xs, fl, scope = SCOPE) {
  gx <- c(scope[["xmin"]], xs, scope[["xmax"]])
  gy <- c(fl[1], fl, fl[length(fl)])
  gy <- pmin(pmax(gy, scope[["ymin"]]), scope[["ymax"]])
  area <- sum(diff(gx) * (head(gy, -1) - scope[["ymin"]]))   # right-continuous step
  area / ((scope[["xmax"]] - scope[["xmin"]]) * (scope[["ymax"]] - scope[["ymin"]]))
}

d_observed <- function(x, y) {
  f <- ce_fdh_floor(x, y)
  d_step(f$xs, f$floor)
}

## CR-FDH: a straight line fitted through the CE-FDH peers.
cr_fdh <- function(px, py) {
  if (length(px) < 2) return(c(a = py[1], b = 0))
  cf <- unname(coef(lm(py ~ px)))
  c(a = cf[1], b = cf[2])
}

d_line <- function(a, b, scope = SCOPE) {
  xs <- seq(scope[["xmin"]], scope[["xmax"]], length.out = 20001)
  ys <- pmin(pmax(a + b * xs, scope[["ymin"]]), scope[["ymax"]])
  h  <- ys - scope[["ymin"]]
  area <- sum(diff(xs) * (head(h, -1) + tail(h, -1)) / 2)     # trapezoid
  area / ((scope[["xmax"]] - scope[["xmin"]]) * (scope[["ymax"]] - scope[["ymin"]]))
}

## Permutation test: reshuffle Y against X and record how often a random
## pairing produces an empty space at least as large as the observed one.
perm_p <- function(x, y, d_obs, nperm = NPERM, seed = 99) {
  set.seed(seed)
  hits <- 0L
  for (i in seq_len(nperm)) {
    if (d_observed(x, sample(y)) >= d_obs - 1e-12) hits <- hits + 1L
  }
  (hits + 1) / (nperm + 1)
}

# ---------------------------------------------------------------------
# 2.  One configuration, end to end
# ---------------------------------------------------------------------

analyse <- function(x, y) {
  n   <- length(x)
  fit <- lm(y ~ x); s <- summary(fit)
  b   <- unname(coef(fit)[2]); se <- s$coefficients[2, 2]
  beta <- b * sd(x) / sd(y)
  f   <- ce_fdh_floor(x, y)
  dce <- d_step(f$xs, f$floor)
  fr  <- cr_fdh(f$px, f$py)
  dcr <- d_line(fr[["a"]], fr[["b"]])
  acc <- 1 - mean(y < fr[["a"]] + fr[["b"]] * x - 1e-9)
  list(n = n, b = b, se = se, beta = beta, r2 = s$r.squared,
       p_ols = s$coefficients[2, 4], d_ce = dce, d_cr = dcr,
       p_d = perm_p(x, y, dce), cr_acc = acc,
       frontier_a = fr[["a"]], frontier_b = fr[["b"]],
       n_peers = length(f$px), fl = f)
}

# ---------------------------------------------------------------------
# 3.  Figure
# ---------------------------------------------------------------------

C_OLS <- "#D55E00"; C_CR <- "#0072B2"; C_CE <- "#009E73"
C_PTS <- "#8C9BAB"; C_SCOPE <- "#9AA3AD"; C_FILL <- "#009E7322"

fmt_p <- function(p) {
  if (p < .001) return("< .001")
  paste0("= ", sub("^0", "", sprintf("%.3f", p)))
}

draw <- function(x, y, st, title, file) {
  open_png(file, width = 6.8, height = 4.06, units = "in", res = 220)
  on.exit(dev.off())
  layout(matrix(1:2, nrow = 1), widths = c(1.86, 1.0))
  par(mar = c(4, 4, 2.5, 0.5), cex = 0.74, col.axis = "#5b646d", col.lab = "#1b1f23")

  plot(NA, xlim = c(-3, 103), ylim = c(-3, 103), xlab = "Condition X",
       ylab = "Outcome Y", axes = FALSE)
  axis(1, at = seq(0, 100, 20), col = "#5b646d"); axis(2, at = seq(0, 100, 20),
       col = "#5b646d", las = 1)
  abline(h = seq(0, 100, 20), v = seq(0, 100, 20), col = "#E6E9EC", lwd = 0.6)

  # shaded empty space beneath the step frontier
  gx <- c(0, st$fl$xs, 100)
  gy <- c(st$fl$floor[1], st$fl$floor, st$fl$floor[length(st$fl$floor)])
  sx <- rep(gx, each = 2); sx <- sx[-1]
  sy <- rep(gy, each = 2); sy <- sy[-length(sy)]
  polygon(c(sx, 100, 0), c(sy, 0, 0), col = C_FILL, border = NA)

  points(x, y, pch = 16, cex = 0.42, col = adjustcolor(C_PTS, .55))
  rect(0, 0, 100, 100, border = C_SCOPE, lty = 2, lwd = 0.8)

  abline(lm(y ~ x), col = C_OLS, lwd = 2)
  abline(a = st$frontier_a, b = st$frontier_b, col = C_CR, lwd = 2, lty = 2)
  lines(sx, sy, col = C_CE, lwd = 2)
  points(st$fl$px, st$fl$py, pch = 21, bg = "white", col = C_CE, lwd = 1.2, cex = 0.62)
  title(main = title, adj = 0, cex.main = 1.16, col.main = "#1b1f23", font.main = 1)

  # right panel: legend and statistics
  par(mar = c(0, 0, 0, 0)); plot.new()
  legend("topleft", bty = "n", cex = 0.98, seg.len = 2.1, y.intersp = 1.45,
         legend = c("OLS regression line", "CE-FDH sufficiency frontier",
                    "CR-FDH linear frontier", "CE-FDH peers",
                    "Sufficiency empty space", "Declared scope", "Observations"),
         col = c(C_OLS, C_CE, C_CR, C_CE, C_CE, C_SCOPE, C_PTS),
         lty = c(1, 1, 2, NA, NA, 2, NA), lwd = c(2, 2, 2, NA, NA, 1, NA),
         pch = c(NA, NA, NA, 21, 15, NA, 16), pt.bg = c(NA, NA, NA, "white", NA, NA, NA),
         pt.cex = c(NA, NA, NA, 0.7, 1.5, NA, 0.6))

  txt <- paste0(
    "Mean effect (OLS)\n",
    sprintf("   b = %.3f (SE %.3f)\n", st$b, st$se),
    sprintf("   beta = %.2f   R2 = %.2f\n", st$beta, st$r2),
    "   p ", fmt_p(st$p_ols), "\n\n",
    "Sufficiency effect (SCA)\n",
    sprintf("   d = %.2f (CE-FDH)\n", st$d_ce),
    sprintf("   d = %.2f (CR-FDH)\n", st$d_cr),
    "   p ", fmt_p(st$p_d), "\n\n",
    sprintf("Frontier slope = %.2f\n", st$frontier_b),
    sprintf("CR-FDH accuracy = %.2f\n", st$cr_acc),
    sprintf("n = %d   peers = %d", st$n, st$n_peers))
  text(0.02, 0.46, txt, adj = c(0, 1), cex = 0.95, col = "#1b1f23")
}

# ---------------------------------------------------------------------
# 4.  Run all four
# ---------------------------------------------------------------------

CONFIGS <- list(
  list(tag = "I",   title = "I. Neither focal effect"),
  list(tag = "II",  title = "II. Sufficiency-dominant"),
  list(tag = "III", title = "III. Mean-dominant"),
  list(tag = "IV",  title = "IV. Mean + sufficiency"))

out <- NULL
for (cf in CONFIGS) {
  dat <- read.csv(file.path(DATA_DIR, sprintf("config_%s.csv", cf$tag)))
  st  <- analyse(dat$X, dat$Y)
  draw(dat$X, dat$Y, st, cf$title, file.path(OUT_DIR, sprintf("fig_%s.png", cf$tag)))
  out <- rbind(out, data.frame(
    Configuration = cf$title, n = st$n,
    b = round(st$b, 3), SE = round(st$se, 3), beta = round(st$beta, 3),
    R2 = round(st$r2, 3), p_OLS = signif(st$p_ols, 3),
    d_CE = round(st$d_ce, 3), d_CR = round(st$d_cr, 3),
    p_d = signif(st$p_d, 3), CR_acc = round(st$cr_acc, 3),
    frontier_slope = round(st$frontier_b, 3), peers = st$n_peers))
}
print(out, row.names = FALSE)

# ---------------------------------------------------------------------
# 5.  The same analysis through SCAtools 0.4.3
# ---------------------------------------------------------------------
# library(SCAtools)
# dat <- read.csv("config_II.csv")
# m <- sca(dat, x = "X", y = "Y", scope = c(0, 100, 0, 100),
#          frontiers = c("ce_fdh", "cr_fdh"), test.rep = 5000)
# summary(m)          # effect size d, permutation p, frontier accuracy
# thresholds(m)       # target-specific threshold schedule
# plot(m)
#
# Note on the permutation p values: R and the reference implementation draw
# their reshuffles from different random number streams, so p varies in the
# third decimal between runs.  The classification of each configuration
# (significant vs. not) is unaffected.
# ---------------------------------------------------------------------
