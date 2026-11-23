# SCAtools: Sufficient Condition Analysis in R

[![CRAN status](https://www.r-pkg.org/badges/version/SCAtools)](https://CRAN.R-project.org/package=SCAtools)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/SCAtools)](https://CRAN.R-project.org/package=SCAtools)

**SCAtools** is an R package for **Sufficient Condition Analysis (SCA)**, a method within condition analysis in degree. It tests whether a high or low level of a condition is enough to guarantee a high or low level of an outcome.

Regression estimates an average effect: how much the outcome changes, on average, when the condition changes. SCA asks a different question: is a given level of the condition sufficient for a given level of the outcome? If it is, one corner of the scatter plot should be empty, and SCA estimates the boundary and size of that empty space.

## What SCAtools does

- States each of the four sufficiency directions (`"HH"`, `"HL"`, `"LH"`, `"LL"`; condition level first, outcome level second) as a claim, and maps it to the corner it predicts to be empty
- Estimates the frontier that bounds the empty space
- Reports the **sufficiency effect size**: the size of the empty space as a share of the scope
- Runs a **permutation test** of the effect
- Produces a **sufficiency threshold table**
- Shows ordinary least-squares and median regression lines beside the frontier, so the sufficiency effect can be read next to the average effect (`sca_compare()`)
- Offers power analysis, outlier diagnostics, resampling and scope-sensitivity checks

## Installation

Install the released version from CRAN:

```r
install.packages("SCAtools")
```

## Quick example

```r
library(SCAtools)

# Simulated example data shipped with the package
iv <- subset(sca_four_configurations, configuration == "IV")

fit <- sca_analysis(iv, x = "X", y = "Y", direction = "HH",
                    scope = c(0, 100, 0, 100))
fit
```

`sca()` is a quick wrapper that runs the permutation test (1,000 permutations by default).

## Main functions

| Function | Purpose |
|---|---|
| `sca()` | Quick sufficient condition analysis |
| `sca_analysis()` | Full sufficient condition analysis |
| `sca_plot()` | Plot SCA frontiers |
| `sca_thresholds()` | Sufficiency threshold table |
| `sca_test_plot()` | Plot the permutation test |
| `sca_compare()` | Average effect beside sufficiency in degree |
| `sca_power()` | Power for a directional sufficiency analysis |
| `sca_outliers()` | Observations that influence the effect size |
| `sca_terms()` | Terminology of condition analysis in degree |

## Related packages

- [NSCA](https://github.com/youngchanresearcher/NSCA): Necessary-and-Sufficient Condition Analysis, built on SCAtools
- [NCA](https://CRAN.R-project.org/package=NCA): Necessary Condition Analysis, used by SCAtools to estimate frontiers

## Citation

If you use SCAtools, please cite:

Chan, Y. (2026). *From necessity to sufficiency and necessity-and-sufficiency: Condition analysis in degree* [Working paper]. University of Leeds.

Chan, Y. (2026). *SCAtools: Direction-aware sufficient condition analysis* [R package]. https://github.com/youngchanresearcher/SCAtools

## Contact

Young Chan, University of Leeds
ORCID: [0000-0003-4859-4218](https://orcid.org/0000-0003-4859-4218)

Questions and bug reports: [GitHub issues](https://github.com/youngchanresearcher/SCAtools/issues)

## License

GPL (>= 3)
