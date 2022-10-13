

# LittleHelpers

Continuously updated collection of little helpers (tm) that facilitates my life in analyzing data (mostly comparative datasets) with R.


Use `remotes::install_github("maksimrudnev/LittleHelpers")` to install.

# Overview

- [Multilevel helpers](#multilevel-helpers)
- [Multigroup helpers](#multigroup-helpers)
- [Tools for labelled data and Rstudio viewer](#tools-for-labelled-data-and-rstudio-viewer)
- deprecated - Pipe helpers [see [gist](https://gist.github.com/MaksimRudnev/bf81eab9f39bd830f9f167c669444472)]
- deprecated - Values, Schwartz, ESS
- [Miscellaneous](#miscellaneous)

## Multilevel helpers

Explore multilevel data:

- `cor_within` prints and plots individual correlations within each group.
- `cor_between` computes means and shows group-level correlation between two variables.
- `scatter_means_ci` Computes means by group and plots on scatterplot against each other (shows country-level correlations).
- `graph_means_ci` Plots means by group.
- `stacked_bar` Computes proportions cross-table and plots them in a nice way, returns ggplot object, so any further `+theme()`, `+scale_x()`, etc. codes can be added.

Recode multilevel data:

- `aggr_and_merge` helps to create group-level variables from individual-level variables and merge them back to the data.frame on the go.
- `grand_center` Quick grand-mean centering.
- `group_center` Quick group-mean centering.

Summarize and visualize multilevel regressions:

- `good_table`/`lmer_table` Function that creates customizable coefficients tables using multiple lmer models; outputs in Rstudio viewer.
- `potential_interactions` Exploratory. If you have no idea what cross-level interactions to look for. Computes pairwise tests of all the possible interactions in the `lmer()` model, or simply shows correlations between random effects and group-level variables.
- `random_interaction` Plots cross-level interactions for `lmer()`-fitted models. Customizable. Can automatically choose real moderator values close to mean+-(2)SD.
- `random_plot` Plots random effects from `lmer()`-fitted models.
- `plef` Quick interaction plot for simple models

Compute extra stats for multilevel regressions:

- `explained_variance.merMod` Computes psudo-R-square for two-level regressions fitted with `lmer()`.
- `vif_mer` Compute variance inflation factor for multilevel regressions fitted with `lmer()`.


## Multigroup helpers

- `lavTestScore_clean` Wrapper around `lavaan::lavTestScore()`, merging parameter labels with parameters and groups names and adding stars. Useful when you decide with between-group contraints might be relaxed.
- `mgcfa_diagnose` Print comprehensible output to diagnose problems with MGCFA models.
- [[ Moved to [MIE package](https://github.com/MaksimRudnev/MIE.package) under the name `globalMI()` ]] `mi_test` Series of measurement invariance tests, analogous to `semTools::measurementInvariance()`.

- See also [Measurement invariance explorer](https://github.com/MaksimRudnev/MIE.package)


## Tools for labelled data and Rstudio viewer

Know the labels:

- `label_book`/`label_table` Creates a codebook for data.frames with labels.

Make use of labels: 

- `cor_table` Prints ready-to-publish correlation tables with significance stars.
- `crosstab` Simple cross-tabulation with labels.

Get rid of labels and other tidyverse attributes:

- `drop_labs` Drops labels if you don't need them.
- `untibble` Get rid of tibble and get clean data.frame.
- `lab_to_fac` Converts labelled variables to factors.

Make use of Rstudio viewer:

- `df_to_viewer` Puts any data.frame to RStudio viewer. Also works with models and anything that can be passed through `stargazer` or `kable`.





## Mplus Automation

- `traceplots_mplus` Extracts Bayesian data from data Mplus, draws the trace plots and autocorrelation plots, and saves in a single pdf.
- `checkMplusModel` Checks if the output contains any negative variances and correlations higher than 1. Also can print errors and warnings. Handy when working with many models.
- `diffTestMLR_` Computes Likelihood ratio test for estimators like MLR.
- `getParamsMplus` Extracts parameters from Bayesian models produced by Mplus where `MplusAutomation` fails.
- `partable_mplus` Collects parameters from several models and ,erges them into a single table (persuing the idea of stargazer/semTable for Mplus).



## Miscellaneous

- `reverse` Recodes variable in reverse order. Works with labels.
- `replace_by_table` Useful for recoding when matching tables are alsready specified in a table. Particularly useful for translation.
- `mean_se_lower_upper` Simply mean, SE, upper and lower 95% CI.
- `verb` Simply prints its arguments.
- `theme_mr` Clean theme for ggplot.


## Values, Schwartz, ESS

- `values` list of value labels.
- `download_ess` Download European Social Survey data
<!-- - `schwartz_circle` Draw Schwartz circle and more with three simple functions: `add_circle`, `add_radius`, and `add_label`. !-->
- `ess_values` Computes 2, 4, or 10 value indices as they are measured in ESS.





