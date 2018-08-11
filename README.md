

# LittleHelpers

<<<<<<< HEAD
Continuously updated collection of little helpers (tm) that facilitates my life in analyzing data (mostly comparative datasets) with R.
=======
Continuously updated collection of little helpers (tm) that facilitate my life in analyzing data (mostly comparative datasets) with R.
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5


Use `devtools::install_github("maksimrudnev/LittleHelpers")` to install.

# Overview

- [Multilevel helpers](#multilevel-helpers)
- [Multigroup helpers](#multigroup-helpers)
- [Tools for labelled data and Rstudio viewer](#tools-for-labelled-data-and-rstudio-viewer)
- [Pipe helpers](#pipe-helpers)
- [Values, Schwartz, ESS](#values-schwartz-ess)
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

- `good_table` Large function that creates customizable coefficients tables using multiple lmer models; outputs in Rstudio viewer.
- `potential_interactions` Exploratory. If you have no idea what cross-level interactions to look for. Computes pairwise tests of all the possible interactions in the `lmer()` model, or simply shows correlations between random effects and group-level variables.
- `random_interaction` Plots cross-level interactions for `lmer()`-fitted models. Customizable. Can automatically choose real moderator values close to mean+-(2)SD.
- `random_plot` Plots random effects from `lmer()`-fitted models.


Compute extra stats for multilevel regressions:

- `explained_variance.merMod` Computes psudo-R-square for two-level regressions fitted with `lmer()`.
- `vif_mer` Compute variance inflation factor for multilevel regressions fitted with `lmer()`.


## Multigroup helpers

- `lavTestScore_clean` Wrapper around `lavaan::lavTestScore()`, merging parameter labels with parameters and groups names and adding stars. Useful when you decide with between-group contraints might be relaxed.
- `mgcfa_diagnose` Print comprehensible output to diagnose problems with MGCFA models.
- `mi_test` Series of measurement invariance tests, analoigous to `semTools::measurementInvariance()`.

- See also [Measurement invariance explorer - Shiny App](https://github.com/MaksimRudnev/MIE)

## Pipe helpers

### Branching/ramifying pipes

Imagine you need to create a list with means, correlations, and regression results. And you like to do it in one single pipe. In general, it is not possible, and you'll have to start a second pipe, probably doing some redundant computations.

Three little functions that allow for branching pipes. It is against [Hadley's idea](http://r4ds.had.co.nz/pipes.html#when-not-to-use-the-pipe), as pipes are in principle linear, and in general I agree, but sometimes it would be comfy to ramify pipes away. It overcomes native `magrittr` `%T>%` by allowing more than one step after cutting the pipe.

- `ramify` Saves current result into temporary object .buf and identifies a point in the pipe where branching will happen. Argument is an id of a ramification.
- `branch` Starts a new branch from the ramify point. (branch(1) can be omitted, as ramify creates the first branch. Second argument is a family of branches, or parent branch. By default it uses the last parent branch created by the last used ramify.
- `harvest` Returns contents of all the branches as a list.

Example that allows it:

```
data.frame(a=1:5, b=1/(1+exp(6:10)) ) %>%
  ramify(1) %>%
    branch(1) %>% colMeans %>% 
    branch(2) %>% lm(a ~ b, .) %>% broom::tidy(.) %>% 
    branch(3) %>% cor %>%
      ramify(2) %>%
        branch(1) %>% round(2) %>%
        branch(2) %>% psych::fisherz(.) %>%
      harvest(2) %>%
  harvest
```



### Save'n'go & Append'n'go

`savengo` is ridiculously  simple but very useful function that saves objects from a middle of your pipe and passes the same object to further elements of the pipe. It allows more efficient debugging and less confusing code, in which you don't have to interrupt your pipe every time you need to save an output.

Its sister function `appendngo` appends an intermediary product to an existing list or a vector.

By analogy, one can create whatever storing function they need.



```
## Example 1
#Saves intermediary result as an object called intermediate.result

final.result <- dt %>% dplyr::filter(score<.5) %>%
                        savengo("intermediate.result") %>% 
                        dplyr::filter(estimated<0)
  
## Example 2
#Saves intermediary result as a first element of existing list myExistingList

final.result <- dt %>% dplyr::filter(score<.5) %>%
                        appendngo(myExistingList, after=0) %>% 
                        dplyr::filter(estimated<0)
```

## Tools for labelled data and Rstudio viewer

Know the labels:

- `label_book` Creates a codebook for data.frames with labels.

Make use of labels: 

- `cor_table` Prints ready-to-publish correlation tables with significance stars.
- `crosstab` Simple cross-tabulation with labels.

Get rid of labels:

- `drop_labs` Drops labels if you don't need them.
- `untibble` Get rid of tibble and get clean data.frame.
- `lab_to_fac` Converts labelled variables to factors.

Make use of Rstudio viewer:

- `df_to_viewer` Puts any data.frame to RStudio viewer. Also works with models and anything that can be passed through `stargazer`.


## Values, Schwartz, ESS

- `values` list of value labels.
- `download_ess` Download European Social Survey data
- `schwartz_circle` Draw Schwartz circle and more with three simple functions: `add_circle`, `add_radius`, and `add_label`.
- `ess_values` Computes 2, 4, or 10 value indices as they are measured in ESS.

## Miscellaneous

- `reverse` Recodes variable in reverse order. Works with labels.
- `replace_by_table` Useful for recoding when matching tables are alsready specified in a table. Particularly useful for translation.
- `mean_se_lower_upper` Simply mean, SE, upper and lower 95% CI.
- `verb` Simply prints its arguments.
<<<<<<< HEAD
- `rename` Renames variables in data.frame without bullshittery.
=======
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5
- `theme_mr` Clean theme for ggplot.
