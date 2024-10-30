#' Compare two correlation matrices
#'
#' Produces data frame of correlations with stars standing for significance
#'
#' @param corr.X First correlation matrix, e.g. produced by `cor` function
#' @param corr.Y Secon correlation matrix, e.g. produced by `cor` function
#'
#' @export
compare_cor <- function(corr.X, corr.Y) {
  differences = merge(
    melt(corr.X),
    melt(corr.Y),
    by = c("Var1", "Var2"), suffixes = c(".X", ".Y")) %>%
    mutate(drop = apply(cbind(.$Var1, .$Var2), 1, function(x) any(duplicated(x)) )) %>%
    mutate(drop2 = apply(cbind(.$Var1, .$Var2), 1, function(x) which(colSums(t(cbind(.$Var2, .$Var1)) == x)==2) )) %>%
    mutate(drop3 = rownames(.) > drop2 ) %>%
    filter(!drop & !drop3) %>%
    mutate(diff = round(value.X - value.Y , 2)) %>%
    arrange(desc(abs(diff)))


  list(differences = differences,
       summary = c(
         average.raw.diff = mean(differences$diff),
         sd.raw.diff = sd(differences$diff),
         average.abs.diff = mean(abs(differences$diff)),
         sd.abs.diff =  sd(abs(differences$diff)),
         fisher.abs.diff = psych::fisherz2r(mean(psych::fisherz(abs(differences$diff)))),
         fisher.abs.dfiff.sd = psych::fisherz2r(sd(psych::fisherz(abs(differences$diff)))
         )))
}

#' Conditional formatting
#'
#' Adds formatting to a vector of parameters
#'
#' @param x Character vector
#' @param star Removes stars if FALSE.
#' @param bold.thresh Bold font if the absolute value is larger than this threshold.
#' @param neg.red Red font if the value is negative value.
#' @param small.thresh Replaces the value with `<|.01|` if the magnitude is smaller than the indicated number. Default is .01.

#'
#' @details Can be used within `mutate()` and passed to `knitr::kable` with parameter `kable.options=list(escape=FALSE)`
#'
#'
#' @export
sig_to_bold <- function(x, star = F, bold.thresh = 1, neg.red = T,
                        small.thresh = .01, replace.small = NA) {
  replace.small = ifelse(is.na(replace.small), paste0("&lt;|", sub("0\\.", "\\.", small.thresh), "|"), replace.small)
  enclose.bold = function(y) paste0("<b>", y, "</b>")
  enclose.red = function(y) paste0('<span style="color: red;">', y, "</span>")
  ifelse(is.na(x), NA, {
    x.out = sub("0\\.", "\\.", x)
    x.num = as.numeric(gsub("\\*+", "", x))
    if(!star) x.out = gsub("\\*+", "", x.out)
    if(neg.red) x.out = ifelse(x.num<0, enclose.red(x.out), x.out)
    if(bold.thresh < 1) {
      x.out = ifelse(abs(x.num)>bold.thresh, enclose.bold(x.out), x.out) }
    if(small.thresh != 0) {
      x.out = ifelse(abs(x.num)<small.thresh, replace.small, x.out) }
    return(x.out)
  })}



#' Make a quick network graph
#'
#'
#' @param df Data frame
#' @param cv Correlation matrix (if df is NULL)
#' @param title  Title of the plot
#' @param legend  Legend position
#' @param gamma Gamma parameter for the model selection function, see `ggmModSelect`
#' @param layout  Default is "fr". Network layout. See more at `ggraph::ggraph`
#' @param start  Default is "glasso", see `ggmModSelect`
#' @param stepwise Default is TRUE, see `ggmModSelect`
#' @param seed  Attempt to make plots replicable.
#' @param group.id Vector of regex expressions to apply to variable names and use it to group variables.
#'

#'
#' @details Can be used within `mutate()` and passed to `knitr::kable` with parameter `kable.options=list(escape=FALSE)`
#'
#'
#' @export
get_net <- function(df = NULL, cv = NULL, n = NULL,
                    title = "Model",
                    legend = "none",
                    gamma = .3,
                    layout = "fr",
                    start = "glasso",
                    stepwise = T,
                    seed = 1,
                    group.id = NA) {
  require(qgraph)
  require(tidygraph)


  if(is.null(cv)) {
    cv <- cor(df, use = "pair")
    n <- nrow(na.omit(df))
  }

  group.key.df <- data.frame(varnames = colnames(cv))
  group.key.df$groupnames <- "none"

  if(!is.na(group.id)) {
    for(i in names(group.id)) {
      group.key.df$groupnames = ifelse(grepl(group.id[[i]], group.key.df$varnames), i, group.key.df$groupnames)
    }
  }



  ggm.out <- ggmModSelect(cv, n = n, stepwise=stepwise, gamma = gamma, criterion = 'ebic', start = start,
                          nCores = 6)

  gr.tbl <- ggm.out$graph
  gr.tbl[lower.tri(gr.tbl, diag = T)] <-NA
  gr.tbl2 <- gr.tbl %>% melt %>% filter(value>.05 & !duplicated(.))
  gr.tbl2$Var1 <- as.character(gr.tbl2$Var1)
  gr.tbl2$Var2 <- as.character(gr.tbl2$Var2)
  gr.tbl3 <- as_tbl_graph(gr.tbl2)

  gr.tbl3 %<>%
    activate(nodes) %>%
    mutate(q.id = gsub("\\D", "", name)) %>%
    mutate(factor.lab = group.key.df[match(name, group.key.df$varnames), "groupnames"]) %>%
    mutate(name = gsub("\\D", "", name))
  set.seed(seed)
  ggraph(gr.tbl3, layout = layout, weights = value) +
    geom_edge_fan(aes(edge_width=value), alpha=.5, show.legend = F) +
    geom_node_circle(aes(r=.4, fill = factor.lab), show.legend = T)+
    geom_node_text(aes(label = name), size = 3, family = "Lato")+
    scale_edge_width(range = c(0.0001, 3), guide = "none")+
    theme_void()+
    ggtitle(title)+
    theme(legend.position = legend)+
    coord_equal()+
    guides(fill=guide_legend(ncol=3, title.position = "bottom",
                             title.theme =element_text(size=10),
                             override.aes = list(alpha = 1)))

}

