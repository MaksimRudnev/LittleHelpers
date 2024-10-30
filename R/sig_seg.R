#' Constructing a significance brackets in ggplot
#' @param df,
#' @param compare.var
#' @param  y.var
#' @param group.var
#' @param heights
#' @param ends
#' @param description

sig_seg <- function(df, compare.var, y.var, group.var, heights, ends) {
  df <-untibble(df)
  df = df[,c(compare.var, y.var, group.var)]
  df[,compare.var]<-as.character(df[,compare.var])

  all.links = expand.grid(unique(df[,compare.var]),
                          unique(df[,compare.var]),
                          stringsAsFactors = F) %>%
    mutate(x = 1:9) %>%
    dcast(Var1 ~ Var2, value.var = "x") %>%
    set_rownames(.$Var1)

  segment.id = all.links[,-1][lower.tri(all.links[,-1])]
  segments = melt(all.links, id.vars = "Var1") %>% filter(value %in% segment.id) %>%
    mutate(variable = as.character(variable))

  framework = lapply(1:nrow(segments), function(x)
    as.data.frame.matrix(matrix(unlist(segments[x, c(1,1,1,2,2,2)]), nrow = 3, byrow = T)) %>% mutate(segm.id = paste(segments[x, c(1,2)], collapse = "_")  ))

  if(is.null(ends)) ends <- d[,y.var]

  segments.paths =
    lapply(df[,group.var], function(pop) {
      a = dplyr::filter(df,df[,group.var]==pop)
      lapply(1:length(framework), function(x) {
        fr = framework[[x]]
        fr$x1 = heights[[x]]
        fr$x2 = heights[[x]]
        if(is.null(ends))  {
          fr[1, "x1"]<- a[,y.var][a[,compare.var] == fr[1,1]]
          fr[3, "x2"]<- a[,y.var][a[,compare.var] == fr[3,1]]
        } else {
          fr[1, "x1"]<- ends[[x]]
          fr[3, "x2"]<- ends[[x]]
        }
        cbind(fr, pop_groups = pop)
      })})

  segm.df = Reduce(rbind, lapply(segments.paths, function(x) Reduce(rbind, x))) %>%
    set_names(c("y", "yend", "segm.id", "x", "xend", group.var))
  segm.df
}
