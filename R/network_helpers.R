#' Convert weighted qgraph object to igraph object
#' @description
#' A wrapper for `igraph::graph_from_adjacency_matrix``
#'
#'@export
qgraph2igraph = function(x) {
  igraph::graph_from_adjacency_matrix(
    getWmat(x),
    mode = "undirected",
    weighted = T)
}


#' Network pruning
#'
#' Removes all the edges that proved to be either non-significant or smaller than the specified threshold. Returns `bootnetResult` object.
#'
#' @param net `bootnetResult` network
#' @param boot `bootnet` object, should be bootstrapped network from `net` argument.
#' @param thesh cutoff level (absolute) below which the edges are removed. By default, NA, keeps all the significant edges.
#' @export
prune_boot <- function(net, boot, thresh = NA) {
  p.edge.tab1 = summary(boot)
  p.edge.tab1 = p.edge.tab1 %>% mutate(sig = as.numeric(CIlower * CIupper >0))

  if(!is.na(thresh)) p.edge.tab1[abs(p.edge.tab1$mean)<abs(thresh), "sig"] <- 0

  edges.part1 = p.edge.tab1 %>% ungroup()  %>% filter( type == "edge") %>% dplyr::select(node1, node2, sig)
  edges.part2 = edges.part1 %>% rename(node1 = 'node2', node2 = 'node1')
  edges = rbind(edges.part1, edges.part2) %>%
    dcast(node1 ~ node2, value.var= "sig") %>% set_rownames(.$node1) %>% dplyr::select(-node1) %>% as.matrix
  diag(edges)=0

  zeroes = rownames(net$graph)
  if(length(names(zeroes))>0) zeroes = names(zeroes)

  net$graph[edges[zeroes, zeroes]==0] <- 0
  net
}

#' Plot networks with identified communities
#'
#' @description
#' A specialized function to plot network structures, overlaying community detection
#' (clusters) via convex hulls and allowing for mapping of theoretical domains
#' to node aesthetics. It utilizes [ggplot2::ggplot()] and [ggforce::geom_mark_hull()]
#' for rendering.
#'
#' @param net Network produced by [bootnet::estimateNetwork()].
#' @param layout Character string or matrix specifying the network layout. Passed to [qgraph::qgraph()]. Defaults to `"spring"`.
#' @param original.categories A `data.frame` with two columns, `"label"` (categories) and `"L1"` (nodes). Used for assigning nodes to theoretical domains.
#' @param pointsize Numeric scalar or vector specifying node size(s). Defaults to `10`.
#' @param concavity Numeric value for the concavity of the cluster hulls. Passed to [ggforce::geom_mark_hull()]. Defaults to `3`.
#' @param legend.title Character string for the fill legend title. Defaults to `""`.
#' @param labels Custom node labels, should be in order of the nodes in the `net$graph`. Defaults to `NA`.
#' @param labelsize Numeric value specifying the size of node labels. Defaults to `3`.
#' @param radius Numeric value specifying the corner radius of cluster hulls. Passed to [ggforce::geom_mark_hull()]. Defaults to `0.04`.
#' @param shape Numeric value specifying the [ggplot2::geom_point()] shape for nodes. Defaults to `21`.
#' @param outline Logical. If `TRUE`, uses [ggplot2::geom_label()] to draw a box around text. If `FALSE`, uses [ggplot2::geom_text()]. Defaults to `FALSE`.
#' @param colored Logical. If `TRUE`, edges are colored (e.g., red/blue for negative/positive). If `FALSE`, edge sign is represented by linetype. Defaults to `FALSE`.
#' @param alpha Numeric value between 0 and 1 specifying edge transparency. Defaults to `1`.
#' @param cluster.base Network object to use for community detection. Defaults to the `net` argument.
#' @param fill.colors Vector of character colors for the node fill aesthetic. If `NA`, defaults to a colorblind-friendly palette.
#' @param show.clusters Logical. If `TRUE`, calculates communities and plots convex hulls around them. Defaults to `TRUE`.
#' @param ... Additional arguments passed to [EGAnet::community.detection()].
#'
#' @return A [ggplot2::ggplot()] object representing the network graph.
#' @md
#' @examples
#' mtcars = datasets::mtcars
#' library(bootnet)
#' library(EGAnet)
#'
#' # Estimate network
#' network <- estimateNetwork(mtcars, default = "EBICglasso")
#'
#' # Plot with clusters
#' plot_clusters(network, layout = "spring", show.clusters = TRUE)
#'
#' @importFrom dplyr %>% mutate rename
#' @importFrom ggplot2 ggplot aes geom_segment scale_color_manual scale_linetype_manual geom_point geom_text geom_label scale_linewidth_continuous scale_fill_manual theme_void scale_y_continuous scale_x_continuous expansion
#' @importFrom ggforce geom_mark_hull
#' @export
plot_clusters <- function(net,
                          layout = "spring",
                          original.categories = NA,
                          pointsize = 10,
                          concavity = 3,
                          legend.title = "",
                          labels = NA,
                          labelsize = 3,
                          radius = 0.04,
                          shape = 21,
                          outline = F,
                          colored = F,
                          alpha = 1,
                          cluster.base = net,
                          fill.colors = NA,
                          show.clusters = T,
                          ...
)  {

# experimental part
  if(is.matrix(net)|is.data.frame(net)) {
    net <- list(
      graph = net,
      labels = colnames(net),
      directed = F
    )
    class(net) <- "bootnetResult"
  }

  # normal part
  bb <- plot(net, weighted = TRUE, signed = TRUE, DoNotPlot = TRUE, layout = layout)

  if (is.character(layout)) {
    bb.lay <- bb$layout
  } else if (is.matrix(layout)) {
    bb.lay <- layout
  } else {
    warning("Can't recognize layout. Use either a matrix or a character string.")
  }

  nodes <- as.data.frame(bb.lay) %>%
    dplyr::rename(x = "V1", y = "V2") %>%
    dplyr::mutate(
      label = net$labels,
      label.long = if (all(is.na(labels))) net$labels else labels
    )

  if (length(pointsize) > 1) {
    nodes <- cbind(nodes, pointsize)
  }

  # finding clusters
  if (show.clusters) {
    clusters <- EGAnet::community.detection(cluster.base$graph, ...) %>%
      as.data.frame() %>%
      dplyr::mutate(items = row.names(.)) %>%
      dplyr::rename(dimension = "x")

    nodes <- merge(nodes, clusters, by.x = "label", by.y = "items", all.x = TRUE, sort = FALSE)
  }

  # adding theoretical domains
  if (!all(is.na(original.categories))) {
    nodes <- merge(nodes, original.categories, by = "label", all.x = TRUE, sort = FALSE)
  } else {
    nodes$L1 <- "All"
  }

  # reshaping edges
  edges <- data.frame(
    from = bb$Edgelist$from,
    to = bb$Edgelist$to,
    weight = bb$Edgelist$weight
  )
  edges$from <- net$labels[edges$from]
  edges$to <- net$labels[edges$to]

  edgenodes <- merge(edges, nodes, by.x = "to", by.y = "label", all.x = TRUE, sort = FALSE)
  edgenodes <- merge(edgenodes, nodes, by.x = "from", by.y = "label", all.x = TRUE, sort = FALSE, suffixes = c("_n2", "_n1"))

  if (all(is.na(fill.colors))) {
    fill.colors <- qgraph:::colorblind(length(unique(nodes$L1)))
  }

  gg1 <- ggplot2::ggplot(nodes, ggplot2::aes(x, y))

  if (show.clusters) {
    gg1 <- gg1 + ggforce::geom_mark_hull(
      data = nodes[!is.na(nodes$dimension), ],
      ggplot2::aes(group = as.factor(dimension)),
      fill = "darkgrey", alpha = 0.5,
      show.legend = FALSE, # color = NA,
      concavity = concavity, radius = radius,
      na.rm = TRUE
    )
  }

  if (colored) {
    gg1 <- gg1 + ggplot2::geom_segment(
      data = edgenodes,
      ggplot2::aes(
        x = x_n1, xend = x_n2,
        y = y_n1, yend = y_n2,
        # linetype = weight<0,
        color = weight < 0,
        linewidth = abs(weight) # , alpha = weight
      ),
      show.legend = FALSE, alpha = alpha
    ) +
      ggplot2::scale_color_manual(values = c("TRUE" = "tomato", "FALSE" = "royalblue"))
  } else {
    gg1 <- gg1 + ggplot2::geom_segment(
      data = edgenodes,
      ggplot2::aes(
        x = x_n1, xend = x_n2,
        y = y_n1, yend = y_n2,
        linetype = weight < 0,
        # color=weight<0,
        linewidth = abs(weight) # , alpha = weight
      ),
      show.legend = FALSE, alpha = alpha
    ) +
      ggplot2::scale_linetype_manual(values = c("TRUE" = "dashed", "FALSE" = "solid"))
  }

  if (length(pointsize) == 1) {
    gg1 <- gg1 + ggplot2::geom_point(ggplot2::aes(fill = L1), size = pointsize, shape = shape)
  } else {
    gg1 <- gg1 + ggplot2::geom_point(ggplot2::aes(fill = L1, size = pointsize), shape = shape)
  }


  if (!outline) {
    gg1 <- gg1 + ggplot2::geom_text(ggplot2::aes(label = label.long), color = "black", size = labelsize)
  } else {
    gg1 <- gg1 + ggplot2::geom_label(ggplot2::aes(label = label.long, fill = L1), size = labelsize)
  }

  gg1 +
    ggplot2::scale_linewidth_continuous(range = c(.1, 3)) +
    ggplot2::scale_fill_manual(
      values = fill.colors[1:length(unique(nodes$L1))],
      name = legend.title
    ) +
    # scale_fill_grey()+
    ggplot2::theme_void() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.1, .1))) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(.1, .1)))
}



#' Summarize fit indices for multiple network models fitted with psychonetrics package
#' @param model.list A list of fitted network models
#' @param what An optional vector of fit indices to summarize
#' @return A data frame with fit indices for each model as columns and fit indices as rows
#' @export
summ_pnet <- function(model.list, what = c("cfi", "rmsea", "srmr", "chisq", "pvalue")) {
  out <- Reduce(function(x, y) merge(x, y, by = "Measure", sort = F),
         lapply(names(model.list), function(x) {
           trash = capture.output(mfit <- fit(model.list[[x]]))
           out = mfit %>% as.data.frame
           colnames(out)[[2]] <- x
           out
         })) %>% set_rownames(.$Measure)

  if(length(what)>0) out %<>% `[`(what,)

     return(out)
}


#' Convert lavaan syntax to psychonetrix factor structure (lambda) matrix
#' @param model A lavaan model syntax
#' @param estimates Logical. If TRUE, uses estimated loadings from a fitted lavaan model. If FALSE, returns just a structure.
#' @export
lav_to_pnet_str <- function(model, estimates = F) {

  if( "psychonetrics" %in% rownames(installed.packages())) {
    requireNamespace("psychonetrics")
  } else {
    stop("Package 'psychonetrics' is required for this function. Please install it first.")
  }

  if(is.character(model)) {
    model <- lavaan::lavaanify(model)
    if(estimates) {
      measure = "ustart"
      warning("Because estimates = T but the model is not fitted, will try to use user starting values. They must be specified! Otherwise set estimates to FALSE")
    } else {
      measure = "user"
    }
  } else if(inherits(model, "lavaan")) {

    model <- lavaan::parTable(model)
    measure = ifelse(estimates, "est", "user")

  } else if(!inherits(model, "lavaanify")) {
    stop("model must be a character string, a list of character strings (for multiple group models), or a lavaan fitted object.")
  }


factor.matrix0 =
  model %>%
  filter(op == "=~") %>%
  mutate(ordering = as.numeric(factor(rhs, levels = rhs[!duplicated(rhs)])))

if(length(unique(factor.matrix0$group))==1) {

  factor.matrix <-
    dcast(factor.matrix0,
          rhs + ordering ~ lhs, value.var = measure, fill = 0) %>%
    arrange(ordering) %>%  # preserving the general order, so that the same anchor indicators' loadings  get fixed to 1
    select(-ordering) %>%
    tibble::column_to_rownames("rhs") %>%  # adds indicator names
    as.matrix
  # put the cross-loadings to the end of the matrix, so they don't get fixed to 1 by default
  factor.matrix <- factor.matrix[order(rowSums(factor.matrix)),]

} else {

  factor.matrix <-
    lapply(split(factor.matrix0, factor.matrix0$group), function(mat.g) {
      out = mat.g %>%
        dcast(rhs + ordering ~ lhs, value.var = measure, fill = 0) %>%
        arrange(ordering) %>%  # preserving the general order, so that the same anchor indicators' loadings  get fixed to 1
        select(-ordering) %>%
        tibble::column_to_rownames("rhs") %>%  # adds indicator names
        as.matrix
      # put the cross-loadings to the end of the matrix
      factor.matrix <- out[order(rowSums(out!=0)),]
    })


}

return(factor.matrix)

}

#' Convert lavaan fitted object to fix the psychonetrics model's loadings
#' @param fit A lavaan fitted object
#' @param lnm.model Optional. UNfitted psychonetrics model object. Must have the same factor structure as the lavaan fitted object. If not provided, a new psychonetrics model will be created based on the lavaan model's structure.
#' @param ... Additional arguments passed to the function `lnm` of `psychonetrics` package.
#'
#' @returns A psychonetrics model with fixed loadings based on the lavaan fitted object
#' @examples
#' HS.model <- ' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9 '
#' fit <- cfa(HS.model, data=HolzingerSwineford1939, group = "school")
#' lnm_mod_fixed = lav_to_pnet_fix(fit, data = HolzingerSwineford1939, groupvar = "school")
#' lnm_mod_fixed %>% runmodel %>% parameters
#'
#' @export
lav_to_pnet_fix <- function(fit, lnm.model=NULL, ...) {

  if( "psychonetrics" %in% rownames(installed.packages())) {
   requireNamespace("psychonetrics")
 } else {
   stop("Package 'psychonetrics' is required for this function. Please install it first.")
 }

 if(is.null(lnm.model)) {

   cat("Making a new lnm...")

   Lambda = lav_to_pnet_str(fit)
   varnames = if(length(Lambda)==1)  rownames(Lambda)  else  lapply(Lambda, rownames)
   lvnames = if(length(Lambda)==1)  colnames(Lambda)  else  lapply(Lambda, colnames)


   ngr = lavInspect(fit, "ngroups")
   lnmodel = lnm(lambda = if(ngr>1) Lambda[[1]] else Lambda, # the function wouldn't work with varying factor structures across groups
                 vars = if(ngr>1) varnames[[1]] else varnames,
                 latents = if(ngr>1)  lvnames[[1]] else lvnames,
                 ...)

 } else {
   lnmodel = lnm.model
 }

  # lnmodel.initial = lnmodel
  # getmatrix(lnmodel.initial, "lambda")
  # getmatrix(lnmodel, "lambda")


 if(lavInspect(fit, "ngroups")>1) {
    cat("applying constraints for MG case")

    for(g in 1:lavInspect(fit, "ngroups")) {

         g.estimates = lavInspect(fit, "estimates")[[g]]

         loadings = g.estimates$lambda[rownames(Lambda[[g]]), colnames(Lambda[[g]])]
         indices <- which(loadings != 0, arr.ind = TRUE)

         custom_fixes <- data.frame(
           row = indices[, "row"],
           col = indices[, "col"],
           # Extract the actual estimated values using those indices
           val = loadings[indices]
         )

         #Loop through  extracted estimates and fix them in the new model
         for(i in 1:nrow(custom_fixes)) {
           lnmodel <- lnmodel %>%
             fixpar(
               matrix = "lambda",
               row = custom_fixes$row[i],
               col = custom_fixes$col[i],
               value = custom_fixes$val[i],
               group = g # Keeps the loop fast and prevents crashes
             )

         }
    }
   } else {
   cat("applying constraints for one-group case")

   loadings = lavInspect(fit, "estimates")$lambda
   indices <- which(loadings != 0, arr.ind = TRUE)
   custom_fixes <- data.frame(
     row = indices[, "row"],
     col = indices[, "col"],
     val = loadings[indices]
   )

   #Loop through extracted estimates
   for(i in 1:nrow(custom_fixes)) {
     lnmodel <- lnmodel %>%
       fixpar(
         matrix = "lambda",
         row = custom_fixes$row[i],
         col = custom_fixes$col[i],
         value = custom_fixes$val[i],
         runmodel = FALSE
       )
   }
 }
  return(lnmodel)
}

#' Get variable names matched with their codes in LNM
#' @param pnet A psychonetrics fitted model object
#' @export
var_labs_pnet <- function(pnet) {
  tr= capture.output(param.names1 <- parameters(pnet))
  param.names1 %<>% filter(op == "--") %>% select(var1, var1_id)
  tr= capture.output(param.names2 <- parameters(pnet))
  param.names2 %<>% filter(op == "--") %>% select(var2, var2_id)
  parms = rbind(set_colnames(param.names1, c("label", "parm")),
                set_colnames(param.names2, c("label", "parm"))) %>% distinct %>% arrange(parm)

  setNames(as.vector(parms$label), parms$parm)
}
