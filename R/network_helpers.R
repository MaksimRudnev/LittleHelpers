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

  bb <- plot(net, weighted = TRUE, signed = TRUE, DoNotPlot = TRUE, layout = layout)

  if (is.character(layout)) {
    bb.lay <- bb$layout
  } else if (is.matrix(layout)) {
    bb.lay <- layout
  } else {
    warning("Can't recognize layout. Use either matrix or character string.")
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
