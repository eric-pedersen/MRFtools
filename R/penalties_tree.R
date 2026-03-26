# internal function to get the shortest phylogenetic distance between two nodes
# in a given tree
get_treedist <- function(tree, tip1, tip2){
  path <- names(phylobase::shortestPath(tree, tip1, tip2))
  #drop the most recent common ancestor of the two nodes
  path <- path[-1]
  #add the two tips in for calculating the total path
  path <- c(path, tip1, tip2)
  sum(phylobase::edgeLength(tree, path))
}

## functions for creating "tree-like" penalties (e.g. phylogenies, dendrograms,
## hierarchical clusters)

#' @title MRF penalty from a phylogeny from a phylo4 object
#'
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom phylobase subset
#' @importMethodsFrom phylobase phylo4
#' @importFrom Matrix Diagonal sparseMatrix Matrix diag
#' @importMethodsFrom Matrix t colMeans colSums 
#' @export
`mrf_penalty.phylo4` <- function(
    object,
    model = c("rw1", "ou", "brownian"),
    rho = NULL, 
    at_tips = NULL,
    internal_nodes = TRUE,
    delta = FALSE,
    ...
) {
  
  model <- match.arg(model)
  
  delta <- check_delta(delta)
  
  tip_labs <- phylobase::tipLabels(object)
  node_labs <- phylobase::labels(object)
  
  if(!is.null(at_tips)){
    object_tips <- as.character(phylobase::tipLabels(object))
    if(any(!at_tips %in% object_tips)){
      stop("One or more values listed in `at_tips` are not named tips in `object`") 
    }
    object <- phylobase::subset(object, tips_include = at_tips) |>
      phylobase::extractTree()
  } else{
    #remove any additional data beyond the phylogenetic object itself
    object <- phylobase::extractTree(object)
  }
  
  n_nodes <- length(unique(object@label))
  edgelist <- as(object, "data.frame") 
  #removing any ancestor connection from the list
  edgelist <- edgelist[edgelist$ancestor != 0, ]
  
  i <- pmin(edgelist$node, edgelist$ancestor)
  j <- pmax(edgelist$node, edgelist$ancestor)
  
  edge_lengths <- edgelist$edge.length
  
  if(model %in% c("rw1", "brownian")){
    pen <- prec_rw1(start = i, end = j, n = n_nodes, dists = edge_lengths)
  } else if(model == "ou"){
    pen <- prec_ou(start = i, end = j, n = n_nodes, dists = edge_lengths, rho = rho)
  }
  
  is_tip <- node_labs %in% tip_labs
  
  if(delta){
    #add delta values on to the tips. This should be equivalent  to Pagel's
    #Lambda (need to confirm this)
    diag(pen)[is_tip] <- diag(pen)[is_tip] + delta
  }
  
  if(internal_nodes){
    node_labels <- as.vector(node_labs)
  } else{
    node_labels <- as.vector(tip_labs)
    tip_nodes <- edgelist$node[is_tip]

    # Need to use a block-matrix inversion to get the precision matrix.
    internal_mat <- pen[-tip_nodes, -tip_nodes]
    cross_mat <- pen[tip_nodes, -tip_nodes]
    
    pen <- pen[tip_nodes, tip_nodes] - cross_mat %*% solve(internal_mat) %*% t(cross_mat)
  }
  
  pen <- as.matrix(pen)
  pen <- as_mrf_penalty(
    pen,
    config = mrf_config(
      type = "tree",
      model = model,
      node_labels = node_labels,
      obj = object,
      delta = delta
    )
  )
  
  pen
  
}



#' @title MRF penalty from a phylogeny
#'
#' @inheritParams mrf_penalty.phylo4
#'
#' @importFrom ape vcv drop.tip
#' @param eps A value to add to the variance-covariance matrix diagonal to
#' make it positive definite
#' 
#' @examples
#' # example code
#' 
#' @export
`mrf_penalty.phylo` <- function(
    object,
    model = c("rw1", "ou", "Brownian"),
    at_tips = NULL,
    tip_labels = NULL,
    include_internal = TRUE,
    delta = FALSE,
    ...
) {
  
  
  
  object <- as(object, "phylo4")
  
  mrf_penalty(
    as(object,"phylo4"),
    model=model, 
    at_tips=at_tips, 
    tip_labels=tip_labels,
    include_internal=include_internal, 
    ...
    )
  
}


#' @title MRF penalty from a dendrogram
#'
#' @inheritParams mrf_penalty.phylo4
#'
#' @importFrom stats cophenetic
#' @export
`mrf_penalty.dendrogram` <- function(
    object,
    model = NULL,
    node_labels = NULL,
    delta = FALSE,
    ...
) {
  delta <- check_delta(delta)
  
  pen <- as.matrix(cophenetic(object))
  pen <- pen - max(pen)
  diag(pen) <- -(rowSums(pen) - diag(pen)) + delta
  
  if (!is.null(node_labels)) {
    if (length(node_labels) != nrow(pen)) {
      stop(
        "'node_labels' is not the same length as the number of observations."
      )
    }
  } else {
    node_labels <- rownames(pen)
  }
  
  pen <- as_mrf_penalty(
    pen,
    config = mrf_config(
      type = "dendrogram",
      model = model,
      obj = object,
      node_labels = node_labels,
      delta = delta
    )
  )
  
  pen
}



#' @title MRF penalty from a hclust object
#'
#' @inheritParams mrf_penalty.factor
#'
#' @importFrom stats as.dendrogram
#'
#' @export
`mrf_penalty.hclust` <- function(object, ...) {
  mrf_penalty(as.dendrogram(object), ...)
}
