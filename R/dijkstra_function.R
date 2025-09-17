#' Dijkstra's Algorithm
#'
#' This function implements **Dijkstra's algorithm** to compute the path
#' distances from a starting node to all other nodes in a weighted graph.
#'
#' @param wiki_graph A data frame representing the graph, with three columns:
#' \describe{
#'   \item{v1}{The starting node of an edge.}
#'   \item{v2}{The ending node of an edge.}
#'   \item{weight}{A numeric value representing the weight of the edge.}
#' }
#' @param init_node The starting node (must match a node in \code{graph}).
#'
#' @return A numeric vector where each element gives the shortest distance
#' from \code{init_node} to that node. Nodes that are unreachable will have value \code{Inf}.
#'
#' @details
#' Dijkstra's algorithm is a greedy algorithm that repeatedly selects the unvisited
#' node with the smallest known distance, updates the distances of its neighbors,
#' and marks it as visited. The algorithm continues until all nodes are visited or
#' the smallest known distance among unvisited nodes is infinite.
#'
#' @references
#' Wikipedia: Dijkstra's Algorithm - https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#'
#' @examples
#' # Example graph
#' wiki_graph <- data.frame(
#'   v1     = c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'   v2     = c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'   weight = c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
#' )
#'
#' dijkstra(wiki_graph, init_node = 1) #returns [1] 0 7 9 20 20 11
#' dijkstra(wiki_graph, init_node = 3) #returns [1] 9 10 0 11 11 2
#'
#' @export
dijkstra <- function(wiki_graph, init_node) {

  # Validate graph input
  if (!all(c("v1", "v2", "w") %in% names(wiki_graph))) {
    stop("Graph must be a data.frame with columns v1, v2, and w")
  }

  # Get all unique nodes
  nodes <- sort(unique(c(as.character(wiki_graph$v1), as.character(wiki_graph$v2))))
  n <- length(nodes)

  # distance and visited nodes
  dist <- stats::setNames(rep(Inf, length(nodes)), nodes) #starts with infinite because the distance is "unknown"
  visited <- stats::setNames(rep(FALSE, length(nodes)), nodes)

  # distance from init_node to itself
  init_node <- as.character(init_node) # force to character
  if (!(init_node %in% nodes)) {
    stop("init_node is not in the graph")
  }
  dist[init_node] <- 0

  for (i in seq_len(n)) {

    u <- names(which.min(ifelse(visited, Inf, dist))) #unvisited node with the smallest distance

    if (dist[u] == Inf) break   # stop if remaining nodes are unreachable
    visited[u] <- TRUE

    # Find neighbors of u
    neighbors <- wiki_graph[wiki_graph$v1 == u | wiki_graph$v2 == u, ]

    # Loop over neighbors
    for (j in seq_len(nrow(neighbors))) {
      v <- if (neighbors$v1[j] == u) as.character(neighbors$v2[j]) else as.character(neighbors$v1[j])

      # Ensure neighbor exists in dist
      if (!(v %in% names(dist))) {
        stop(paste("Neighbor node", v, "not found in graph"))
      }

      if (!visited[v]) {
        alt <- dist[u] + neighbors$weight[j]
        if (alt < dist[v]) {
          dist[v] <- alt
        }
      }

    }
  }

  #return(unname(dist))
  return(dist)

}
