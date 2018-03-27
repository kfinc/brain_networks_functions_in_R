#' Calculating edges between large-scale networks (LSNs)
#' 
#' The function takes binary symetrical adjacency matrix, module assignment of each ROI and calculate number of edges between and within each 
#' large-scale network.
#' 
#' @param A adjacency matrix (N x N)
#' @param lsn N-length vector with module assignment for each node
#' @return matrix with number of edges between each module
#' 
#' #' Karolina Finc, Centre for Modern Interdisciplinary Technologies, NCU


calculate_lsn_edges <- function(A, lsn){
  
  lsn_matrix <- data_frame()
  columns <- unique(lsn)
  
  for (col in 1:nrow(columns)){
    module = columns[col, ]
    for (row in 1:nrow(lsn)){
      if (lsn[row, ] == module){
        lsn_matrix[row, col] <- 1
      }
      else{
        lsn_matrix[row, col] <- 0
      }
    }
  }
  
  names(lsn_matrix) <- t(unique(lsn))
  lsn_edges <- t(as.matrix(lsn_matrix)) %*% A %*% as.matrix(lsn_matrix)
  lsn_edges
}
