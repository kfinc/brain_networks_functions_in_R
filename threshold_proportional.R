#' Proportional thresholding of connectivity matrices
#' 
#' The function takes binary/weighted symetrical connectivity matrix and preseves proportion p (0 < p < 1) of the strongest, positive weights.
#' All other weights and diagonal values are set to 0.
#' 
#' @param M weighted symetrical connectivity matrix
#' @param p proportion of weights to preserve. Range p = 0 (no weights preserved) to p = 1 (all weights preserved)
#' @param binarize retuns weighted (FALSE) or binarized (TRUE) thresholded matrix
#' @return thresholded weighted or binary connectivity matrix
#' 
#' #' Karolina Finc, Centre for Modern Interdisciplinary Technologies, NCU

threshold_proportional <- function(M, p, binarize = FALSE){
  
  if (isSymmetric(M)){
    diag(M) <-  0
    vec <- as.vector(M)
    vec <- vec[vec > 0]
    vec_sorted <- sort(vec, decreasing = TRUE)
    min_edge <- round(p * length(vec_sorted))
    thr <- vec_sorted[min_edge]
    M[M < thr] <- 0
    if (binarize == TRUE){
      M[M > 0] <- 1
    }
    M
  }
  
  else{
    message("Error: matrix unsymetrical.")
  }
}