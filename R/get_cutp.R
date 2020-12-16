#' Getting cut-points of a discretization algorithm given the continuous and the discretized sets.
#'
#' This function returns the cut-points
#' @param disc_train_set The matrix or dataframe of the discretized training dataset.
#' @param cont_train_set The matrix or dataframe of the continuous training dataset.
#' @keywords internal sample test train validation
#' @author Adrien Ehrhardt

# #' @examples
# #' # Create continuous training and test sets.
# #'
# #' # Recovering the cutpoints and discretizing the test
# #' # sets based exclusively on the training dataset.
# #' get_cutp(disc_train_set,cont_train_set)

get_cutp <- function(disc_train_set, cont_train_set) {
  d <- ncol(cont_train_set)
  cutoff <- list()

  for (k in 1:d) {
    if (nlevels(as.factor(disc_train_set[, k])) > 1) {
      pairs <- data.frame(modalites = as.factor(disc_train_set[, k]), cont = cont_train_set[, k])

      min_max <- sqldf::sqldf("
                                       select distinct modalites, min(cont), max(cont)
                                       from pairs
                                       group by modalites")

      min_max <- min_max[order(min_max[, "min(cont)"]), ]
      min_cutoff <- min_max[2:nrow(min_max), "min(cont)", drop = FALSE]
      min_cutoff <- min_cutoff[order(min_cutoff[, "min(cont)"]), "min(cont)", drop = FALSE]
      max_cutoff <- min_max[1:(nrow(min_max) - 1), "max(cont)", drop = FALSE]
      max_cutoff <- max_cutoff[order(max_cutoff[, "max(cont)"]), "max(cont)", drop = FALSE]
      cutoffs <- cbind(min_cutoff, max_cutoff)
      cutoffs <- rowMeans(cutoffs)
      cutoffs[length(cutoffs) + 1] <- -Inf
      cutoffs[length(cutoffs) + 1] <- Inf
      cutoff[[k]] <- cutoffs
    } else {
      cutoff[[k]] <- c(-Inf, Inf)
    }
  }

  return(cutoff)
}
