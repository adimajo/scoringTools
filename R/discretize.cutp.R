#' Discretization of a test dataset when provided with an already discretized training set.
#'
#' This function discretizes a dataset when provided with both continuous and discretized versions of a training dataset.
#' @param cont_test_set The matrix or dataframe of the dataset to discretize.
#' @param disc_train_set The matrix or dataframe of the discretized training dataset.
#' @param cont_train_set The matrix or dataframe of the continuous training dataset.
#' @keywords internal sample test train validation
#' @author Adrien Ehrhardt

# #' @examples
# #' # Create continuous training and test sets.
# #'
# #' # Recovering the cutpoints and discretizing the test
# #' # sets based exclusively on the training dataset.
# #' discretize_cutp(cont_test_set, disc_train_set, cont_train_set)

discretize_cutp <- function(cont_test_set, disc_train_set, cont_train_set) {
  d <- ncol(cont_train_set)

  data_validation <- matrix(0, nrow = nrow(cont_test_set), ncol = d)

  cutoff <- get_cutp(disc_train_set, cont_train_set)

  for (k in 1:d) {
    if (nlevels(as.factor(disc_train_set[, k])) > 1) {
      data_validation[, k] <- cut(cont_test_set[, k], cutoff[[k]], include.lowest = FALSE, labels = seq(1:(length(cutoff[[k]]) - 1)))
      data_validation[, k] <- factor(data_validation[, k])
    } else {
      data_validation[, k] <- rep(factor(1), nrow(cont_test_set))
    }
  }

  return(data_validation)
}
