#' Discretization of a test dataset when provided with an already discretized training set.
#'
#' This function discretizes a dataset when provided with both continuous and discretized versions of a training dataset.
#' @param cont_test_set The matrix or dataframe of the dataset to discretize.
#' @param disc_train_set The matrix or dataframe of the discretized training dataset.
#' @param cont_train_set The matrix or dataframe of the continuous training dataset.
#' @keywords sample, test, train, validation

# #' @examples
# #' # Create continuous training and test sets.
# #'
# #' # Recovering the cutpoints and discretizing the test
# #' # sets based exclusively on the training dataset.
# #' discretize_cutp(cont_test_set,disc_train_set,cont_train_set)

discretize_cutp <- function(cont_test_set,disc_train_set,cont_train_set) {

     d = ncol(cont_train_set)

     data_validation <- matrix(0,nrow=nrow(cont_test_set),ncol=d)

     for (k in 1:d) {
          if (nlevels(as.factor(disc_train_set[,k])) > 1) {
               pairs <- data.frame(modalites = as.factor(disc_train_set[,k]),cont = cont_train_set[,k])

               min_max <- sqldf::sqldf('
                                          select distinct modalites, min(cont), max(cont)
                                          from pairs
                                          group by modalites')

               min_cutoff <- min_max[2:nrow(min_max),"min(cont)",drop = FALSE]
               min_cutoff <- min_cutoff[order(min_cutoff),]
               max_cutoff <- min_max[1:(nrow(min_max)-1),"max(cont)",drop = FALSE]
               max_cutoff <- max_cutoff[order(max_cutoff),]
               cutoff <- cbind(min_cutoff,max_cutoff)
               cutoff <- rowMeans(cutoff)
               cutoff[length(cutoff)+1] <- -Inf
               cutoff[length(cutoff)+1] <- Inf

               data_validation[,k] <- cut(cont_test_set[,k],cutoff, include.lowest = FALSE, labels = seq(1:(length(cutoff)-1)))
               data_validation[,k] <- factor(data_validation[,k])
          } else {
               data_validation[,k] <- rep(factor(1),nrow(cont_test_set))
          }
     }

     return(data_validation)
}
