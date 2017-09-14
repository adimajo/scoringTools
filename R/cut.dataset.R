#' Randoming and sampling a dataset by its row numbers
#'
#' This function chooses the rows of a dataset to be included in the train, test (optional) and validation (optional) datasets according to proportions specified by the user.
#' @param n The total number of rows to sample from.
#' @param test TRUE if a test dataset is wanted, FALSE otherwise (default=TRUE).
#' @param validation TRUE if a test dataset is wanted, FALSE otherwise (default=TRUE).
#' @param proportions The list of the (2) proportions wanted for test and validation set. Only the first is used when there is only one of either test or validation that is set to TRUE. Produces an error when the sum is greater to one. Useless if both test and validation are set to FALSE. Default: list(0.2,0.2).
#' @param seed The seed for the random number generator (optional).
#' @keywords sample, test, train, validation

# #' @examples

# #' # We randomly separate 30 observations in 40\% of training, 30\% of test and 30\% of validation.
# #' cut_dataset(n=30,test=TRUE,
# #' validation=TRUE,proportions=c(0.3,0.3),seed=1)


cut_dataset <- function(n,test=TRUE,validation=TRUE,proportions=c(0.2,0.2),seed=1) {
     set.seed(seed)
     if (test==TRUE) {
          if (validation==TRUE) {
               ind_train = sample.int(n,n)
               ind_test = ind_train[1:floor(proportions[1]*n)]
               ind_validation = ind_train[(floor(proportions[1]*n)+1):floor((proportions[1]+proportions[2])*n)]
               ind_train = ind_train[(floor((proportions[1]+proportions[2])*n)+1):n]
               return(list(ind_train,ind_test,ind_validation))
          } else {
               ind_train = sample.int(n,n)
               ind_test = ind_train[1:floor(proportions[1]*n)]
               ind_train = ind_train[(floor(proportions[1]*n)+1):n]
               return(list(ind_train,ind_test))
          }
     } else {
          if (validation==TRUE) {
               ind_train = sample.int(n,n)
               ind_validation = ind_train[1:floor(proportions[1]*n)]
               ind_train = ind_train[(floor(proportions[1]*n)+1):n]
               return(list(ind_train,ind_validation))
          } else {
               ind_train = sample.int(n,n)
               return(list(ind_train))
          }
     }
}
