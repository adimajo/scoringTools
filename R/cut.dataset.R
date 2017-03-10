#' Randoming and sampling a dataset by its row numbers
#'
#' This function chooses the rows of a dataset to be included in the train, test (optional) and validation (optional) datasets according to proportions specified by the user.
#' @param n The total number of rows to sample from.
#' @param test TRUE if a test dataset is wanted, FALSE otherwise (default=TRUE).
#' @param validation TRUE if a test dataset is wanted, FALSE otherwise (default=TRUE).
#' @param proportions The list of the proportions.
#' @param seed The seed for the random number generator (optional).
#' @keywords sample, test, train, validation
#' @export
#' @examples
#' cut.dataset()


cut.dataset <- function(n,test=TRUE,validation=TRUE,proportions=c(),seed=1) {
     set.seed(seed)
     if (test==TRUE) {
          if (validation==TRUE) {
               ind_train = sample.int(n,n)
               ind_test = ind_train[1:floor(0.2*n)]
               ind_validation = ind_train[(floor(0.2*n)+1):floor(0.4*n)]
               ind_train = ind_train[(floor(0.4*n)+1):n]
               return(list(ind_train,ind_test,ind_validation))
          } else {
               ind_train = sample.int(n,n)
               ind_test = ind_train[1:floor(0.3*n)]
               ind_train = ind_train[(floor(0.3*n)+1):n]
               return(list(ind_train,ind_test))
          }
     } else {
          if (validation==TRUE) {
               ind_train = sample.int(n,n)
               ind_validation = ind_train[1:floor(0.3*n)]
               ind_train = ind_train[(floor(0.3*n)+1):n]
               return(list(ind_train,ind_validation))
          } else {
               ind_train = sample.int(n,n)
               return(list(ind_train))
          }
     }
}
