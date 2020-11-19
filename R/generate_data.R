#' Generate data following different missingness mechanisms
#'
#' This function performs generates
#' @param n The number of samples to return.
#' @param d The dimension of samples to return.
#' @param type The matrix of financed clients' labels
#' @return Dataframe containing features as x.1..d, labels as y.
#' @keywords reject inference réintégration scorecard credit scoring
#' @export
#' @author Adrien Ehrhardt
#' @details
#' This function generates data from a uniform(0,1) distribution, and generates
#' labels y according to a logistic regression on this data with random -1/1
#' parameter for each coordinate (MAR well-specified), the square of this data
#' (MAR misspecified), or this data and some additional feature (from U(0,1) as
#' well - MNAR).
#' @references
#' Ehrhardt, A., Biernacki, C., Vandewalle, V., Heinrich, P. and Beben, S. (2018), Reject Inference Methods in Credit Scoring: a rational review,
#' @examples
#' # We simulate data from financed clients
#' generate_data(n = 100, d = 3, type = "MAR well specified")
generate_data <- function(n = 100, d = 3, type = "MAR well specified") {
  if (!is.numeric(n) | n <= 10) {
    stop("Invalid value for n; must be numeric and >=10.")
  }
  if (!is.numeric(d) | d <= 0) {
    stop("Invalid value for d; must be numeric and >0.")
  }
  if (!is.character(type) | !(type %in% c(
    "MAR well specified",
    "MAR misspecified",
    "MNAR"
  ))) {
    stop("Invalid value for type; must be character and one of 'MAR well specified',
               'MAR misspecified' and 'MNAR'.")
  }
  x <- matrix(stats::runif(n * d), nrow = n, ncol = d)
  theta <- ifelse(stats::rbinom(d, 1, 0.5) == 0, -1, 1)
  switch(type,
    "MAR well specified" = {
      log_odd <- apply(x, 1, function(row) theta %*% row)
    },
    "MAR misspecified" = {
      log_odd <- apply(x^2, 1, function(row) theta %*% row)
    },
    "MNAR" = {
      x_mnar <- matrix(stats::runif(n * 1), nrow = n, ncol = 1)
      log_odd <- apply(cbind(x, x_mnar), 1, function(row) c(theta, 1) %*% row)
      y <- stats::rbinom(n, 1, 1 / (1 + exp(-log_odd)))
      return(data.frame(x = x, y = y, hidden = x_mnar))
    }
  )
  y <- stats::rbinom(n, 1, 1 / (1 + exp(-log_odd)))
  return(data.frame(x = x, y = y))
}
