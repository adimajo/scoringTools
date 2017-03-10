#' Calculating the normalized Gini index
#'
#' This function calculates the Gini index of a classification rule outputting probabilities.
#' @param actual The numeric binary vector of the actual labels observed.
#' @param predicted The vector of the probabilities predicted by the classification rule.
#' @keywords gini, index
#' @export
#' @examples
#' normalizedGini()


normalizedGini <- function(actual, predicted) {
     Gini <- function(a, p) {
          if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
          temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
          temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
          population.delta <- 1 / length(a)
          total.losses <- sum(a)
          null.losses <- rep(population.delta, length(a)) # Hopefully is similar to accumulatedPopulationPercentageSum
          accum.losses <- temp.df$actual / total.losses # Hopefully is similar to accumulatedLossPercentageSum
          gini.sum <- cumsum(accum.losses - null.losses) # Not sure if this is having the same effect or not
          sum(gini.sum) / length(a)
     }
     Gini(actual,predicted) / Gini(actual,actual)
}
