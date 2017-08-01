#' Compressing glm results
#'
#' This function suppresses attributes of glm models that depend on the size of the input dataset.
#' @param modele The glm model to compress.
#' @keywords glm, size
# #' @examples
# #' reduireGLM()

reduireGLM <- function(modele) {
     modele$y = c()
     modele$model = c()

     modele$residuals = c()
     modele$fitted.values = c()
     modele$effects = c()
     modele$qr$qr = c()
     modele$linear.predictors = c()
     modele$weights = c()
     modele$prior.weights = c()
     modele$data = c()

     modele$family$variance = c()
     modele$family$dev.resids = c()
     modele$family$aic = c()
     modele$family$validmu = c()
     modele$family$simulate = c()
     attr(modele$terms,".Environment") = c()
     attr(modele$formula,".Environment") = c()

     return(modele)
}
