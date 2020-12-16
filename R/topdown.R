#' Wrapper function for the 3 topdown functions from the discretization package.
#'
#' This function discretizes a training set using the user provided method(s) among the three topdown methods from the discretization package. Depending on the user providing a test and/or a validation set, the function returns the best discretization for logistic regression.
#' @param predictors The matrix array containing the numeric attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @param param List providing the methods to test (from 1, 2 and 3, default: list(1,2,3)).
#' @param proportions The list of the (2) proportions wanted for test and validation set. Only the first is used when there is only one of either test or validation that is set to TRUE. Produces an error when the sum is greater to one. Useless if both test and validation are set to FALSE. Default: list(0.2,0.2).
#' @keywords topdown discretization
#' @importFrom stats predict
#' @export
#' @author Adrien Ehrhardt
#' @seealso \code{glm}, \code{speedglm}, \code{discretization}
#' @details
#' This function discretizes a dataset containing continuous features \eqn{X} in a supervised way, i.e. knowing observations of a binomial random variable \eqn{Y} which we would like to predict based on the discretization of \eqn{X}.
#' To do so, the \code{Topdown} alorithms ...
#' In the context of Credit Scoring, a logistic regression is fitted between the ‘‘discretized'' features \eqn{E} and the response feature \eqn{Y}. As a consequence, the output of this function is the discretized features \eqn{E}, the logistic regression model of \eqn{E} on \eqn{Y} and the parameters used to get this fit.
#' @references
#' Enea, M. (2015), speedglm: Fitting Linear and Generalized Linear Models to Large Data Sets, \url{https://CRAN.R-project.org/package=speedglm}
#'
#' HyunJi Kim (2012). discretization: Data preprocessing, discretization for classification. R package version 1.0-1. \url{https://CRAN.R-project.org/package=discretization}
#'
#' Gonzalez-Abril, L., Cuberos, F. J., Velasco, F. and Ortega, J. A. (2009) Ameva: An autonomous discretization algorithm, \emph{Expert Systems with Applications}, \strong{36}, 5327–5332.
#'
#' Kurgan, L. A. and Cios, K. J. (2004). CAIM Discretization Algorithm, \emph{IEEE Transactions on knowledge and data engineering}, \strong{16}, 145–153.
#'
#' Tsai, C. J., Lee, C. I. and Yang, W. P. (2008). A discretization algorithm based on Class-Attribute Contingency Coefficient, \emph{Information Sciences}, \strong{178}, 714–731.
#' @examples
#' # Simulation of a discretized logit model
#' x <- matrix(runif(300), nrow = 100, ncol = 3)
#' cuts <- seq(0, 1, length.out = 4)
#' xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
#' theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
#' log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
#'   sapply(
#'     seq_along(xd[row_id, ]),
#'     function(element) theta[xd[row_id, element], element]
#'   )
#' })))
#' y <- stats::rbinom(100, 1, 1 / (1 + exp(-log_odd)))
#'
#' topdown_iter(x, y)
topdown_iter <- function(predictors, labels, test = F, validation = F, proportions = c(0.3, 0.3), criterion = "gini", param = list(1, 2, 3)) {
  if (!criterion %in% c("gini", "aic")) {
    stop(simpleError("Criterion must be either 'gini' or 'aic'"))
  }
  if (!length(labels) == length(predictors[, 1])) {
    stop(simpleError("Arguments labels and predictors must have same length."))
  }
  # Calcul des longueurs pour reutilisation ulterieure
  n <- length(labels)
  d <- length(predictors[1, ])
  # Initialisation des listes de modele
  logit <- list()
  disc <- list()
  criterlist <- list()

  # Decoupage de l'ensemble
  ensemble <- cut_dataset(n, proportions = proportions, test = test, validation = validation)

  data_train <- data.frame(predictors[ensemble[[1]], ], labels = labels[ensemble[[1]]])

  # topdown
  for (i in 1:length(param)) {
    disc[[i]] <- discretization::disc.Topdown(
      data = data_train[, sapply(
        data_train,
        function(col) !is.factor(col)
      )],
      method = param[[i]]
    )
    if (!(is_speedglm_installed() & is_speedglm_predict_installed())) {
      warning("Speedglm not installed, using glm instead (slower).", call. = FALSE)
      logit[[i]] <- fit_disc(disc[[i]], data_train, type = "glm")
    } else {
      logit[[i]] <- fit_disc(disc[[i]], data_train, type = "speedglm")
    }

    criterlist[[i]] <- calculate_criterlist(
      predictors,
      labels,
      validation,
      criterion,
      ensemble,
      disc[[i]],
      logit[[i]]
    )
  }

  best.disc <- list(logit[[which.min(criterlist)]], disc[[which.min(criterlist)]], which.min(criterlist))

  performance <- calculate_performance(test, validation, criterion, ensemble, predictors, labels, best.disc)

  return(output_disc(
    method.name = "topdown",
    predictors = predictors,
    labels = labels,
    test = test,
    validation = validation,
    criterion = criterion,
    param = param,
    ensemble = ensemble,
    best.disc = best.disc,
    performance = performance
  ))
}
