#' Wrapper function for the extended Chi2 function from the discretization package.
#'
#' This function discretizes a training set using the extended Chi2 method and the user-provided parameters and chooses the best discretization scheme among them based on a user-provided criterion and eventually a test set.
#' @param predictors The matrix array containing the numeric attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @param param List providing the parameters to test (see ?discretization::extendChi2, default=list(alp = 0.5)).
#' @param proportions The list of the (2) proportions wanted for test and validation set. Only the first is used when there is only one of either test or validation that is set to TRUE. Produces an error when the sum is greater to one. Useless if both test and validation are set to FALSE. Default: list(0.2,0.2).
#' @keywords extended Chi2 discretization
#' @importFrom stats predict
#' @author Adrien Ehrhardt
#' @seealso \code{glm}, \code{speedglm}, \code{discretization}
#' @details
#' This function discretizes a dataset containing continuous features \eqn{X} in a supervised way, i.e. knowing observations of a binomial random variable \eqn{Y} which we would like to predict based on the discretization of \eqn{X}.
#' To do so, the \code{ExtendedChi2} alorithm starts by putting each unique values of \eqn{X} in a separate value of the ‘‘discretized'' categorical feature \eqn{E}. It then tests if two adjacent values of \eqn{E} are significantly different using the \eqn{\chi^2}-test.
#' In the context of Credit Scoring, a logistic regression is fitted between the ‘‘discretized'' features \eqn{E} and the response feature \eqn{Y}. As a consequence, the output of this function is the discretized features \eqn{E}, the logistic regression model of \eqn{E} on \eqn{Y} and the parameters used to get this fit.
#' @export
#' @references
#' Enea, M. (2015), speedglm: Fitting Linear and Generalized Linear Models to Large Data Sets, \url{https://CRAN.R-project.org/package=speedglm}
#'
#' HyunJi Kim (2012). discretization: Data preprocessing, discretization for classification. R package version 1.0-1. \url{https://CRAN.R-project.org/package=discretization}
#'
#' Liu, H. and Setiono, R. (1995). Chi2: Feature selection and discretization of numeric attributes, \emph{Tools with Artificial Intelligence}, 388–391.
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
#' echi2_iter(x, y)
echi2_iter <- function(predictors, labels, test = FALSE, validation = FALSE, proportions = c(0.3, 0.3), criterion = "gini", param = list(alp = 0.5)) {
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

  # eChi2
  for (i in 1:length(param)) {
    disc[[i]] <- discretization::extendChi2(data = data_train, alp = param[[i]])
    if (!(is_speedglm_installed() & is_speedglm_predict_installed())) {
      warning("Speedglm not installed, using glm instead (slower).", call. = FALSE)
      logit[[i]] <- stats::glm(formula = stats::formula("labels ~ ."), family = stats::binomial(link = "logit"), data = Filter(function(x) (length(unique(x)) > 1), data.frame(sapply(disc[[i]]$Disc.data, as.factor), stringsAsFactors = TRUE)), weights = NULL)
    } else {
      logit[[i]] <- speedglm::speedglm(formula = stats::formula("labels ~ ."), family = stats::binomial(link = "logit"), data = Filter(function(x) (length(unique(x)) > 1), data.frame(sapply(disc[[i]]$Disc.data, as.factor), stringsAsFactors = TRUE)), fitted = TRUE, weights = NULL)
    }

    if (validation == TRUE) {
      data_validation <- data.frame(sapply(data.frame(discretize_cutp(predictors[ensemble[[2]], ], disc[[i]][["Disc.data"]], predictors[ensemble[[1]], ])), as.factor), stringsAsFactors = TRUE)
      if (criterion == "gini") {
        criterlist[[i]] <- normalizedGini(labels[ensemble[[2]]], predict(logit[[i]], data_validation, type = "response"))
      } else {
        criterlist[[i]] <- logit[[i]]$aic
      }
    } else {
      if (criterion == "gini") {
        if (!(is_speedglm_installed() & is_speedglm_predict_installed())) {
          criterlist[[i]] <- normalizedGini(labels[ensemble[[1]]], logit[[i]]$fitted.values)
        } else {
          criterlist[[i]] <- normalizedGini(labels[ensemble[[1]]], logit[[i]]$linear.predictors)
        }
      } else {
        criterlist[[i]] <- logit[[i]]$aic
      }
    }
  }

  best.disc <- list(logit[[which.min(criterlist)]], disc[[which.min(criterlist)]], which.min(criterlist))

  if (test == TRUE) {
    if (criterion == "gini") {
      if (validation == TRUE) {
        data_test <- data.frame(sapply(data.frame(discretize_cutp(predictors[ensemble[[3]], ], disc[[i]][["Disc.data"]], predictors[ensemble[[1]], ])), as.factor), stringsAsFactors = TRUE)
        performance <- normalizedGini(labels[ensemble[[3]]], predict(best.disc[[1]], data_test, type = "response"))
      } else {
        data_test <- data.frame(sapply(data.frame(discretize_cutp(predictors[ensemble[[2]], ], disc[[i]][["Disc.data"]], predictors[ensemble[[1]], ])), as.factor), stringsAsFactors = TRUE)
        performance <- normalizedGini(labels[ensemble[[2]]], predict(best.disc[[1]], data_test, type = "response"))
      }
    } else {
      if (validation == TRUE) performance <- 0 else performance <- 0
    }
  } else {
    if (criterion == "gini") {
      if (validation == TRUE) {
        data_validation <- data.frame(sapply(data.frame(discretize_cutp(predictors[ensemble[[2]], ], disc[[i]][["Disc.data"]], predictors[ensemble[[1]], ])), as.factor), stringsAsFactors = TRUE)
        performance <- normalizedGini(labels[ensemble[[2]]], predict(best.disc[[1]], data_validation, type = "response"))
      } else {
        if (!(is_speedglm_installed() & is_speedglm_predict_installed())) {
          performance <- normalizedGini(labels[ensemble[[1]]], best.disc[[1]]$fitted.values)
        } else {
          performance <- normalizedGini(labels[ensemble[[1]]], best.disc[[1]]$linear.predictors)
        }
      }
    } else {
      if (validation == TRUE) performance <- 0 else performance <- best.disc[[1]]$aic
    }
  }

  if (test == TRUE) {
    return(methods::new(Class = "discretization", method.name = "echi2", parameters = list(predictors, test, validation, criterion, param, ensemble), best.disc = best.disc, performance = list(performance), disc.data = data.frame(discretize_cutp(predictors[ensemble[[3]], ], best.disc[[2]][["Disc.data"]], predictors[ensemble[[1]], ]), labels = labels[ensemble[[3]]], stringsAsFactors = TRUE), cont.data = data.frame(predictors[ensemble[[3]], ], labels = labels[ensemble[[3]]])))
  } else if (validation == TRUE) {
    return(methods::new(Class = "discretization", method.name = "echi2", parameters = list(predictors, test, validation, criterion, param, ensemble), best.disc = best.disc, performance = list(performance), disc.data = data.frame(discretize_cutp(predictors[ensemble[[2]], ], best.disc[[2]][["Disc.data"]], predictors[ensemble[[1]], ]), labels = labels[ensemble[[2]]], stringsAsFactors = TRUE), cont.data = data.frame(predictors[ensemble[[2]], ], labels = labels[ensemble[[2]]])))
  } else {
    return(methods::new(Class = "discretization", method.name = "echi2", parameters = list(predictors, test, validation, criterion, param, ensemble), best.disc = best.disc, performance = list(performance), disc.data = data.frame(discretize_cutp(predictors[ensemble[[1]], ], best.disc[[2]][["Disc.data"]], predictors[ensemble[[1]], ]), labels = labels[ensemble[[1]]], stringsAsFactors = TRUE), cont.data = data.frame(predictors[ensemble[[1]], ], labels = labels[ensemble[[1]]])))
  }
}
