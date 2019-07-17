#' Wrapper function for the mdlp function from the discretization package.
#'
#' This function discretizes a training set using the Minimum Description Length Principle method and the user-provided parameters.
#' @param predictors The matrix array containing the numeric attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @param proportions The list of the (2) proportions wanted for test and validation set. Only the first is used when there is only one of either test or validation that is set to TRUE. Produces an error when the sum is greater to one. Useless if both test and validation are set to FALSE. Default: list(0.2,0.2).
#' @keywords mdlp, discretization
#' @importFrom stats predict
#' @export
#' @author Adrien Ehrhardt
#' @seealso \code{glm}, \code{speedglm}, \code{discretization}
#' @details
#' This function discretizes a dataset containing continuous features \eqn{X} in a supervised way, i.e. knowing observations of a binomial random variable \eqn{Y} which we would like to predict based on the discretization of \eqn{X}.
#' To do so, the \code{MDLP} alorithm dichotomizes \eqn{X} and puts the subsequent two values in the ‘‘discretized'' categorical feature \eqn{E}. It chooses the cut-off point so as to minimize the resulting entropy and goes on in the subsequent two sub-spaces it just created.
#' In the context of Credit Scoring, a logistic regression is fitted between the ‘‘discretized'' features \eqn{E} and the response feature \eqn{Y}. As a consequence, the output of this function is the discretized features \eqn{E}, the logistic regression model of \eqn{E} on \eqn{Y} and the parameters used to get this fit.
#' @references
#' Enea, M. (2015), speedglm: Fitting Linear and Generalized Linear Models to Large Data Sets, \url{https://CRAN.R-project.org/package=speedglm}
#' HyunJi Kim (2012). discretization: Data preprocessing, discretization for classification. R package version 1.0-1. \url{https://CRAN.R-project.org/package=discretization}
#' Fayyad, U. M. and Irani, K. B.(1993). Multi-interval discretization of continuous-valued attributes for classification learning, \emph{Artificial intelligence}, \strong{13}, 1022–1027.
#' @examples
#' # Simulation of a discretized logit model
#' set.seed(1)
#' x = matrix(runif(300), nrow = 100, ncol = 3)
#' cuts = seq(0,1,length.out= 4)
#' xd = apply(x,2, function(col) as.numeric(cut(col,cuts)))
#' theta = t(matrix(c(0,0,0,2,2,2,-2,-2,-2),ncol=3,nrow=3))
#' log_odd = rowSums(t(sapply(seq_along(xd[,1]), function(row_id) sapply(seq_along(xd[row_id,]),
#' function(element) theta[xd[row_id,element],element]))))
#' y = stats::rbinom(100,1,1/(1+exp(-log_odd)))
#'
#' mdlp_iter(x,y)


mdlp_iter <- function(predictors,labels,test=F,validation=F,proportions = c(0.3,0.3),criterion='gini') {
     if (criterion %in% c('gini','aic')) {
          if (length(labels)==length(predictors[,1])) {
               # Calcul des longueurs pour reutilisation ulterieure
               n = length(labels)
               d = length(predictors[1,])

               # Initialisation du critere de performance
               if (criterion=="gini") ginidisc=list() else aicdisc=list()

               # Decoupage de l'ensemble
               ensemble <- cut_dataset(n,proportions=proportions,test=test,validation=validation)

               data_train = as.data.frame(cbind(predictors[ensemble[[1]],],labels = labels[ensemble[[1]]]))

               # mdlp
               disc = discretization::mdlp(data = data_train)
               if (!requireNamespace("speedglm", quietly = TRUE)) {
                    warning("Speedglm not installed, using glm instead (slower).",call. = FALSE)
                    logit = stats::glm(labels ~ ., family = stats::binomial(link = "logit"), data = Filter(function(x)(length(unique(x))>1),as.data.frame(sapply(disc$Disc.data,as.factor))))

               } else {
                    logit = speedglm::speedglm(labels ~ ., family = stats::binomial(link = "logit"), data = Filter(function(x)(length(unique(x))>1),as.data.frame(sapply(disc$Disc.data,as.factor))), fitted = TRUE)
                    # methods::setIs(class(logit), "glmORlogicalORspeedglm")
               }

               if (validation==TRUE) {
                    data_test = as.data.frame(sapply(as.data.frame(discretize_cutp(predictors[ensemble[[2]],],disc[["Disc.data"]],predictors[ensemble[[1]],])),as.factor))
                    if (criterion=='gini') {
                         ginidisc = glmdisc::normalizedGini(labels[ensemble[[2]]],predict(logit,data_test,type="response"))
                    } else {
                         aicdisc = logit$aic
                    }
               } else {
                    if (criterion=='gini') {
                         if (!requireNamespace("speedglm", quietly = TRUE)) {
                              ginidisc = glmdisc::normalizedGini(labels[ensemble[[1]]],logit$fitted.values)
                         } else {
                              ginidisc = glmdisc::normalizedGini(labels[ensemble[[1]]],logit$linear.predictors)
                         }
                    } else aicdisc = logit$aic
               }


               if (test==TRUE) {
                    if (criterion=="gini") {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) {
                              data_validation = as.data.frame(sapply(as.data.frame(discretize_cutp(predictors[ensemble[[3]],],disc[["Disc.data"]],predictors[ensemble[[1]],])),as.factor))
                              performance = glmdisc::normalizedGini(labels[ensemble[[3]]],predict(best.disc[[1]],data_validation,type="response"))
                         } else performance = glmdisc::normalizedGini(labels[ensemble[[2]]],predict(best.disc[[1]],data_test,type="response"))
                    } else {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) performance = 0 else performance =  best.disc[[1]]$aic
                    }
               } else {
                    if (criterion=="gini") {
                         best.disc = list(logit,disc,1)
                         if (validation==TRUE) {
                              data_validation = as.data.frame(sapply(as.data.frame(discretize_cutp(predictors[ensemble[[3]],],disc[["Disc.data"]],predictors[ensemble[[1]],])),as.factor))
                              performance = glmdisc::normalizedGini(labels[ensemble[[3]]],predict(best.disc[[1]],data_validation,type="response"))
                         } else {
                              if (!requireNamespace("speedglm", quietly = TRUE)) {
                                   performance = glmdisc::normalizedGini(labels[ensemble[[1]]],best.disc[[1]]$fitted.values)
                              } else {
                                   performance = glmdisc::normalizedGini(labels[ensemble[[1]]], best.disc[[1]]$linear.predictors)
                              }
                         }
                    } else {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) performance = 0 else performance = best.disc[[1]]$aic
                    }

               }


               if (test==TRUE) {
                    return(methods::new(Class = "discretization", method.name = "mdlp", parameters = list(predictors,test,validation,criterion,list(),ensemble), best.disc = best.disc, performance = list(performance), disc.data = data.frame(cbind(discretize_cutp(predictors[ensemble[[3]],],best.disc[[2]][["Disc.data"]],predictors[ensemble[[1]],]),labels[ensemble[[3]]])), cont.data = data.frame(cbind(predictors[ensemble[[3]],],labels[ensemble[[3]]]))))
               } else if (validation==TRUE) {
                    return(methods::new(Class = "discretization", method.name = "mdlp", parameters = list(predictors,test,validation,criterion,list(),ensemble), best.disc = best.disc, performance = list(performance), disc.data = data.frame(cbind(discretize_cutp(predictors[ensemble[[2]],],best.disc[[2]][["Disc.data"]],predictors[ensemble[[1]],]),labels[ensemble[[2]]])), cont.data = data.frame(cbind(predictors[ensemble[[2]],],labels[ensemble[[2]]]))))
               } else {
                    return(methods::new(Class = "discretization", method.name = "mdlp", parameters = list(predictors,test,validation,criterion,list(),ensemble), best.disc = best.disc, performance = list(performance), disc.data = data.frame(cbind(discretize_cutp(predictors[ensemble[[1]],],best.disc[[2]][["Disc.data"]],predictors[ensemble[[1]],]),labels[ensemble[[1]]])), cont.data = data.frame(cbind(predictors[ensemble[[1]],],labels[ensemble[[1]]]))))
               }


          }
          else {
               print("Arguments labels and predictors must have same length.")
          }
     }
     else {
          print("Criterion must be either 'gini' or 'aic'")
     }
}
