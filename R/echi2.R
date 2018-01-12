#' Wrapper function for the extended Chi2 function from the discretization package.
#'
#' This function discretizes a training set using the extended Chi2 method and the user-provided parameters and chooses the best discretization scheme among them based on a user-provided criterion and eventually a test set.
#' @param predictors The matrix array containing the numeric attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @param param List providing the parameters to test (see ?discretization::extendChi2, default=list(alp = 0.5)).
#' @keywords extended Chi2, discretization
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
#' HyunJi Kim (2012). discretization: Data preprocessing, discretization for classification. R package version 1.0-1. \url{https://CRAN.R-project.org/package=discretization}
#' Liu, H. and Setiono, R. (1995). Chi2: Feature selection and discretization of numeric attributes, \emph{Tools with Artificial Intelligence}, 388–391.
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
#' echi2_iter(x,y)


echi2_iter <- function(predictors,labels,test=TRUE,validation=TRUE,criterion='gini',param=list(alp = 0.5)) {


     if (criterion %in% c('gini','aic')) {
          if (length(labels)==length(predictors[,1])) {
               # Calcul des longueurs pour reutilisation ulterieure
               n = length(labels)
               d = length(predictors[1,])
               # Initialisation des listes de modele
               logit = list()
               disc = list()
               ginidisc = list()
               # Initialisation du critere de performance
               if (criterion=="gini") ginidisc=list() else aicdisc=list()

               # Decoupage de l'ensemble
               ensemble <- cut_dataset(n,test=test,validation=validation)

               data_train = as.data.frame(cbind(predictors[ensemble[[1]],],labels = labels[ensemble[[1]]]))

               # eChi2
               for (i in 1:length(param)) {
                    disc[[i]] = discretization::extendChi2(data = data_train, alp = param[[i]])
                    if (!requireNamespace("speedglm", quietly = TRUE)) {
                         warning("Speedglm not installed, using glm instead (slower).",call. = FALSE)
                         logit[[i]] = stats::glm(labels ~ ., family = stats::binomial(link = "logit"), data = Filter(function(x)(length(unique(x))>1),as.data.frame(sapply(disc[[i]]$Disc.data,as.factor))))

                    } else {
                         logit[[i]] = speedglm::speedglm(labels ~ ., family = stats::binomial(link = "logit"), data = Filter(function(x)(length(unique(x))>1),as.data.frame(sapply(disc[[i]]$Disc.data,as.factor))))
                         # methods::setIs(class(logit[[i]]), "glmORlogicalORspeedglm")
                    }

                    if (test==TRUE) {
                         data_test = as.data.frame(sapply(as.data.frame(discretize_cutp(predictors[ensemble[[2]],],disc[[i]][["Disc.data"]],predictors[ensemble[[1]],])),as.factor))
                         if (criterion=='gini') ginidisc[[i]] = glmdisc::normalizedGini(labels[ensemble[[2]]],predict(logit[[i]],data_test,type="response")) else aicdisc[[i]] = logit[[i]]$aic
                    } else {
                         if (criterion=='gini') ginidisc[[i]] = glmdisc::normalizedGini(labels[ensemble[[1]]],logit[[i]]$fitted.values) else aicdisc[[i]] = logit[[i]]$aic
                    }
               }


               if (test==TRUE) {
                    if (criterion=="gini") {
                         best.disc = list(logit[[which.min(ginidisc)]],disc[[which.min(ginidisc)]],which.min(ginidisc))
                         if (validation==TRUE) {
                              data_validation = as.data.frame(sapply(as.data.frame(discretize_cutp(predictors[ensemble[[3]],],disc[[i]][["Disc.data"]],predictors[ensemble[[1]],])),as.factor))
                              performance = glmdisc::normalizedGini(labels[ensemble[[3]]],predict(best.disc[[1]],data_validation,type="response"))
                         } else performance = glmdisc::normalizedGini(labels[ensemble[[2]]],predict(best.disc[[1]],data_test,type="response"))
                    } else {
                         best.disc = list(logit[[which.min(aicdisc)]],disc[[which.min(aicdisc)]],which.min(aicdisc))
                         if (validation==TRUE) performance = 0 else performance = 0
                    }
               } else {
                    if (criterion=="gini") {
                         best.disc = list(logit[[which.min(ginidisc)]],disc[[which.min(ginidisc)]],which.min(ginidisc))
                         if (validation==TRUE) {
                              data_validation = as.data.frame(sapply(as.data.frame(discretize_cutp(predictors[ensemble[[3]],],disc[[i]][["Disc.data"]],predictors[ensemble[[1]],])),as.factor))
                              performance = glmdisc::normalizedGini(labels[ensemble[[3]]],predict(best.disc[[1]],data_validation,type="response"))
                         } else performance = glmdisc::normalizedGini(labels[ensemble[[1]]],best.disc[[1]]$fitted.values)
                    } else {
                         best.disc = list(logit[[which.min(aicdisc)]],disc[[which.min(aicdisc)]],which.min(aicdisc))
                         if (validation==TRUE) performance = 0 else performance = best.disc[[1]]$aic
                    }
               }

               if (test==TRUE) {
                    return(methods::new(Class = "discretization", method.name = "echi2", parameters = list(predictors,test,validation,criterion,param,ensemble), best.disc = best.disc, performance = list(performance), disc.data = data.frame(cbind(discretize_cutp(predictors[ensemble[[3]],],best.disc[[2]][["Disc.data"]],predictors[ensemble[[1]],]),labels[ensemble[[3]]])), cont.data = data.frame(cbind(predictors[ensemble[[3]],],labels[ensemble[[3]]]))))
               } else if (validation==TRUE) {
                    return(methods::new(Class = "discretization", method.name = "echi2", parameters = list(predictors,test,validation,criterion,param,ensemble), best.disc = best.disc, performance = list(performance), disc.data = data.frame(cbind(discretize_cutp(predictors[ensemble[[2]],],best.disc[[2]][["Disc.data"]],predictors[ensemble[[1]],]),labels[ensemble[[2]]])), cont.data = data.frame(cbind(predictors[ensemble[[2]],],labels[ensemble[[2]]]))))
               } else {
                    return(methods::new(Class = "discretization", method.name = "echi2", parameters = list(predictors,test,validation,criterion,param,ensemble), best.disc = best.disc, performance = list(performance), disc.data = data.frame(cbind(discretize_cutp(predictors[ensemble[[1]],],best.disc[[2]][["Disc.data"]],predictors[ensemble[[1]],]),labels[ensemble[[1]]])), cont.data = data.frame(cbind(predictors[ensemble[[1]],],labels[ensemble[[1]]]))))
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
