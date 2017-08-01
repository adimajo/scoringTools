#' Wrapper function for the mdlp function from the discretization package.
#'
#' This function discretizes a training set using the Minimum Description Length Principle method and the user-provided parameters.
#' @param predictors The matrix array containing the numeric attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @keywords mdlp, discretization
#' @importFrom stats predict
#' @export
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


mdlp_iter <- function(predictors,labels,test=TRUE,validation=TRUE,criterion='gini') {
     if (criterion %in% c('gini','aic')) {
          if (length(labels)==length(predictors[,1])) {
               # Calcul des longueurs pour reutilisation ulterieure
               n = length(labels)
               d = length(predictors[1,])

               # Initialisation du critere de performance
               if (criterion=="gini") ginidisc=list() else aicdisc=list()

               # Decoupage de l'ensemble
               ensemble <- cut_dataset(n,test=test,validation=validation)

               data_train = as.data.frame(cbind(predictors[ensemble[[1]],],labels = labels[ensemble[[1]]]))

               # mdlp
               disc = discretization::mdlp(data = data_train)
               if (!requireNamespace("speedglm", quietly = TRUE)) {
                    warning("Speedglm not installed, using glm instead (slower).",call. = FALSE)
                    logit = stats::glm(labels ~ ., family = stats::binomial(link = "logit"), data = Filter(function(x)(length(unique(x))>1),as.data.frame(sapply(disc$Disc.data,as.factor))))

               } else {
                    logit = speedglm::speedglm(labels ~ ., family = stats::binomial(link = "logit"), data = Filter(function(x)(length(unique(x))>1),as.data.frame(sapply(disc$Disc.data,as.factor))))
               }

               if (test==TRUE) {
                    data_test = as.data.frame(sapply(as.data.frame(discretize_cutp(predictors[ensemble[[2]],],disc[["Disc.data"]],predictors[ensemble[[1]],])),as.factor))
                    if (criterion=='gini') ginidisc = normalizedGini(labels[ensemble[[2]]],predict(logit,data_test,type="response")) else aicdisc = logit$aic
               } else {
                    if (criterion=='gini') ginidisc = normalizedGini(labels[ensemble[[1]]],logit$fitted.values) else aicdisc = logit$aic
               }



               # setClass("mdlp_disc", representation(method.name = "character", parameters = "list", best.disc = "list", performance = "numeric"))

               if (test==TRUE) {
                    if (criterion=="gini") {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) {
                              data_validation = as.data.frame(sapply(as.data.frame(discretize_cutp(predictors[ensemble[[3]],],disc[["Disc.data"]],predictors[ensemble[[1]],])),as.factor))
                              performance = normalizedGini(labels[ensemble[[3]]],predict(best.disc[[1]],data_validation,type="response"))
                         } else performance = normalizedGini(labels[ensemble[[2]]],predict(best.disc[[1]],data_test,type="response"))
                    } else {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) performance = 0 else performance =  best.disc[[1]]$aic
                    }
               } else {
                    if (criterion=="gini") {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) {
                              data_validation = as.data.frame(sapply(as.data.frame(discretize_cutp(predictors[ensemble[[3]],],disc[["Disc.data"]],predictors[ensemble[[1]],])),as.factor))
                              performance = normalizedGini(labels[ensemble[[3]]],predict(best.disc[[1]],data_validation,type="response"))
                         } else performance = normalizedGini(labels[ensemble[[1]]],best.disc[[1]]$fitted.values)
                    } else {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) performance = 0 else performance = best.disc[[1]]$aic
                    }

               }

               # return(new(Class = "mdlp_disc", method.name = "mdlp", parameters = list(test,validation,criterion), best.disc = best.disc, performance = performance))
               return(list(method.name = "mdlp", parameters = list(test,validation,criterion), best.disc = best.disc, performance = performance))

          }
          else {
               print("Arguments labels and predictors must have same length.")
          }
     }
     else {
          print("Criterion must be either 'gini' or 'aic'")
     }
}
