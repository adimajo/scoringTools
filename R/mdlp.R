#' Wrapper function for the mdlp function from the discretization package.
#'
#' This function discretizes a training set using the Minimum Description Length Principle method and the user-provided parameters.
#' @param predictors The matrix array containing the numeric attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @keywords mdlp, discretization
#' @export
#' @examples
#' mdlp_iter()


mdlp_iter <- function(predictors,labels,test=TRUE,validation=TRUE,criterion='gini') {
     if (criterion %in% c('gini','aic')) {
          if (length(labels)==length(predictors[,1])) {
               # Calcul des longueurs pour reutilisation ulterieure
               n = length(labels)
               d = length(predictors[1,])
               # Initialisation des listes de modele
               logit = list()
               disc = list()
               # Initialisation du critere de performance
               if (criterion=="gini") ginidisc=list() else aicdisc=list()

               # Decoupage de l'ensemble
               if (test==TRUE) {
                    if (validation==TRUE) {
                         ind_train = sample.int(n,n)
                         ind_test = ind_train[1:floor(0.2*n)]
                         ind_validation = ind_train[(floor(0.2*n)+1):floor(0.4*n)]
                         ind_train = ind_train[(floor(0.4*n)+1):n]
                    } else {
                         ind_train = sample.int(n,n)
                         ind_test = ind_train[1:floor(0.3*n)]
                         ind_train = ind_train[(floor(0.3*n)+1):n]
                    }
               } else {
                    if (validation==TRUE) {
                         ind_train = sample.int(n,n)
                         ind_validation = ind_train[1:floor(0.3*n)]
                         ind_train = ind_train[(floor(0.3*n)+1):n]
                    } else {
                         # ind_train = sample.int(n,n)
                         ind_train = seq(1:n)
                    }
               }

               data_train = as.data.frame(cbind(predictors[ind_train,],labels = labels[ind_train]))
               if (test==TRUE) data_test = as.data.frame(cbind(predictors[ind_test,],labels = labels[ind_test]))

               # mdlp
               disc = discretization::mdlp(data = data_train)
               logit = speedglm::speedglm(labels ~ ., family=stats::binomial(link="logit"), data = Filter(function(x)(length(unique(x))>1), as.data.frame(sapply(disc$Disc.data,as.factor))))

               if (test==TRUE) {
                    for (j in 1:d) {
                         if (!is.character(disc[["cutp"]][[j]])) {
                              cutoffvalueschi2 <- disc[["cutp"]][[j]]
                              cutoffvalueschi2[length(cutoffvalueschi2)+1] <- -Inf
                              cutoffvalueschi2[length(cutoffvalueschi2)+1] <- Inf

                              data_test[,j] <- cut(data_test[,j],cutoffvalueschi2, include.lowest = FALSE, labels = seq(1:(length(cutoffvalueschi2)-1)))
                              data_test[,j] <- factor(data_test[,j])

                         }
                    }
                    if (criterion=='gini') ginidisc = normalizedGini(labels[ind_test],speedglm:::predict.speedglm(logit,data_test,type="response")) else aicdisc = logit$aic
               } else {
                    if (criterion=='gini') ginidisc = normalizedGini(labels[ind_train],logit$fitted.values) else aicdisc = logit$aic
               }



               setClass("mdlp_disc", representation(method.name = "character", parameters = "list", best.disc = "list", performance = "numeric"))

               if (test==TRUE) {
                    if (criterion=="gini") {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) {
                              data_validation = as.data.frame(cbind(predictors[ind_validation,],labels = labels[ind_validation]))
                              for (j in 1:d) {
                                   if (!is.character(disc[["cutp"]][[j]])) {
                                        cutoffvalueschi2 <- disc[[which.min(ginidisc)]][["cutp"]][[j]]
                                        cutoffvalueschi2[length(cutoffvalueschi2)+1] <- -Inf
                                        cutoffvalueschi2[length(cutoffvalueschi2)+1] <- Inf

                                        data_validation[,j] <- cut(data_validation[,j],cutoffvalueschi2, include.lowest = FALSE, labels = seq(1:(length(cutoffvalueschi2)-1)))
                                        data_validation[,j] <- factor(data_validation[,j])
                                   }
                              }
                              performance = normalizedGini(labels[ind_validation],speedglm:::predict.speedglm(best.disc,data_validation,type="response"))
                         } else performance = normalizedGini(labels[ind_test],speedglm:::predict.speedglm(best.disc,data_test,type="response"))
                    } else {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) performance = 0 else performance =  best.disc[[1]]$aic
                    }
               } else {
                    if (criterion=="gini") {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) {
                              data_validation = as.data.frame(cbind(predictors[ind_validation,],labels = labels[ind_validation]))
                              for (j in 1:d) {
                                   cutoffvalueschi2 <- disc[[which.min(ginidisc)]][["cutp"]][[j]]
                                   cutoffvalueschi2[length(cutoffvalueschi2)+1] <- -Inf
                                   cutoffvalueschi2[length(cutoffvalueschi2)+1] <- Inf

                                   data_validation[,j] <- cut(data_validation[,j],cutoffvalueschi2, include.lowest = FALSE, labels = seq(1:(length(cutoffvalueschi2)-1)))
                                   data_validation[,j] <- factor(data_validation[,j])
                              }
                              performance = normalizedGini(labels[ind_validation],speedglm:::predict.speedglm(best.disc,data_validation,type="response"))
                         } else performance = normalizedGini(labels[ind_train],best.disc$fitted.values)
                    } else {
                         best.disc = list(logit,disc)
                         if (validation==TRUE) performance = 0 else performance = best.disc[[1]]$aic
                    }

               }

               return(new(Class = "mdlp_disc", method.name = "mdlp", parameters = list(test,validation,criterion), best.disc = best.disc, performance = performance))

          }
          else {
               print("Les labels et les predicteurs doivent avoir la meme longueur")
          }
     }
     else {
          print("Le critere doit etre 'gini' ou 'aic'")
     }
}
