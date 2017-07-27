#' Model-based multivariate discretization for logistic regression (using the mnlogit package).
#'
#' This function discretizes a training set using an SEM-Gibbs based method (see References section).
#' @param predictors The matrix array containing the numeric attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @param iter The number of iterations to do in the SEM protocole (default: 1000).
#' @param m_depart The maximum number of resulting categories for each variable wanted (default: 20).
#' @param reg_type The model to use between discretized and continuous features (currently, only multinomial logistic regression ('poly') and ordered logistic regression ('polr') are supported ; default: 'poly').
#' @keywords SEM, Gibbs, discretization
#' @export
#' @examples
#' sem_polytomique()


sem_polytomique <- function(predictors,labels,test=TRUE,validation=TRUE,criterion='gini',iter=1000,m_depart=20,reg_type='poly') {
     source("~/Documents/discretisation/R_discretisation/methods/bugfix_speedglm.R")
     if (criterion %in% c('gini','aic')) {
          if (length(labels)==length(predictors[,1])) {

               # Calcul des longueurs pour r??utilisation ult??rieure
               n = length(labels)
               d = length(predictors[1,])

               # Initialisation du crit??re de performance
               criterion_iter=list()

               # D??coupage de l'ensemble
               ensemble <- cut.dataset(n,test=test,validation=validation)

               # Initialisation al??atoire de e dans 1:m_depart pour l'ensemble d'apprentissage
               e=array(0,c(n,d))
               emap=array(0,c(n,d))
               e = sapply(1:d,function(i) as.factor(sample(1:m_depart,n,replace = TRUE)))
               emap = sapply(1:d,function(i) as.factor(sample(1:m_depart,n,replace = TRUE)))
               #                for (j in 1:d) {
               #                e[j,1,] = (as.character(as.numeric(cut(predictors[,j],seq(0,1,length.out=m_depart)))))
               #                emap[j,1,] = e[j,1,]
               #                }

               m = rep(m_depart,d)
               names(m) <- paste("V", 1:length(m), sep = "")

               current_best = 1
               best_reglog = 0
               best_link = 0

               # Algo SEM
               for (i in 1:iter){

                    data = as.data.frame(cbind(e,labels))
                    data_logit = as.data.frame(cbind(emap,labels))

                    # Entra??nement de y | e
                    # model_reglog[[i]] = step(glm(labels~.,family = "binomial", data=Filter(function(x)(length(unique(x))>1),data_logit)),trace = 0) # Ajout de stepwise
                    model_reglog = tryCatch((speedglm::speedglm(labels~.,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE,start=model_reglog$coefficients,eigendec=FALSE)),error=function(cond) reduireGLM(speedglm(labels~.,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE,eigendec=FALSE)))
                    # model_reglog = tryCatch(reduireGLM(speedglm(labels~.,family = binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE,start=model_reglog$coefficients, mustart=logit$weights)),error=function(cond) reduireGLM(speedglm(labels~.,family = binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE)))

                    logit = tryCatch((speedglm::speedglm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE,start=logit$coefficients,eigendec=FALSE)),error=function(cond) reduireGLM(speedglm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE,eigendec=FALSE)))
                    # logit = tryCatch(reduireGLM(glm(labels ~ .,family = binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE,start=logit$coefficients, mustart=logit$weights)),error=function(cond) reduireGLM(glm(labels ~ .,family = binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE)))

                    # Calcul du crit??re
                    if (criterion=='gini') criterion_iter[[i]] = normalizedGini(labels[ensemble[[2]]],speedglm:::predict.speedglm(model_reglog,data_logit[ensemble[[2]],],type='response')) else criterion_iter[[i]] = -model_reglog$aic

                    if (criterion_iter[[i]] >= criterion_iter[[current_best]]) {
                         best_reglog = model_reglog
                         best_link = tryCatch(link,error=function(cond) list())
                         current_best = i
                    }

                    # Initialisation link function
                    link=list()

                    for (j in sample(1:d)) {
                         # Entra??nement de e^j | x^j
                         if (length(unique(e[ensemble[[1]],j]))>1) {
                              # Autorisation de suppression de modalit??s
                              m[j] = nlevels(as.factor(e[,j]))
                              lev_j <- levels(as.factor(e[,j]))

                              if (reg_type=='poly') {
                                   long_dataset <- data.frame(e = as.vector(sapply(e[ensemble[[1]],j],function(var) (lev_j[seq(1:m[j])]==var))),x = as.vector(sapply(predictors[ensemble[[1]],j], function(var) rep(var,m[j]))), names = as.character(as.vector(rep(lev_j[seq(1:m[j])],length(ensemble[[1]])))))
                                   link[[j]] = tryCatch(mnlogit::mnlogit(e ~ 1 | x | 1, data=long_dataset, choiceVar = "names", start = link[[j]]$coefficients, returnData = FALSE, order = TRUE),error=function(cond) link[[j]] = mnlogit::mnlogit(e ~ 1 | x | 1, data=long_dataset, choiceVar = "names", returnData = FALSE, order = TRUE))
                              } else {
                                   link[[j]] = tryCatch(MASS::polr(factor(as.numeric(ordered(e[ensemble[[1]],j],levels = names(sort(unlist(by(predictors[ensemble[[1]],j],e[ensemble[[1]],j],mean)))))), ordered=T) ~ predictors[ensemble[[1]],j], Hess = FALSE, model = FALSE, weights = link[[j]]$weights),error=function(cond) MASS::polr(factor(as.numeric(ordered(e[ensemble[[1]],j],levels = names(sort(unlist(by(predictors[ensemble[[1]],j],e[ensemble[[1]],j],mean)))))), ordered=T) ~ predictors[ensemble[[1]],j], Hess = FALSE, model = FALSE))
                              }
                         }
                         if (m[j]>1) {
                              y_p = array(0,c(n,m[j]))

                              for (k in 1:m[j]) {
                                   # Dataset e^{-j} et e^j = k
                                   modalites_k <- cbind(e,rep(lev_j[k],n))
                                   colnames(modalites_k) <- paste("V", 1:(d+1), sep = "")
                                   modalites_k <- modalites_k[,-j]
                                   colnames(modalites_k)[ncol(modalites_k)] <- paste("V",j,sep="")
                                   modalites_k <- as.data.frame(modalites_k)
                                   if (as.numeric(lev_j[k])==1) {
                                        levels(modalites_k[,ncol(modalites_k)]) <- c("1","2")
                                   } else {
                                        levels(modalites_k[,ncol(modalites_k)]) <- c(lev_j[k],"1")
                                        modalites_k[,ncol(modalites_k)] <- stats::relevel(modalites_k[,ncol(modalites_k)],"1")
                                   }
                                   # p = p(y_i=1|e^{-j},e^j = k)
                                   # t = p(y_i|e^{-j},e^j = k) p(e^j | x^j) prop. p(y_i,e_i|x_i)
                                   p = speedglm:::predict.speedglm(logit,newdata=modalites_k,type = "response")
                                   #                                    if (link[[i]][[j]]$edf==2) t = prop.table(cbind(predict(link[[i]][[j]],predictors[,j],type="probs"),rep(1,n)-predict(link[[i]][[j]],predictors[,j],type="probs"))*(labels[ensemble[[1]]]*p+(1-labels[ensemble[[1]]])*(1-p)),1) else t = prop.table(predict(link[[i]][[j]],predictors[,j],type="probs")*(labels[ensemble[[1]]]*p+(1-labels[ensemble[[1]]])*(1-p)),1)
                                   #                                    t <- t[,order(as.numeric(colnames(t)))]
                                   y_p[,k] <- (labels*p+(1-labels)*(1-p))
                              }
                              if (reg_type=='poly') {
                                   long_dataset <- data.frame(e = as.vector(sapply(e[,j],function(var) (lev_j[seq(1:m[j])]==var))),x = as.vector(sapply(predictors[,j], function(var) rep(var,m[j]))), names = as.character(as.vector(rep(lev_j[seq(1:m[j])],n))))
                                   t = mnlogit:::predict.mnlogit(link[[j]], newdata = long_dataset,type="probs", choiceVar = "names")
                              } else {
                                   t = mnlogit:::predict.mnlogit(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs")
                              }
                              #                           t <- t[,order(as.numeric(colnames(t)))]
                              emap[,j] <- apply(t,1,function(p) names(which.max(p)))
                              t <- prop.table(t*y_p,1)
                              # mise ?? jour de e^j
                              e[,j] <- apply(t,1,function(p) sample(lev_j,1,prob = p))
                              # diff_train_test <- setdiff(levels(as.factor(emap[j,ensemble[[2]]])),levels(as.factor(emap[j,ensemble[[1]]])))
                              # diff_train_validation <- setdiff(levels(as.factor(emap[j,ensemble[[3]]])),levels(as.factor(emap[j,ensemble[[1]]])))

                              # if (length(diff_train_test) != 0) {
                              #      t_partial <- t[emap[j,] %in% diff_train_test,,drop=FALSE]
                              #      emap[j,emap[j,]==diff_train_test] <- apply(t_partial,1,function(p) names(which.max(p[-which.max(p)])))
                              # }

                         } else {
                              # mise ?? jour de e^j
                              e[,j] <- emap[,j] <- factor(rep(1,n))

                         }
                    }
               }

               # setClass("sem", representation(method.name = "character", parameters = "list", best.disc = "list", performance = "list"))

               if (test==TRUE) {
                    if (criterion=="gini") {
                         best.disc = list(best_reglog,best_link)
                         if (validation==TRUE) performance = normalizedGini(labels[ensemble[[3]]],speedglm:::predict.speedglm(best.disc[[1]],as.data.frame(t(emap[,which.max(criterion_iter),ensemble[[3]]])),type="response")) else performance = normalizedGini(labels[ensemble[[2]]],speedglm:::predict.speedglm(best.disc[[1]],as.data.frame(t(emap[,which.max(criterion_iter),ensemble[[2]]])),type="response"))
                    } else {
                         best.disc = list(best_reglog,best_link)
                         if (validation==TRUE) performance = 0 else performance = 0
                    }
               } else {
                    if (criterion=="gini") {
                         best.disc = list(best_reglog,best_link)
                         if (validation==TRUE) performance = normalizedGini(labels[ensemble[[3]]],speedglm:::predict.speedglm(best.disc[[1]],as.data.frame(t(emap[,which.max(criterion_iter),ensemble[[3]]])),type="response")) else performance = normalizedGini(labels[ensemble[[1]]],best.disc[[1]]$fitted.values)
                    } else {
                         best.disc = list(best_reglog,best_link)
                         if (validation==TRUE) performance = 0 else performance = best.disc[[1]]$aic
                    }

               }

               # return(new(Class = "sem", method.name = paste("sem",reg_type,sep="_"), parameters = list(test,validation,criterion,iter,m_depart,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter,time_measure)))
               return(list(method.name = paste("sem",reg_type,sep="_"), parameters = list(test,validation,criterion,iter,m_depart,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter)))
          }
          else {
               print("Les labels et les pr??dicteurs doivent avoir la m??me longueur")
          }
     }
     else {
          print("Le crit??re doit ??tre 'gini' ou 'aic'")
     }
}
