#' Model-based multivariate discretization for logistic regression (using the mnlogit package).
#'
#' This function discretizes a training set using an SEM-Gibbs based method (see References section).
#' @param predictors The matrix array containing the numeric attributes to discretize.
#' @param labels The actual labels of the provided predictors (0/1).
#' @param validation Boolean : True if the algorithm should use predictors to construct a validation set on which to search for the best discretization scheme using the provided criterion (default: TRUE).
#' @param test Boolean : True if the algorithm should use predictors to construct a test set on which to calculate the provided criterion using the best discretization scheme (chosen thanks to the provided criterion on either the test set (if true) or the training set (otherwise)) (default: TRUE).
#' @param criterion The criterion ('gini','aic','bic') to use to choose the best discretization scheme among the generated ones (default: 'gini'). Nota Bene: it is best to use 'gini' only when test is set to TRUE and 'aic' or 'bic' when it is not. When using 'aic' or 'bic' with a test set, the likelihood is returned as there is no need to penalize for generalization purposes.
#' @param iter The number of iterations to do in the SEM protocole (default: 1000).
#' @param m_depart The maximum number of resulting categories for each variable wanted (default: 20).
#' @param reg_type The model to use between discretized and continuous features (currently, only multinomial logistic regression ('poly') and ordered logistic regression ('polr') are supported ; default: 'poly').
#' @keywords SEM, Gibbs, discretization
#' @author Adrien Ehrhardt
#' @seealso \code{glm}, \code{mnlogit}, \code{polr}
#' @details
#' This function finds the most appropriate discretization scheme for logistic regression. When provided with a continuous variable \eqn{X}, it tries to convert it to a categorical variable \eqn{E} which values uniquely correspond to intervals of the continuous variable \eqn{X}.
#' When provided with a categorical variable \eqn{X}, it tries to find the best regroupement of its values and subsequently creates categorical variable \eqn{E}. The goal is to perform supervised learning with logistic regression so that you have to specify a target variable \eqn{Y} denoted by \code{labels}.
#' The ‘‘discretization'' process, i.e. the transformation of \eqn{X} to \eqn{E} is done so as to achieve the best logistic regression model \eqn{p_\theta(y|e)}. It can be interpreted as a special case feature engineering algorithm.
#' Subsequently, its outputs are: the optimal discretization scheme and the logistic regression model associated with it. We also provide the parameters that were provided to the function and the evolution of the criterion with respect to the algorithm's iterations.
#' @importFrom stats predict
#' @export
#' @references
#' Ehrhardt, A., Biernacki, C., Vandewalle, V., Heinrich, P. (2018), Model-based multivariate discretization for logistic regression,
#'
#' Celeux, G., Chauveau, D., Diebolt, J. (1995), On Stochastic Versions of the EM Algorithm. [Research Report] RR-2514, INRIA. 1995. <inria-00074164>
#'
#' Asad Hasan, Wang Zhiyu and Alireza S. Mahani (2015). mnlogit: Multinomial Logit Model. R package version 1.2.4. \url{https://CRAN.R-project.org/package=mnlogit}
#'
#' Agresti, A. (2002) \emph{Categorical Data}. Second edition. Wiley.
#' @examples
#' # Simulation of a discretized logit model
#' set.seed(1)
#' x = matrix(runif(300), nrow = 100, ncol = 3)
#' cuts = seq(0,1,length.out= 4)
#' xd = apply(x,2, function(col) as.numeric(cut(col,cuts)))
#' theta = t(matrix(c(0,0,0,2,2,2,-2,-2,-2),ncol=3,nrow=3))
#' log_odd = rowSums(t(sapply(seq_along(xd[,1]), function(row_id) sapply(seq_along(xd[row_id,]),
#' function(element) theta[xd[row_id,element],element]))))
#' y = rbinom(100,1,1/(1+exp(-log_odd)))
#'
#' sem_disc <- sem_polytomique(x,y,iter=100,m_depart=4,test=FALSE,validation=FALSE,criterion="aic")
#' discretize(sem_disc,data.frame(x))


sem_polytomique <- function(predictors,labels,interact=TRUE,validation=TRUE,test=TRUE,criterion='gini',iter=1000,m_depart=20,reg_type='poly') {
#     source("~/Documents/discretisation/R_discretisation/methods/bugfix_speedglm.R")

     # requireNamespace("mnlogit")
     if (criterion %in% c('gini','aic','bic')) {
          if (length(labels)==length(predictors[,1])) {

               # requireNamespace("MASS")
               # Calcul des longueurs pour r??utilisation ult??rieure
               n = length(labels)
               d = length(predictors[1,])
               types_data <- sapply(predictors[1,],class)

               if (sum(!(types_data %in% c("numeric","factor")))>0) {
                    stop("Unsupported data types. Columns of predictors must be numeric or factor.")
               }

               # Initialisation du crit??re de performance
               criterion_iter=list()

               # D??coupage de l'ensemble
               ensemble <- scoring:::cut_dataset(n,test=test,validation=validation)

               # Initialisation al??atoire de e dans 1:m_depart pour l'ensemble d'apprentissage
               e = emap = array(0,c(n,d))
               if (sum(types_data=="numeric")>0) {
                    e[,which(types_data=="numeric")] = emap[,which(types_data=="numeric")] = sapply(which(types_data=="numeric"),function(i) as.factor(sample(1:m_depart,n,replace = TRUE)))
               }
               if (sum(types_data=="factor")>0) {
                    e[,which(types_data=="factor")] = emap[,which(types_data=="factor")] = sapply(which(types_data=="factor"),function(i) as.factor(sample(1:nlevels(predictors[,i]),n,replace = TRUE)))
               }

               m = rep(m_depart,d)
               m[which(types_data=="factor")] = as.vector(sapply(predictors[,which(types_data=="factor")],nlevels))
               names(m) <- paste("X", 1:length(m), sep = "")

               current_best = 1
               best_reglog = 0
               best_link = 0

               if ((criterion=='gini') & (validation==FALSE)) {
                    warning("Using Gini index on training set might yield an overfitted model.")
               }

               if ((criterion %in% c('aic','bic')) & (validation==TRUE)) {
                    warning("No need to penalize the log-likelihood when a validation set is used. Using log-likelihood instead of AIC/BIC.")
               }

               if (interact==TRUE) {
                    delta <- matrix(as.logical(sample(0:1,d^2,replace=TRUE,prob=c(0.5,0.5))),nrow=d,ncol=d)
                    delta[lower.tri(delta)] = FALSE
                    diag(delta) <- FALSE
                    xPrincipal <- paste0("X", 1:d)

                    p_delta = matrix(0,nrow=d,ncol=d)
                    delta[lower.tri(delta)] = 0
                    diag(delta) <- 0

                    for (j in 1:(d-1)) {
                         for (k in (j+1):d) {
                              sans_inter <- stats::glm(labels ~ X1 + X2, family=binomial(link="logit"), data=data.frame(labels = labels[ensemble[[1]]],X1 = predictors[ensemble[[1]],j],X2 = predictors[ensemble[[1]],k]))
                              avec_inter <- stats::glm(labels ~ X1 + X2 + X1:X2, family=binomial(link="logit"), data=data.frame(labels = labels[ensemble[[1]]],X1 = predictors[ensemble[[1]],j],X2 = predictors[ensemble[[1]],k]))
                              p_delta[j,k] <- 1/(1+exp(sans_inter$deviance - 2*log(length(ensemble[[1]]))*length(sans_inter$coefficients))/exp(avec_inter$deviance - 2*log(length(ensemble[[1]]))*length(avec_inter$coefficients)))
                         }
                    }
               }

               # Algo SEM
               for (i in 1:iter){

                    data = data.frame(e,labels = labels)
                    data_logit = data.frame(emap,labels = labels)

                    # Entra??nement de y | e
                    ## Create a formula for a model with a large number of variables:
                    if (interact==TRUE) {
                         tab_vrai <- which(delta,arr.ind=TRUE)

                         if (sum(tab_vrai)>0) {
                              xInter <- xInter_logit <- sapply(1:nrow(tab_vrai), function(row) paste0("X",tab_vrai[row,"row"],":X",tab_vrai[row,"col"]))
                              ejecter_logit <- sapply(1:(ncol(data_logit)-1), function(col) length(unique(data_logit[,col]))==1)
                              ejecter <- sapply(1:(ncol(data)-1), function(col) length(unique(data[,col]))==1)

                              if (length(xPrincipal[ejecter])>0) {
                                   if (length(grep(xPrincipal[ejecter],xInter_logit,value=TRUE))>0) {
                                        xInter <- xInter[!(xInter==grep(xPrincipal[ejecter],xInter,value=TRUE))]
                                   }
                              }

                              if (length(xPrincipal[ejecter_logit])>0) {
                                   if (length(grep(xPrincipal[ejecter_logit],xInter_logit,value=TRUE))>0) {
                                        xInter_logit <- xInter_logit[!(xInter_logit==grep(xPrincipal[ejecter_logit],xInter_logit,value=TRUE))]
                                   }
                              }

                              if (length(xInter)>0) {
                                   fmla <- as.formula(paste("labels ~ ", paste(xPrincipal[!ejecter], collapse= "+"), "+", paste(xInter, collapse= "+")))
                              } else {
                                   fmla <- as.formula(paste("labels ~ ", paste(xPrincipal[!ejecter], collapse= "+")))
                              }

                              if (length(xInter_logit)>0) {
                                   fmla_logit <- as.formula(paste("labels ~ ", paste(xPrincipal[!ejecter_logit], collapse= "+"), "+", paste(xInter_logit, collapse= "+")))
                              } else {
                                   fmla_logit <- as.formula(paste("labels ~ ", paste(xPrincipal[!ejecter_logit], collapse= "+")))
                              }
                              model_reglog = tryCatch((stats::glm(fmla_logit,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE,start=model_reglog$coefficients)),error=function(cond) (stats::glm(fmla_logit,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE)))

                              logit = tryCatch((stats::glm(fmla,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE,start=logit$coefficients)),error=function(cond) (stats::glm(fmla,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE)))
                         } else {
                              model_reglog = tryCatch((stats::glm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE,start=model_reglog$coefficients)),error=function(cond) (stats::glm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE)))

                              logit = tryCatch((stats::glm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE,start=logit$coefficients)),error=function(cond) (stats::glm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE)))
                         }
                    } else {
                         model_reglog = tryCatch((stats::glm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE,start=model_reglog$coefficients)),error=function(cond) (stats::glm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE)))

                         logit = tryCatch((stats::glm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE,start=logit$coefficients)),error=function(cond) (stats::glm(labels ~ .,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data[ensemble[[1]],]), y=FALSE, model=FALSE)))
                    }

                    # Calcul du crit??re
                    if ((criterion=='gini')&&(validation==FALSE)) {
                         criterion_iter[[i]] = normalizedGini(labels[ensemble[[1]]],predict(model_reglog,data_logit[ensemble[[1]],],type='response'))
                    } else if ((criterion=='gini')&&(validation==TRUE)) {
                         criterion_iter[[i]] = normalizedGini(labels[ensemble[[2]]],predict(model_reglog,data_logit[ensemble[[2]],],type='response'))
                    } else if ((criterion=='aic')&&(validation==FALSE)) {
                         criterion_iter[[i]] = -model_reglog$aic
                    } else if ((criterion=='bic')&&(validation==FALSE)) {
                         criterion_iter[[i]] = -model_reglog$deviance + 2*log(length(ensemble[[1]]))*length(model_reglog$coefficients)
                    } else if ((criterion %in% c('aic','bic'))&&(validation==TRUE)) {
                         criterion_iter[[i]] = sum(log(labels[ensemble[[2]]]*predict(model_reglog,data_logit[ensemble[[2]],],type='response') + (1-labels[ensemble[[2]]])*(1-labels[ensemble[[2]]]*predict(model_reglog,data_logit[ensemble[[2]],],type='response'))))
                    } else stop("validation must be boolean!")


                    if (criterion_iter[[i]] >= criterion_iter[[current_best]]) {
                         best_reglog = model_reglog
                         best_link = tryCatch(link,error=function(cond) list())
                         current_best = i
                    }

                    # Interactions
                    if (interact==TRUE) {
                         for (j in 1:(d-1)) {
                              for (k in (j+1):d) {
                                   if (delta[j,k]) {
                                        delta_new = delta
                                        delta_new[j,k] = FALSE

                                        if (sum(delta_new)>0) {
                                             xInter_new <- sapply(1:nrow(which(delta_new,arr.ind=TRUE)), function(row) paste0("X",which(delta_new,arr.ind=TRUE)[row,"row"],":X",which(delta_new,arr.ind=TRUE)[row,"col"]))
                                             fmla_new <- as.formula(paste("labels ~ ", paste(xPrincipal, collapse= "+"), "+", paste(xInter_new, collapse= "+")))
                                        } else {
                                             fmla_new <- as.formula(paste("labels ~ ", paste(xPrincipal, collapse= "+")))
                                        }
                                        new_aic = stats::glm(fmla_new,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE)$aic
                                   } else {
                                        delta_new = delta
                                        delta_new[j,k] = TRUE

                                        if (sum(delta_new)>0) {
                                             xInter_new <- sapply(1:nrow(which(delta_new,arr.ind=TRUE)), function(row) paste0("X",which(delta_new,arr.ind=TRUE)[row,"row"],":X",which(delta_new,arr.ind=TRUE)[row,"col"]))
                                             fmla_new <- as.formula(paste("labels ~ ", paste(xPrincipal, collapse= "+"), "+", paste(xInter_new, collapse= "+")))
                                        } else {
                                             fmla_new <- as.formula(paste("labels ~ ", paste(xPrincipal, collapse= "+")))
                                        }

                                        new_aic = stats::glm(fmla_new,family = stats::binomial(link = "logit"), data=Filter(function(x)(length(unique(x))>1),data_logit[ensemble[[1]],]), y=FALSE, model=FALSE)$aic
                                   }
                                   alpha = exp(model_reglog$aic-new_aic)
                                   print(alpha)
                                   delta[j,k] <- sample(c(delta_new[j,k],delta[j,k]),1,prob = c(max(0,min(alpha,1)),1-max(0,min(alpha,1))))
                              }
                         }
                    }


                    # Initialisation link function
                    link=list()

                    for (j in sample(1:d)) {

                         # Entra??nement de e^j | x^j
                         if (length(unique(e[ensemble[[1]],j]))>1) {

                              # Autorisation de suppression de modalit??s
                              m[j] = nlevels(as.factor(e[,j]))
                              lev_j <- levels(as.factor(e[,j]))

                              if ((reg_type=='poly')&(types_data[j]=="numeric")) {
                                        long_dataset <- data.frame(e = as.vector(sapply(e[ensemble[[1]],j],function(var) (lev_j[seq(1:as.numeric(m[j]))]==var))),x = as.vector(sapply(predictors[ensemble[[1]],j], function(var) rep(var,as.numeric(m[j])))), names = as.character(as.vector(rep(lev_j[seq(1:as.numeric(m[j]))],length(ensemble[[1]])))),stringsAsFactors=FALSE)
                                        link[[j]] = tryCatch(mnlogit::mnlogit(e ~ 1 | x | 1, data=long_dataset, choiceVar = "names", start = link[[j]]$coefficients, returnData = TRUE, order = TRUE),error=function(cond) link[[j]] = mnlogit::mnlogit(e ~ 1 | x | 1, data=long_dataset, choiceVar = "names", returnData = TRUE, order = TRUE))
                              } else if (types_data[j]=="numeric") {
                                   link[[j]] = tryCatch(MASS::polr(e ~ x, data=data.frame(e = factor(as.numeric(ordered(e[ensemble[[1]],j],levels = names(sort(unlist(by(predictors[ensemble[[1]],j],e[ensemble[[1]],j],mean)))))), ordered=T), x = predictors[ensemble[[1]],j]), Hess = FALSE, model = FALSE, weights = link[[j]]$weights),error=function(cond) tryCatch(MASS::polr(e ~ x, data=data.frame(e = factor(as.numeric(ordered(e[ensemble[[1]],j],levels = names(sort(unlist(by(predictors[ensemble[[1]],j],e[ensemble[[1]],j],mean)))))), ordered=T), x = predictors[ensemble[[1]],j]), Hess = FALSE, model = FALSE), error=function(cond) stats::glm(e ~ x, data=data.frame(e = factor(e[ensemble[[1]],j]), x = predictors[ensemble[[1]],j]), family = stats::binomial(link="logit"), model = FALSE)))
                              }
                         }
                         if (as.numeric(m[j])>1) {
                              y_p = array(0,c(n,as.numeric(m[j])))
                              for (k in 1:as.numeric(m[j])) {
                                   # Dataset e^{-j} et e^j = k
                                   modalites_k <- cbind(e,rep(lev_j[k],n))
                                   colnames(modalites_k) <- paste("X", 1:(d+1), sep = "")
                                   modalites_k <- modalites_k[,-j]
                                   colnames(modalites_k)[ncol(modalites_k)] <- paste("X",j,sep="")
                                   modalites_k <- as.data.frame(modalites_k)
                                   p = predict(logit,newdata=modalites_k,type = "response")

                                   y_p[,k] <- (labels*p+(1-labels)*(1-p))
                              }
                              if ((types_data[j]=="numeric")) {
                                   if (reg_type=='poly') {
                                        if (!requireNamespace("mnlogit", quietly = TRUE)) {
                                             t = predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs")
                                        } else {
                                             long_dataset <- data.frame(e = as.vector(sapply(e[,j],function(var) (lev_j[seq(1:as.numeric(m[j]))]==var))),x = as.vector(sapply(predictors[,j], function(var) rep(var,as.numeric(m[j])))), names = as.character(as.vector(rep(lev_j[seq(1:as.numeric(m[j]))],n))))
                                             t = predict(link[[j]], newdata = long_dataset,type="probs", choiceVar = "names")
                                             t[which(is.nan(t),arr.ind = TRUE)[,"row"],which(is.nan(t),arr.ind = TRUE)[,"col"]] = 1
                                        }
                                   } else {
                                        t = tryCatch(predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs"), error = function(cond) matrix(c(1-predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="response"),predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="response")),ncol=2,dimnames = list(seq(1:n),c(min(levels(factor(e[ensemble[[1]],j]))),max(levels(factor(e[ensemble[[1]],j])))))))
                                   }
                              } else {
                                   link[[j]] = table(e[ensemble[[1]],j],predictors[ensemble[[1]],j])
                                   t = prop.table(t(sapply(predictors[,j],function(row) link[[j]][,row])),1)
                              }

                              # Sans contrainte
                              emap[,j] <- apply(t,1,function(p) names(which.max(p)))

                              t <- prop.table(t*y_p,1)

                              # mise ?? jour de e^j
                              e[,j] <- apply(t,1,function(p) sample(lev_j,1,prob = p))


                              # Controler que dans test et validation on n'a pas de e ou de emap qui ne sont pas dans train
                              # Test

                              if (test==TRUE) {

                                   # E

                                   ind_diff_train_test <- which(e[ensemble[[2]],j]==setdiff(factor(e[ensemble[[2]],j]),factor(e[ensemble[[1]],j])))
                                   while (!length(ind_diff_train_test)==0) {
                                        t[ind_diff_train_test,e[ensemble[[2]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table(t,1)
                                        e[ensemble[[2]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) sample(lev_j,1,prob = p))
                                        ind_diff_train_test <- which(e[ensemble[[2]],j]==setdiff(factor(e[ensemble[[2]],j]),factor(e[ensemble[[1]],j])))
                                   }

                                   # E_MAP

                                   ind_diff_train_test <- which(emap[ensemble[[2]],j]==setdiff(factor(emap[ensemble[[2]],j]),factor(emap[ensemble[[1]],j])))
                                   while (!length(ind_diff_train_test)==0) {
                                        if (reg_type=='poly') {
                                             if (!requireNamespace("mnlogit", quietly = TRUE)) {
                                                  t = predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs")
                                             } else {
                                                  long_dataset <- data.frame(e = as.vector(sapply(e[,j],function(var) (lev_j[seq(1:as.numeric(m[j]))]==var))),x = as.vector(sapply(predictors[,j], function(var) rep(var,as.numeric(m[j])))), names = as.character(as.vector(rep(lev_j[seq(1:as.numeric(m[j]))],n))))
                                                  t = predict(link[[j]], newdata = long_dataset,type="probs", choiceVar = "names")
                                                  t[which(is.nan(t),arr.ind = TRUE)[,"row"],which(is.nan(t),arr.ind = TRUE)[,"col"]] = 1
                                             }
                                        } else {
                                             t = tryCatch(predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs"), error = function(cond) matrix(c(1-predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="response"),predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="response")),ncol=2,dimnames = list(seq(1:n),c(min(levels(factor(e[ensemble[[1]],j]))),max(levels(factor(e[ensemble[[1]],j])))))))
                                        }
                                        t[ind_diff_train_test,emap[ensemble[[2]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table(t,1)
                                        emap[ensemble[[2]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) names(which.max(p)))
                                        ind_diff_train_test <- which(emap[ensemble[[2]],j]==setdiff(factor(emap[ensemble[[2]],j]),factor(emap[ensemble[[1]],j])))
                                   }
                              }


                              # Validation

                              if (validation==TRUE) {

                                   # E

                                   ind_diff_train_test <- which(e[ensemble[[3]],j]==setdiff(factor(e[ensemble[[3]],j]),factor(e[ensemble[[1]],j])))
                                   while (!length(ind_diff_train_test)==0) {
                                        t[ind_diff_train_test,e[ensemble[[3]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table(t,1)
                                        e[ensemble[[3]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) sample(lev_j,1,prob = p))
                                        ind_diff_train_test <- which(e[ensemble[[3]],j]==setdiff(factor(e[ensemble[[3]],j]),factor(e[ensemble[[1]],j])))
                                   }

                                   # E_MAP

                                   ind_diff_train_test <- which(emap[ensemble[[3]],j]==setdiff(factor(emap[ensemble[[3]],j]),factor(emap[ensemble[[1]],j])))
                                   while (!length(ind_diff_train_test)==0) {
                                        if (reg_type=='poly') {
                                             if (!requireNamespace("mnlogit", quietly = TRUE)) {
                                                  t = predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs")
                                             } else {
                                                  long_dataset <- data.frame(e = as.vector(sapply(e[,j],function(var) (lev_j[seq(1:as.numeric(m[j]))]==var))),x = as.vector(sapply(predictors[,j], function(var) rep(var,as.numeric(m[j])))), names = as.character(as.vector(rep(lev_j[seq(1:as.numeric(m[j]))],n))))
                                                  t = predict(link[[j]], newdata = long_dataset,type="probs", choiceVar = "names")
                                                  t[which(is.nan(t),arr.ind = TRUE)[,"row"],which(is.nan(t),arr.ind = TRUE)[,"col"]] = 1
                                             }
                                        } else {
                                             t = tryCatch(predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="probs"), error = function(cond) matrix(c(1-predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="response"),predict(link[[j]], newdata = data.frame(x = predictors[,j]),type="response")),ncol=2,dimnames = list(seq(1:n),c(min(levels(factor(e[ensemble[[1]],j]))),max(levels(factor(e[ensemble[[1]],j])))))))
                                        }
                                        t[ind_diff_train_test,emap[ensemble[[3]],j][ind_diff_train_test]] <- 0
                                        t <- prop.table(t,1)
                                        emap[ensemble[[3]],j][ind_diff_train_test] <- apply(t[ind_diff_train_test,,drop=FALSE],1,function(p) names(which.max(p)))
                                        ind_diff_train_test <- which(e[ensemble[[3]],j]==setdiff(factor(e[ensemble[[3]],j]),factor(e[ensemble[[1]],j])))
                                   }
                              }

                              # Controler le cas ou la modalite de remplacement est elle meme absente du train


                         } else {
                              # mise ?? jour de e^j
                              e[,j] <- emap[,j] <- factor(rep(1,n))

                         }
                    }
               }


               best.disc = list(best_reglog,best_link)

               if (validation==TRUE) {
                    if (criterion=="gini") {
                         if (test==TRUE) performance = normalizedGini(labels[ensemble[[3]]],predict(best.disc[[1]],discretize_link(best.disc[[2]],predictors[ensemble[[3]],]),type="response")) else performance = normalizedGini(labels[ensemble[[2]]],predict(best.disc[[1]],discretize_link(best.disc[[2]],predictors[ensemble[[3]],]),type="response"))
                    } else {
                         if (test==TRUE) performance = -2*sum(labels[ensemble[[3]]]*predict(best.disc[[1]],as.data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[3]],])),type="response")+(1-labels[ensemble[[3]]])*(1-predict(best.disc[[1]],as.data.frame(discretize_link(best.disc[[2]],predictors[ensemble[[3]],])),type="response"))) else performance = criterion_iter[[current_best]]
                    }
               } else {
                    if (criterion=="gini") {
                         if (test==TRUE) performance = normalizedGini(labels[ensemble[[2]]],predict(best.disc[[1]],as.data.frame(t(emap[,which.max(criterion_iter),ensemble[[2]]])),type="response")) else performance = normalizedGini(labels[ensemble[[1]]],best.disc[[1]]$fitted.values)
                    } else {
                         if (test==TRUE) performance = -2*rowSums(labels[ensemble[[2]]]*predict(best.disc[[1]],discretize_link(best.disc[[2]],predictors[ensemble[[2]],]),type="response")+(1-labels[ensemble[[2]]])*(1-predict(best.disc[[1]],discretize_link(best.disc[[2]],predictors[ensemble[[2]],]),type="response"))) else performance = criterion_iter[[current_best]]
                    }

               }

               if (test==TRUE) {
                    return(methods::new(Class = "discretization", method.name = paste("sem",reg_type,sep="_"), parameters = list(test,validation,criterion,iter,m_depart,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter), disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[3]],])),labels = labels[ensemble[[3]]]), cont.data = data.frame(cbind(predictors[ensemble[[3]],]),labels = labels[ensemble[[3]]])))
               } else if (validation==TRUE) {
                    return(methods::new(Class = "discretization", method.name = paste("sem",reg_type,sep="_"), parameters = list(test,validation,criterion,iter,m_depart,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter), disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[2]],])),labels = labels[ensemble[[2]]]), cont.data = data.frame(cbind(predictors[ensemble[[2]],]),labels = labels[ensemble[[2]]])))
               } else {
                    return(methods::new(Class = "discretization", method.name = paste("sem",reg_type,sep="_"), parameters = list(test,validation,criterion,iter,m_depart,reg_type), best.disc = best.disc, performance = list(performance,criterion_iter), disc.data = data.frame(cbind(discretize_link(best.disc[[2]],predictors[ensemble[[1]],])),labels = labels[ensemble[[1]]]), cont.data = data.frame(cbind(predictors[ensemble[[1]],]),labels = labels[ensemble[[1]]])))
               }
          }
          else {
               print("labels and predictors must be of same length")
          }
     }
     else {
          print("criterion must be gini, aic or bic")
     }
}
