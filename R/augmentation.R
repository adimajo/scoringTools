#' Augmentation
#'
#' This function performs Reject Inference using the Augmentation technique. Note that this technique is theoretically better than using the financed clients scorecard in the MAR and misspecified model case.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same features in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @return List containing the model using financed clients only and the model produced using the Augmentation method.
#' @keywords reject, inference, r?int?gration, scorecard, credit, scoring
#' @importFrom stats predict
#' @export
#' @author Adrien Ehrhardt
#' @seealso \code{glm}, \code{speedglm}
#' @details
#' This function performs the Augmentation method on the data. When provided with labeled observations \eqn{(x^\ell,y)}, it first fits the logistic regression model \eqn{p_\theta} of
#' \eqn{x^\ell} on \eqn{y}, then reweighs labeled observations according to their probability of being sampled, i.e. calculates the predicted probabilities of \eqn{p_\theta} on all observations, defines score-bands and calculates, in each of these score-bands, the probability of having been accepted as the proportion of labeled samples in that score-band.
#' It then refits a logistic regression model \eqn{p_\eta} on the labeled samples.
#' @references
#' Enea, M. (2015), speedglm: Fitting Linear and Generalized Linear Models to Large Data Sets, \url{https://CRAN.R-project.org/package=speedglm}
#' Ehrhardt, A., Biernacki, C., Vandewalle, V., Heinrich, P. and Beben, S. (2018), Reject Inference Methods in Credit Scoring: a rational review,
#' @examples
#' # We simulate data from financed clients
#' set.seed(1)
#' xf = matrix(runif(100*2), nrow = 100, ncol = 2)
#' theta = c(2,-2)
#' log_odd = apply(xf, 1, function(row) theta%*%row)
#' yf = rbinom(100,1,1/(1+exp(-log_odd)))
#' # We simulate data from not financed clients (MCAR mechanism)
#' xnf = matrix(runif(100*2), nrow = 100, ncol = 2)
#' augmentation(xf,xnf,yf)

augmentation <- function(xf, xnf, yf) {
     df_f <- data.frame(labels = yf, x = xf)
     if (!requireNamespace("speedglm", quietly = TRUE)) {
          warning("Speedglm not installed, using glm instead (slower).",call. = FALSE)
          model_f <- stats::glm(labels ~ ., family=stats::binomial(link="logit"), data = df_f)
     } else {
          model_f <- speedglm::speedglm(labels ~ ., family=stats::binomial(link="logit"), data = df_f)
          # methods::setOldClass(class(model_f)[1])
          # methods::setOldClass(class(model_f)[2])
          # methods::setIs(class(model_f)[1], "glmORlogicalORspeedglm")
          # methods::setIs(class(model_f)[2], "glmORlogicalORspeedglm")
     }

     df <- rbind(df_f, data.frame(labels = rep(NA,nrow(xnf)), x = xnf))
     df_f$classe_SCORE <- round(predict(model_f,df_f,type="response"), digits=1)
     df$classe_SCORE <- round(predict(model_f,df,type="response"), digits=1)
     df$acc = NA
     df$acc[1:nrow(df_f)] <- 1
     df$acc[(nrow(df_f)+1):nrow(df)] <- 0

     poids <- sqldf::sqldf(
          'select distinct count(*) as count, classe_SCORE, acc
          from df
          group by classe_SCORE, acc
          '
     )
     poids_acceptes <- poids[poids$acc==1,]
     poids_rejetes <- poids[poids$acc==0,]

     poids_acceptes$count_acc <- poids_acceptes$count
     poids_acceptes$count <- NULL
     poids_acceptes$acc <- NULL

     poids_rejetes$count_rej <- poids_rejetes$count
     poids_rejetes$count <- NULL
     poids_rejetes$acc <- NULL

     poids_tot <- merge(poids_acceptes, poids_rejetes, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
     poids_tot$poidsfinal <- ifelse(is.na(poids_tot$count_acc),0,ifelse(is.na(poids_tot$count_rej),1,1+poids_tot$count_rej/poids_tot$count_acc))
     poids_tot$count_acc <- NULL
     poids_tot$count_rej <- NULL

     df_augmente <- merge(df_f, poids_tot, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
     if (!requireNamespace("speedglm", quietly = TRUE)) {
          model_augmente = stats::glm(labels ~ ., family = stats::binomial(link='logit'), df_augmente[,-which(names(df_augmente) %in% c("poidsfinal","classe_SCORE"))], weights = df_augmente$poidsfinal)
     } else {
          model_augmente = speedglm::speedglm(labels ~ ., family = stats::binomial(link='logit'), df_augmente[,-which(names(df_augmente) %in% c("poidsfinal","classe_SCORE"))], weights = df_augmente$poidsfinal)
          # methods::setIs(class(model_augmente), "glmORlogicalORspeedglm")
     }


     return(methods::new(Class = "reject_infered", method_name = "augmentation", financed_model = model_f, acceptance_model = as.logical(NA), infered_model = model_augmente))

}
