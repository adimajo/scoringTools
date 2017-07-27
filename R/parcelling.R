#' Parcelling
#'
#' This function performs Reject Inference using the Parcelling technique. Note that this technique is theoretically good in the MNAR framework although coefficients must be chosen a priori.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @return List containing the model using financed clients only and the model produced using the Parcelling method.
#' @keywords reject, inference, r?int?gration, scorecard, credit, scoring
#' @import speedglm
#' @export
#' @examples
#' # We simulate data from financed clients
#' set.seed(1)
#' xf = matrix(runif(100*2), nrow = 100, ncol = 2)
#' theta = c(2,-2)
#' log_odd = apply(xf, 1, function(row) theta%*%row)
#' yf = rbinom(100,1,1/(1+exp(-log_odd)))
#' # We simulate data from not financed clients (MCAR mechanism)
#' xnf = matrix(runif(100*2), nrow = 100, ncol = 2)
#' list_models <- parcelling(xf,xnf,yf)
#' # This is the model constructed using the financed clients (xf,yf):
#' list_models[1]
#' # This is the model constructed using all the clients (xf,yf,xnf) and the parcelling technique:
#' list_models[2]


parcelling <- function(xf,xnf,yf) {
     df_f <- data.frame(labels = yf, x = xf)
     model_f <- speedglm::speedglm(labels ~ ., family=stats::binomial(link="logit"), data = df_f)

     df <- rbind(df_f, data.frame(labels = rep(NA,nrow(xnf)), x = xnf))
     df_f$classe_SCORE <- round(speedglm:::predict.speedglm(model_f,df_f,type="response"), digits=1)
     df$classe_SCORE <- round(speedglm:::predict.speedglm(model_f,df,type="response"), digits=1)
     df$acc[1:nrow(df_f)] <- 1
     df$acc[(nrow(df_f)+1):nrow(df)] <- 0

     poids_part <- sqldf::sqldf(
          'select distinct count(labels) as count, classe_SCORE, labels
          from df_f
          group by classe_SCORE, labels
          '
     )

     poids_bon <- poids_part[poids_part$labels==1,]
     poids_mauvais <- poids_part[poids_part$labels==0,]
     poids_bon$labels <- NULL
     poids_mauvais$labels <- NULL
     poids <- merge(poids_bon, poids_mauvais, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
     poids$poids_final <- poids$count.y/(poids$count.x+poids$count.y)
     poids$count.x <- NULL
     poids$count.y <- NULL

     df_parceling <- merge(df, poids, by="classe_SCORE", all.x=TRUE, all.y=TRUE)
     df_parceling$poids_final <- ifelse(is.na(df_parceling$poids_final), 1, df_parceling$poids_final)

     df_parceling[df_parceling$acc==0,"labels"] <- sapply(df_parceling[df_parceling$acc==0,"poids_final"],function(x) (stats::rbinom(1,1,1-x)))

     model_parcelling = speedglm::speedglm(labels ~ ., family = stats::binomial(link='logit'), df_parceling[,-which(names(df_parceling) %in% c("poids_final","classe_SCORE","acc"))])

     return(list(financed.model = model_f, parcelling.model = model_parcelling))
}
