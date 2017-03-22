#' Parcelling
#'
#' This function performs Reject Inference using the Parcelling technique. Note that this technique is theoretically good in the MNAR framework although coefficients must be chosen a priori.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @keywords reject, inference, r?int?gration, scorecard, credit, scoring
#' @import speedglm
#' @export
#' @examples
#' parcelling()

parcelling <- function(xf,xnf,yf) {
     df_f <- data.frame(labels = yf, x = xf)
     model_f <- speedglm(labels ~ ., family=binomial(link="logit"), data = df_f)

     df <- rbind(df_f, data.frame(labels = rep(NA,nrow(xnf)), x = xnf))
     df_f$classe_SCORE <- round(predict(model_f,df_f,type="response"), digits=1)
     df$classe_SCORE <- round(predict(model_f,df,type="response"), digits=1)
     df$acc[1:nrow(df_f)] <- 1
     df$acc[(nrow(df_f)+1):nrow(df)] <- 0

     poids_part <- sqldf(
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

     df_parceling[df_parceling$acc==0,"labels"] <- sapply(df_parceling[df_parceling$acc==0,"poids_final"],function(x) (rbinom(1,1,1-x)))

     model_parcelling = speedglm(labels ~ ., family = binomial(link='logit'), df_parceling[,-which(names(df_parceling) %in% c("poids_final","classe_SCORE","acc"))])

     return(list(financed.model = model_f, parcelling.model = model_parcelling))
}
