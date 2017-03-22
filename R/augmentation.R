#' Augmentation
#'
#' This function performs Reject Inference using the Augmentation technique. Note that this technique is theoretically better than using the financed clients scorecard in the MAR and misspecified model case.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @keywords reject, inference, r?int?gration, scorecard, credit, scoring
#' @import speedglm
#' @export
#' @examples
#' augmentation()

augmentation <- function(xf, xnf, yf) {
     df_f <- data.frame(labels = yf, x = xf)
     model_f <- speedglm(labels ~ ., family=binomial(link="logit"), data = df_f)

     df <- rbind(df_f, data.frame(labels = rep(NA,nrow(xnf)), x = xnf))
     df_f$classe_SCORE <- round(predict(model_f,df_f,type="response"), digits=1)
     df$classe_SCORE <- round(predict(model_f,df,type="response"), digits=1)
     df$acc[1:nrow(df_f)] <- 1
     df$acc[(nrow(df_f)+1):nrow(df)] <- 0

     poids <- sqldf(
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
     model_augmente = speedglm(labels ~ ., family = binomial(link='logit'), df_augmente[,-which(names(df_augmente) %in% c("poidsfinal","classe_SCORE"))], weights = df_augmente$poidsfinal)

     return(list(financed.model = model_f, augmented.model = model_augmente))
}
