#' Parcelling
#'
#' This function performs Reject Inference using the Parcelling technique. Note that this technique is theoretically good in the MNAR framework although coefficients must be chosen a priori.
#' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
#' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
#' @param yf The matrix of financed clients' labels
#' @param probs The sequence of quantiles to use to make scorebands (see the vignette).
#' @param alpha The user-defined coefficients to use with Parcelling (see the vignette).
#' @return List containing the model using financed clients only and the model produced using the Parcelling method.
#' @keywords reject inference réintégration scorecard credit scoring
#' @importFrom stats predict
#' @export
#' @author Adrien Ehrhardt
#' @seealso \code{glm}, \code{speedglm}
#' @details
#' This function performs the Parcelling method on the data. When provided with labeled observations \eqn{(x^\ell,y)}, it first fits the logistic regression model \eqn{p_\theta} of
#' \eqn{x^\ell} on \eqn{y}, then labels the unlabelled samples \eqn{x^{u}} with the observed bad rate in user-defined classes of predicted probabilities of \eqn{p_\theta} reweighted using user-supplied weights, i.e. \eqn{\hat{y}^{u} = \alpha_k T(k)} where \eqn{k} denotes the group (which depends on \eqn{p_\theta}) and T(k) the observed bad rate of labeled observations in this group.
#' It then refits a logistic regression model \eqn{p_\eta} on the whole sample.
#' @references
#' Enea, M. (2015), speedglm: Fitting Linear and Generalized Linear Models to Large Data Sets, \url{https://CRAN.R-project.org/package=speedglm}
#' Ehrhardt, A., Biernacki, C., Vandewalle, V., Heinrich, P. and Beben, S. (2018), Reject Inference Methods in Credit Scoring: a rational review,
#' @examples
#' # We simulate data from financed clients
#' df <- generate_data(n = 100, d = 2)
#' xf <- df[, -ncol(df)]
#' yf <- df$y
#' # We simulate data from not financed clients (MCAR mechanism)
#' xnf <- generate_data(n = 100, d = 2)[, -ncol(df)]
#' parcelling(xf, xnf, yf)
parcelling <- function(xf, xnf, yf, probs = seq(0, 1, 0.25), alpha = rep(1, length(probs) - 1)) {
  check_consistency(xf, xnf, yf)
  df_f <- data.frame(labels = yf, x = xf)
  model_f <- model_finances(df_f)

  df <- rbind(df_f, data.frame(labels = rep(NA, nrow(xnf)), x = xnf))
  classe_SCORE <- stats::quantile(predict(model_f, df_f, type = "response"), probs = probs)
  df_f$classe_SCORE <- cut(predict(model_f, df_f, type = "response"), breaks = c(classe_SCORE[2:(length(classe_SCORE) - 1)], Inf, -Inf), labels = names(classe_SCORE[-1]))
  df$classe_SCORE <- cut(predict(model_f, df, type = "response"), breaks = c(classe_SCORE[2:(length(classe_SCORE) - 1)], Inf, -Inf), labels = names(classe_SCORE[-1]))
  df$acc <- NA
  df$acc[1:nrow(df_f)] <- 1
  df$acc[(nrow(df_f) + 1):nrow(df)] <- 0

  poids_part <- sqldf::sqldf(
    "select distinct count(labels) as count, classe_SCORE, labels
          from df_f
          group by classe_SCORE, labels
          "
  )

  poids_bon <- poids_part[poids_part$labels == 1, ]
  poids_mauvais <- poids_part[poids_part$labels == 0, ]
  poids_bon$labels <- NULL
  poids_mauvais$labels <- NULL
  poids <- merge(poids_bon, poids_mauvais, by = "classe_SCORE", all.x = TRUE, all.y = TRUE)
  poids$poids_final <- poids$count.y / (poids$count.x + poids$count.y)
  poids$poids_final <- poids$poids_final * alpha
  poids$count.x <- NULL
  poids$count.y <- NULL

  df_parceling <- merge(df, poids, by = "classe_SCORE", all.x = TRUE, all.y = TRUE)
  df_parceling$poids_final <- ifelse(is.na(df_parceling$poids_final), 1, df_parceling$poids_final)

  df_parceling[df_parceling$acc == 0, "labels"] <- sapply(df_parceling[df_parceling$acc == 0, "poids_final"], function(x) (stats::rbinom(1, 1, 1 - x)))

  if (!(is_speedglm_installed() & is_speedglm_predict_installed())) {
    model_parcelling <- stats::glm(labels ~ ., family = stats::binomial(link = "logit"), df_parceling[, -which(names(df_parceling) %in% c("poids_final", "classe_SCORE", "acc"))])
  } else {
    model_parcelling <- speedglm::speedglm(labels ~ ., family = stats::binomial(link = "logit"), df_parceling[, -which(names(df_parceling) %in% c("poids_final", "classe_SCORE", "acc"))])
  }
  class(model_parcelling) <- c("glmORlogicalORspeedglm", class(model_parcelling))

  return(methods::new(Class = "reject_infered", method_name = "parceling", financed_model = model_f, acceptance_model = as.logical(NA), infered_model = model_parcelling))
}
