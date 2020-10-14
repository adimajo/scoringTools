# #' Check consistency for reject inference inputs
# #'
# #' This function checks consistency for reject inference inputs
# #' @param xf The matrix of financed clients' characteristics to be used in the scorecard.
# #' @param xnf The matrix of not financed clients' characteristics to be used in the scorecard (must be the same in the same order as xf!).
# #' @param yf The matrix of financed clients' labels
# #' @author Adrien Ehrhardt
# #' @seealso \code{glm}, \code{speedglm}
# #' @keywords internal
# #' @author Adrien Ehrhardt
# #' @examples
# #' # We simulate data from financed clients
# #' xf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
# #' theta <- c(2, -2)
# #' log_odd <- apply(xf, 1, function(row) theta %*% row)
# #' yf <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))
# #' xnf <- matrix(runif(100 * 2), nrow = 100, ncol = 2)
# #' check_consistency(xf, xnf, yf)
check_consistency <- function(xf, xnf, yf) {
  for (obj in list(xf, xnf)) {
    if (!any(class(obj) %in% c("matrix", "array", "data.frame"))) {
      stop(simpleError(paste0(deparse(substitute(obj)), " must be a matrix or a data.frame object.")))
    }
  }
  if (((class(yf) != "integer") & (class(yf) != "numeric")) | any(levels(factor(yf)) != c("0", "1"))) {
    stop(simpleError("yf must be a 0/1 integer vector."))
  }
  if (ncol(xf) != ncol(xnf)) {
    stop(simpleError("xf and xnf must have the same number of columns."))
  }
  if (nrow(xf) != length(yf)) {
    stop(simpleError("xf and yf must have the same number of rows."))
  }
}
