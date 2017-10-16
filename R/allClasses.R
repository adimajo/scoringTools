

#' An S4 class to represent a discretization scheme.
#'
#' @slot method.name The name of the used discretization method.
#' @slot parameters The parameters associated with the used method.
#' @slot best.disc The best discretization scheme found by the method given its parameters.
#' @slot performance The performance obtained with the method given its parameters.

methods::setClass("discretization", representation(method.name = "character", parameters = "list", best.disc = "list", performance = "list"))

#' An S4 class to represent a reject inference technique.
#'
#' @slot method_name The name of the used reject inference method.
#' @slot financed_model The logistic regression model on financed clients.
#' @slot acceptance_model The acceptance model (if estimated by the given method).
#' @slot infered_model The logistic regression model resulting from the reject inference method.

if (requireNamespace("speedglm", quietly = TRUE)) {
     methods::setClassUnion("glmORlogicalORspeedglm", c("glm", "speedglm","logical"))
} else {
     methods::setClassUnion("glmORlogicalORspeedglm", c("glm","logical"))
}

methods::setClass("reject_infered", representation(method_name = "character", financed_model = "glmORlogicalORspeedglm", acceptance_model = "glmORlogicalORspeedglm", infered_model = "glmORlogicalORspeedglm"))

