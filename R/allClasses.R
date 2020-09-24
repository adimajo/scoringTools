#' Class discretization
#'
#' An S4 class to represent a discretization scheme.
#' @rdname discretization
#' @name discretization
#' @slot method.name The name of the used discretization method.
#' @slot parameters The parameters associated with the used method.
#' @slot best.disc The best discretization scheme found by the method given its parameters.
#' @slot performance The performance obtained with the method given its parameters.
#' @slot disc.data The discretized data: test set if test is TRUE; if test is FALSE and validation is TRUE, then it provides the discretized validation set. Otherwise, it provides the discretized training set.
#' @slot disc.data The continuous data: test set if test is TRUE; if test is FALSE and validation is TRUE, then it provides the discretized validation set. Otherwise, it provides the discretized training set.

methods::setClass("discretization", methods::representation(method.name = "character", parameters = "list", best.disc = "list", performance = "list", disc.data = "data.frame", cont.data = "data.frame"))

if (!requireNamespace("speedglm", quietly = TRUE)) {
  methods::setClassUnion("glmORlogicalORspeedglm", c("glm", "logical"))
} else {
  methods::setOldClass("speedglm")
  methods::setOldClass("speedlm")
  methods::setClassUnion("glmORlogicalORspeedglm", c("glm", "logical", "speedglm", "speedlm"))
}

#' Class reject_infered
#'
#' An S4 class to represent a reject inference technique.
#' @rdname reject_infered-class
#' @name reject_infered-class
#' @slot method_name The name of the used reject inference method.
#' @slot financed_model The logistic regression model on financed clients.
#' @slot acceptance_model The acceptance model (if estimated by the given method).
#' @slot infered_model The logistic regression model resulting from the reject inference method.

methods::setClass("reject_infered", methods::representation(method_name = "character", financed_model = "glmORlogicalORspeedglm", acceptance_model = "glmORlogicalORspeedglm", infered_model = "glmORlogicalORspeedglm"))
