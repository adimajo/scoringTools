#' @include allClasses.R
NULL

is_speedglm_installed <- function() {
  requireNamespace("speedglm", quietly = TRUE)
}

methods::setMethod("show", methods::signature(object = "reject_infered"), function(object) {
  methods::show(object@infered_model)
})


methods::setMethod(
  "show",
  methods::signature(object = "discretization"),
  function(object) {
    methods::show(object@best.disc[[1]])
  }
)


print.discretization <- function(object) {
  print(object@best.disc[[1]])
}

#' Summary
#'
#' @name summary
#' @description Summary generic.
#' @param object S4 discretization object.
if (!isGeneric("summary")) {
  methods::setGeneric("summary", function(object, ...) standardGeneric("summary"))
}

#' @describeIn summary Summary for the discretization class.
summary.discretization <- function(object) {
  summary(object@best.disc[[1]])
}
methods::setMethod(f = "summary", signature = c(object = "discretization"), definition = summary.discretization)


#' Prediction on a raw test set of the best logistic regression model on discretized data.
#'
#' @name predict
#' @description This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "discretization" object.
#' It then applies the learnt logistic regression model and outputs its prediction (see predict.glm).
#' @param object The S4 discretization object.
#' @param newdata The test dataframe to discretize and for which we wish to have predictions.
if (!isGeneric("predict")) {
  methods::setGeneric("predict", function(object, newdata, ...) standardGeneric("predict"))
}

#' @describeIn predict Prediction on a raw test set of the best logistic regression model on discretized data.
predict.discretization <- function(object, newdata) {
  predict(object = object@best.disc[[1]], newdata = data.frame(discretize_cutp(object@parameters[[1]][object@parameters[[6]][[1]], ], object@best.disc[[2]][["Disc.data"]], newdata)) %>%
    dplyr::mutate_if(is.numeric, as.factor), type = "response")
}
methods::setMethod(f = "predict", signature = c(object = "discretization", newdata = "data.frame"), definition = predict.discretization)
methods::setMethod(f = "predict", signature = c(object = "discretization", newdata = "matrix"), definition = predict.discretization)
methods::setMethod(f = "predict", signature = c(object = "glmORlogicalORspeedglm"), definition = stats::predict)
methods::setMethod(f = "predict", signature = c(object = "glmORlogicalORspeedglm"), definition = stats::predict)
if (is_speedglm_installed()) {
  methods::setMethod(f = "predict", signature = c(object = "speedglm", newdata = "data.frame"), definition = speedglm:::predict.speedglm)
}

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Different kinds of plots using either plotly (if available) or the standard plot (graphics package).
#'
#' @name plot
#' @description This function aims at producing useful graphs in the context of credit scoring in order to simplify the validation process
#' of the produced credit score.
#' @param x S4 discretization object.
#' @param type Type of plot. For now only "ROC" is supported.
#'
if (!isGeneric("plot")) {
  methods::setGeneric("plot", function(x, y, type, ...) standardGeneric("plot"))
}

#' @describeIn plot plots for the S4 discretization object
#' @param x S4 discretization object.
#' @param type Type of plot. For now only "ROC" is supported.
#' @param ... Additional plot parameters.
#' @importFrom magrittr "%>%"
plot.discretization <- function(x, type, ...) {
  if (requireNamespace("plotly", quietly = TRUE)) {

    # The produced graph depends on the 'type' variable
    if (type == "ROC") {
      if (requireNamespace("pROC", quietly = TRUE)) {
        if (x@parameters[[3]] == TRUE) {
          warning("No test data, using validation data might provide an overly optimist estimate of the algorithm's performance.")
        } else {
          warning("No test data, no validation data, using train data will provide an overly optimist estimate of the algorithm's performance.")
        }
        roc_curve <- pROC::roc(x@disc.data$labels, predict(x@best.disc[[1]], x@disc.data %>% dplyr::mutate_if(is.numeric, as.factor), type = "response"))
        plotly_plot <- plotly::plot_ly(data.frame(Specificity = roc_curve$specificities, Sensitivity = roc_curve$sensitivities),
          x = ~ (1 - Specificity), y = ~Sensitivity, hoverinfo = "none",
          height = 600, width = 800
        ) %>%
          plotly::add_lines(
            name = "Model",
            line = list(shape = "spline", color = "#737373", width = 7),
            fill = "tozeroy", fillcolor = "#2A3356"
          ) %>%
          plotly::add_segments(
            x = 0, y = 0, xend = 1, yend = 1,
            line = list(dash = "7px", color = "#F35B25", width = 4),
            name = "Random"
          ) %>%
          plotly::add_segments(
            x = 0, y = 0, xend = 0, yend = 1,
            line = list(dash = "10px", color = "black", width = 4),
            showlegend = F
          ) %>%
          plotly::add_segments(
            x = 0, y = 1, xend = 1, yend = 1,
            line = list(dash = "10px", color = "black", width = 4),
            showlegend = F
          ) %>%
          plotly::add_annotations(
            x = 0.8, y = 0.2, showarrow = F,
            text = paste0("Area Under Curve: ", round(roc_curve$auc, 3)),
            font = list(family = "serif", size = 18, color = "#E8E2E2")
          ) %>%
          plotly::add_annotations(
            x = 0, y = 0.98, showarrow = F, xanchor = "left",
            xref = "paper", yref = "paper",
            text = paste0("Charts the percentage of correctly identified defaults against the percentage of false alarms."),
            font = list(family = "serif", size = 14, color = "#999999")
          ) %>%
          plotly::layout(
            title = "ROC Curve", xaxis = list(
              range = c(0, 1), zeroline = F, showgrid = F,
              title = "1 - Specificity"
            ),
            yaxis = list(
              range = c(0, 1), zeroline = F, showgrid = F,
              domain = c(0, 0.9),
              title = "Sensibility"
            ),
            plot_bgcolor = "#E8E2E2"
          )
        return(plotly_plot)
      }
    }

    if (type == "lift") {

    }

    if (type == "discretization") {

    }

    if (type == "glm") {

    }
  } else {
    for (j in 1:length(x@best.disc[[2]])) {
      graphics::plot(x@best.disc[[1]]$data[j], x@best.disc[[1]][[j]]$data)
    }
  }
}
methods::setMethod(f = "plot", signature = c(x = "discretization", y = "missing", type = "character"), definition = plot.discretization)

#' Generic method "discretize" for discretization objects.
#'
#' This function discretizes a new data set using a previously learnt discretization scheme.
# #' @rdname discretize
# #' @name discretize
# #' @docType methods
#' @exportMethod discretize
#' @param object the S4 discretization object
#' @param data new data to discretize
#' @description This defines the generic method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class discretization.
methods::setGeneric("discretize", function(object, data) attributes(object))

# #' Method for discretizing a new input dataset given a discretization scheme of class "discretization".
# #'
# #' This function discretizes a new data set using a previously learnt discretization scheme.
# #' @rdname discretize-method
# #' @name discretize-method
# #' @aliases discretize,scoringTools-method
# #' @param object the S4 discretization object
# #' @param data new data to discretize
# #' @description This defines the method "discretize" which will discretize a new input dataset given a discretization scheme of S4 class discretization.
#' @rdname discretize
methods::setMethod("discretize", methods::signature(object = "discretization"), function(object, data) {
  # if (substr(object@method.name,1,3)=="sem") {
  #      discretize_link(object@best.disc[[2]],data)
  # } else {
  discretize_cutp(data, object@best.disc[[2]]$Disc.data, object@parameters[[1]][object@parameters[[6]][[1]], ])
  # }
})
