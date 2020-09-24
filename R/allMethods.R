#' @include allClasses.R
NULL

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


summary.discretization <- function(object) {
  summary(object@best.disc[[1]])
}


#' Prediction on a raw test set of the best logistic regression model on discretized data.
#'
#' This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "discretization" object.
#' It then applies the learnt logistic regression model and outputs its prediction (see predict.glm).
#' @param object The S4 discretization object.
#' @param predictors The test dataframe to discretize and for which we wish to have predictions.
#' @keywords test discretization predict prediction

predict.discretization <- function(object, predictors) {
  predict(object@best.disc[[1]], as.data.frame(discretize_cutp(object@parameters[[1]][object@parameters[[6]][[1]], ], object@best.disc[[2]][["Disc.data"]], predictors)))
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
#' This function aims at producing useful graphs in the context of credit scoring in order to simplify the validation process
#' of the produced credit score.
#' @param object The S4 discretization object.
#' @param type The test dataframe to discretize and for which we wish to have predictions.
#' @param ... See additional parameters of plotly (if installed) or the standard plot function (from the graphics package).
#' @keywords test discretization predict prediction
#' @importFrom magrittr "%>%"

plot.discretization <- function(object, type, ...) {
  if (requireNamespace("plotly", quietly = TRUE)) {

    # The produced graph depends on the 'type' variable
    if (type == "ROC") {
      if (requireNamespace("pROC", quietly = TRUE)) {
        if (object@parameters[[1]] == TRUE) {
          roc_curve <- pROC::roc(object@disc.data$labels, predict(object@best.disc[[1]], object@disc.data, type = "response"))
          plotly::plot_ly(data.frame(Specificity = roc_curve$specificities, Sensitivity = roc_curve$sensitivities),
            x = ~ (1 - Specificity), y = ~Sensitivity, hoverinfo = "none",
            height = 600, width = 800
          ) %>%
            plotly::add_lines(
              name = "Model",
              line = list(shape = "spline", color = "#737373", width = 7),
              fill = "tozeroy", fillcolor = "#2A3356"
            ) %>%

            # add_annotations(y = roc_curve$sensitivities, x = 1-roc_curve$specificities, text = sample(1:5, size = 28, replace = T),
            #                 ax = 20, ay = 20,
            #                 arrowcolor = "white",
            #                 arrowhead = 3,
            #                 font = list(color = "white")) %>%

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

            # add_annotations(x = 0, y = 1, showarrow = F, xanchor = "left",
            #                 xref = "paper", yref = "paper",
            #                 text = paste0("Receiver Operator Curve"),
            #                 font = list(family = "arial", size = 30, color = "#595959")) %>%

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
        } else if (object@parameters[[2]] == TRUE) {
          warning("No test data, using validation data might provide an overly optimist estimate of the algorithm's performance.")
          roc_curve <- pROC::roc(object@disc.data$labels, predict(object@best.disc[[1]], object@disc.data, type = "response"))
          plotly::plot_ly(data.frame(Specificity = roc_curve$specificities, Sensitivity = roc_curve$sensitivities),
            x = ~ (1 - Specificity), y = ~Sensitivity, hoverinfo = "none",
            height = 600, width = 800
          ) %>%
            plotly::add_lines(
              name = "Model",
              line = list(shape = "spline", color = "#737373", width = 7),
              fill = "tozeroy", fillcolor = "#2A3356"
            ) %>%

            # add_annotations(y = roc_curve$sensitivities, x = 1-roc_curve$specificities, text = sample(1:5, size = 28, replace = T),
            #                 ax = 20, ay = 20,
            #                 arrowcolor = "white",
            #                 arrowhead = 3,
            #                 font = list(color = "white")) %>%

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

            # add_annotations(x = 0, y = 1, showarrow = F, xanchor = "left",
            #                 xref = "paper", yref = "paper",
            #                 text = paste0("Receiver Operator Curve"),
            #                 font = list(family = "arial", size = 30, color = "#595959")) %>%

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
        } else {
          warning("No test data, no validation data, using train data will provide an overly optimist estimate of the algorithm's performance.")
          roc_curve <- pROC::roc(object@disc.data$labels, predict(object@best.disc[[1]], object@disc.data, type = "response"))
          plotly::plot_ly(data.frame(Specificity = roc_curve$specificities, Sensitivity = roc_curve$sensitivities),
            x = ~ (1 - Specificity), y = ~Sensitivity, hoverinfo = "none",
            height = 600, width = 800
          ) %>%
            plotly::add_lines(
              name = "Model",
              line = list(shape = "spline", color = "#737373", width = 7),
              fill = "tozeroy", fillcolor = "#2A3356"
            ) %>%

            # add_annotations(y = roc_curve$sensitivities, x = 1-roc_curve$specificities, text = sample(1:5, size = 28, replace = T),
            #                 ax = 20, ay = 20,
            #                 arrowcolor = "white",
            #                 arrowhead = 3,
            #                 font = list(color = "white")) %>%

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

            # add_annotations(x = 0, y = 1, showarrow = F, xanchor = "left",
            #                 xref = "paper", yref = "paper",
            #                 text = paste0("Receiver Operator Curve"),
            #                 font = list(family = "arial", size = 30, color = "#595959")) %>%

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
        }
      }
    }

    if (type == "lift") {

    }

    if (type == "discretization") {

    }

    if (type == "glm") {

    }
  } else {
    for (j in 1:length(object@best.disc[[2]])) {
      graphics::plot(object@best.disc[[1]]$data[j], object@best.disc[[1]][[j]]$data)
    }
  }
}

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
  discretize_cutp(data, object@best.disc[[2]]@Disc.data, object@parameters[[1]][object@parameters[[6]][[1]], ])
  # }
})
