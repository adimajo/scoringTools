#' @include allClasses.R
NULL

is_speedglm_installed <- function() {
  installed <- requireNamespace("speedglm", quietly = TRUE)
  return(installed)
}

is_speedglm_predict_installed <- function() {
  # les_methodes <- methods::findMethods("predict")@signatures
  # for (j in length(les_methodes)) {
  #   present <- FALSE
  #   if ("speedglm" %in% les_methodes[[j]]) {
  #     present <- TRUE
  #   }
  # }
  # return(present)
  return(TRUE)
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
#' @exportMethod summary
#' @name summary
#' @description Summary generic.
#' @param object S4 discretization object.
#' @param ... Other parameters to \code{summary}
methods::setGeneric("summary")

#' Summary for the discretization class.
#' @rdname summary
#' @param object S4 discretization object.
summary.discretization <- function(object) {
  summary(object@best.disc[[1]])
}
#' Summary for the discretization class.
#' @rdname summary
methods::setMethod(f = "summary", signature = c(object = "discretization"), definition = summary.discretization)


#' Prediction on a raw test set of the best logistic regression model on discretized data.
#'
#' @exportMethod predict
#' @name predict
#' @description This function discretizes a user-provided test dataset given a discretization scheme provided by an S4 "discretization" object.
#' It then applies the learnt logistic regression model and outputs its prediction (see predict.glm).
#' @param object The S4 discretization object.
#' @param newdata The test dataframe to discretize and for which we wish to have predictions.
#' @param ... Other parameters to \code{predict}
# if (!methods::isGeneric("predict")) {
methods::setGeneric("predict")
# }

#' Prediction on a raw test set of the best logistic regression model on discretized data.
#' @rdname predict
#' @param object The S4 discretization object.
#' @param newdata The test dataframe to discretize and for which we wish to have predictions.
predict.discretization <- function(object, newdata) {
  df_to_predict <- data.frame(
    discretize_cutp(
      cont_test_set = newdata,
      disc_train_set = object@best.disc[[2]][["Disc.data"]],
      cont_train_set = object@parameters[[1]][object@parameters[[6]][[1]], ]
    )
  ) %>%
    dplyr::mutate_if(is.numeric, as.factor)
  return(predict(
    object = object@best.disc[[1]],
    newdata = df_to_predict, type = "response"
  ))
}

#' Prediction on a raw test set of the logistic regression model after reject inference.
#' @rdname predict
#' @param object The S4 reject_infered object.
#' @param newdata The test dataframe to discretize and for which we wish to have predictions.
#' @param ... Additional parameters to pass on to base predict.
predict.reject_infered <- function(object, newdata, ...) {
  if (object@method_name == "twins") {
    newdata <- data.frame(x = newdata)
    newdata$score_acc <- predict(object@acceptance_model, newdata = newdata)
    newdata$score_def <- predict(object@financed_model, newdata = newdata)
    return(predict(object = object@infered_model, newdata = newdata, ...))
  } else {
    return(predict(object = object@infered_model, newdata = data.frame(x = newdata), ...))
  }
}

#' Prediction on a raw test set of the best logistic regression model on discretized data.
#' @rdname predict
methods::setMethod(f = "predict", signature = c(object = "discretization"), definition = predict.discretization)

#' Prediction on a raw test set of the logistic regression model after reject inference.
#' @rdname predict
methods::setMethod(f = "predict", signature = c(object = "reject_infered"), definition = predict.reject_infered)

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
#' @exportMethod plot
#' @description This function aims at producing useful graphs in the context of credit scoring in order to simplify the validation process
#' of the produced credit score.
#' @param x S4 discretization object.
#' @param y (For standard plots only)
#' @param ... (For standard plots only)
#' @param type Type of plot. For now only "ROC" is supported.
methods::setGeneric("plot")

is_plotly_installed <- function() {
  installed <- requireNamespace("plotly", quietly = TRUE)
  return(installed)
}

is_pROC_installed <- function() {
  installed <- requireNamespace("pROC", quietly = TRUE)
  return(installed)
}

#' Different kinds of plots using either plotly (if available) or the standard plot (graphics package).
#'
#' @rdname plot
#' @param x S4 discretization object.
#' @param type Type of plot. For now only "ROC" is supported.
#' @importFrom magrittr "%>%"
plot.discretization <- function(x, type) {
  if (is_plotly_installed()) {

    # The produced graph depends on the 'type' variable
    if (type == "ROC") {
      if (is_pROC_installed()) {
        if (x@parameters[[2]] == FALSE & x@parameters[[3]] == TRUE) {
          warning("No test data, using validation data might provide an overly optimist estimate of the algorithm's performance.")
        } else {
          if (x@parameters[[2]] == FALSE & x@parameters[[3]] == FALSE) {
            warning("No test data, no validation data, using train data will provide an overly optimist estimate of the algorithm's performance.")
          }
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
      } else {
        stop(simpleError("Package 'pROC' suggested but not installed, cannot use type='ROC'."))
      }
    }

    if (type == "lift") {
      stop(simpleError("Not implemented."))
    }

    if (type == "discretization") {
      stop(simpleError("Not implemented."))
    }

    if (type == "glm") {
      stop(simpleError("Not implemented."))
    }
  } else {
    warning("Package 'plotly' suggested but not installed, falling back to 'graphics' package.")
    if (type == "ROC") {
      if (is_pROC_installed()) {
        if (x@parameters[[2]] == FALSE & x@parameters[[3]] == TRUE) {
          warning("No test data, using validation data might provide an overly optimist estimate of the algorithm's performance.")
        } else {
          if (x@parameters[[2]] == FALSE & x@parameters[[3]] == FALSE) {
            warning("No test data, no validation data, using train data will provide an overly optimist estimate of the algorithm's performance.")
          }
        }
        roc_curve <- pROC::roc(x@disc.data$labels, predict(x@best.disc[[1]], x@disc.data %>% dplyr::mutate_if(is.numeric, as.factor), type = "response"))
        return(roc_curve)
      } else {
        stop(simpleError("Package 'pROC' suggested but not installed, cannot use type='ROC'."))
      }
    }

    if (type == "lift") {
      stop(simpleError("Not implemented."))
    }

    if (type == "discretization") {
      stop(simpleError("Not implemented."))
    }

    if (type == "glm") {
      graphics::plot(x@best.disc[[1]])
    }
  }
}

#' Different kinds of plots using either plotly (if available) or the standard plot (graphics package).
#'
#' @rdname plot
methods::setMethod("plot", "discretization", plot.discretization)

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
