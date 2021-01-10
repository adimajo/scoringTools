fit_disc <- function(disc, data_train, type = "glm") {
  if (type == "glm") {
    return(
      stats::glm(
        formula = stats::formula("labels ~ ."),
        family = stats::binomial(link = "logit"),
        data = Filter(
          function(x) (length(unique(x)) > 1),
          cbind(
            data.frame(sapply(
              disc$Disc.data,
              as.factor
            ),
            stringsAsFactors = TRUE
            ),
            data_train[, sapply(
              data_train,
              is.factor
            ),
            drop = FALSE
            ]
          )
        ),
        weights = NULL
      )
    )
  } else {
    return(
      speedglm::speedglm(
        formula = stats::formula("labels ~ ."),
        family = stats::binomial(link = "logit"),
        data = Filter(
          function(x) (length(unique(x)) > 1),
          cbind(
            data.frame(sapply(
              disc$Disc.data,
              as.factor
            ),
            stringsAsFactors = TRUE
            ),
            data_train[, sapply(
              data_train,
              is.factor
            ),
            drop = FALSE
            ]
          )
        ),
        fitted = TRUE,
        weights = NULL
      )
    )
  }
}


compute_data_val_test <- function(row_subset_val_test, row_subset_train, predictors, disc) {
  return(
    cbind(
      data.frame(
        sapply(
          data.frame(discretize_cutp(
            predictors[row_subset_val_test, ] %>%
              dplyr::as_tibble(.name_repair = "unique") %>%
              dplyr::select_if(function(col) !is.factor(col)) %>%
              as.data.frame(),
            disc[["Disc.data"]],
            predictors[row_subset_train, ] %>%
              dplyr::as_tibble(.name_repair = "unique") %>%
              dplyr::select_if(function(col) !is.factor(col)) %>%
              as.data.frame()
          )),
          as.factor
        ),
        stringsAsFactors = TRUE
      ),
      predictors[row_subset_val_test, ] %>%
        dplyr::as_tibble(.name_repair = "unique") %>%
        dplyr::select_if(function(col) is.factor(col)) %>%
        as.data.frame()
    )
  )
}

output_disc <- function(method.name,
                        predictors,
                        labels,
                        test,
                        validation,
                        criterion,
                        param,
                        ensemble,
                        best.disc,
                        performance) {
  if (test & validation) {
    return(methods::new(
      Class = "discretization",
      method.name = method.name,
      parameters = list(
        predictors,
        test,
        validation,
        criterion,
        param,
        ensemble
      ),
      best.disc = best.disc,
      performance = list(performance),
      disc.data = cbind(compute_data_val_test(ensemble[[3]], ensemble[[1]], predictors, best.disc[[2]]),
        labels = labels[ensemble[[3]]]
      ),
      cont.data = data.frame(predictors[ensemble[[3]], ],
        labels = labels[ensemble[[3]]]
      )
    ))
  } else if (test | validation) {
    return(methods::new(
      Class = "discretization",
      method.name = method.name,
      parameters = list(
        predictors,
        test,
        validation,
        criterion,
        param,
        ensemble
      ),
      best.disc = best.disc,
      performance = list(performance),
      disc.data = cbind(compute_data_val_test(ensemble[[2]], ensemble[[1]], predictors, best.disc[[2]]),
        labels = labels[ensemble[[2]]]
      ),
      cont.data = data.frame(predictors[ensemble[[2]], ],
        labels = labels[ensemble[[2]]]
      )
    ))
  } else {
    return(methods::new(
      Class = "discretization",
      method.name = method.name,
      parameters = list(
        predictors,
        test,
        validation,
        criterion,
        param,
        ensemble
      ),
      best.disc = best.disc,
      performance = list(performance),
      disc.data = cbind(compute_data_val_test(ensemble[[1]], ensemble[[1]], predictors, best.disc[[2]]),
        labels = labels[ensemble[[1]]]
      ),
      cont.data = data.frame(predictors[ensemble[[1]], ],
        labels = labels[ensemble[[1]]]
      )
    ))
  }
}

calculate_performance <- function(test, validation, criterion, ensemble, predictors, labels, best.disc) {
  if (test) {
    if (criterion == "gini") {
      if (validation) {
        data_test <- compute_data_val_test(ensemble[[3]], ensemble[[1]], predictors, best.disc[[2]])
        performance <- normalizedGini(labels[ensemble[[3]]], predict(best.disc[[1]], data_test, type = "response"))
      } else {
        data_test <- compute_data_val_test(ensemble[[2]], ensemble[[1]], predictors, best.disc[[2]])

        performance <- normalizedGini(labels[ensemble[[2]]], predict(best.disc[[1]], data_test, type = "response"))
      }
    } else {
      if (validation) performance <- 0 else performance <- 0
    }
  } else {
    if (criterion == "gini") {
      if (validation) {
        data_validation <- compute_data_val_test(ensemble[[2]], ensemble[[1]], predictors, best.disc[[2]])
        performance <- normalizedGini(labels[ensemble[[2]]], predict(best.disc[[1]], data_validation, type = "response"))
      } else {
        if (!(is_speedglm_installed() & is_speedglm_predict_installed())) {
          performance <- normalizedGini(labels[ensemble[[1]]], best.disc[[1]]$fitted.values)
        } else {
          performance <- normalizedGini(labels[ensemble[[1]]], best.disc[[1]]$linear.predictors)
        }
      }
    } else {
      if (validation) performance <- 0 else performance <- best.disc[[1]]$aic
    }
  }
}

calculate_criterlist <- function(predictors,
                                 labels,
                                 validation,
                                 criterion,
                                 ensemble,
                                 disc,
                                 logit) {
  if (validation) {
    data_validation <- cbind(
      data.frame(
        sapply(
          data.frame(discretize_cutp(
            predictors[ensemble[[2]], ] %>%
              dplyr::as_tibble(.name_repair = "unique") %>%
              dplyr::select_if(function(col) !is.factor(col)) %>%
              as.data.frame(),
            disc[["Disc.data"]],
            predictors[ensemble[[1]], ] %>%
              dplyr::as_tibble(.name_repair = "unique") %>%
              dplyr::select_if(function(col) !is.factor(col)) %>%
              as.data.frame()
          )),
          as.factor
        ),
        stringsAsFactors = TRUE
      ),
      predictors[ensemble[[2]], ] %>%
        dplyr::as_tibble(.name_repair = "unique") %>%
        dplyr::select_if(function(col) is.factor(col)) %>%
        as.data.frame()
    )
    if (criterion == "gini") {
      return(normalizedGini(labels[ensemble[[2]]], predict(logit, data_validation, type = "response")))
    } else {
      return(logit$aic)
    }
  } else {
    if (criterion == "gini") {
      if (!is_speedglm_installed()) {
        return(normalizedGini(labels[ensemble[[1]]], logit$fitted.values))
      } else {
        return(normalizedGini(labels[ensemble[[1]]], logit$linear.predictors))
      }
    } else {
      return(logit$aic)
    }
  }
}
