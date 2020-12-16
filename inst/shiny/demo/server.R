server <- function(input, output, session) {
  library(dplyr)
  list_imported_df <- shiny::reactiveValues()

  shiny::observe({
    if (input$use_lendingClub) {
      list_imported_df[["lendingClub"]] <- scoringTools::lendingClub
    } else {
      tryCatch(
        .subset2(list_imported_df, "lendingClub")$.values$remove("name"),
        error = function(e) {
        }
      )
    }
  })

  shiny::observe({
    if (input$use_MAR_well_specified) {
      list_imported_df[["MAR_well_specified"]] <-
        scoringTools::generate_data(
          n = input$n_MAR_well_specified,
          d = input$dim_MAR_well_specified,
          type = "MAR well specified"
        )
    } else {
      tryCatch(
        .subset2(list_imported_df, "MAR_well_specified")$.values$remove("name"),
        error = function(e) {
        }
      )
    }
  })

  shiny::observe({
    if (input$use_MAR_misspecified) {
      list_imported_df[["MAR_misspecified"]] <-
        scoringTools::generate_data(
          n = input$n_MAR_misspecified,
          d = input$dim_MAR_misspecified,
          type = "MAR misspecified"
        )
    } else {
      tryCatch(
        .subset2(list_imported_df, "MAR_misspecified")$.values$remove("name"),
        error = function(e) {
        }
      )
    }
  })

  shiny::observe({
    if (input$use_MNAR) {
      list_imported_df[["MNAR"]] <- scoringTools::generate_data(
        n = input$n_MNAR,
        d = input$dim_MNAR,
        type = "MNAR"
      )
    } else {
      tryCatch(
        .subset2(list_imported_df, "MNAR")$.values$remove("name"),
        error = function(e) {
        }
      )
    }
  })

  # Fonction d'import de données
  shiny::observeEvent(
    (!is.null(input$imported_files)) |
      (!is.null(input$use_lendingClub)) |
      (!is.null(input$use_MNAR)) |
      (!is.null(input$use_MAR_well_specified)) |
      (!is.null(input$use_MAR_misspecified)) |
      unlist(lapply(1:length(input$imported_files[, 1]), function(i) {
        input[[paste0("good_to_go_", i)]] > 0
      })),
    shiny::updateSelectInput(
      session = session,
      "selectedDataRejectInference",
      choices = names(list_imported_df)
    )
  )
  shiny::observeEvent(
    (!is.null(input$imported_files)) |
      (!is.null(input$use_lendingClub)) |
      (!is.null(input$use_MNAR)) |
      (!is.null(input$use_MAR_well_specified)) |
      (!is.null(input$use_MAR_misspecified)) |
      unlist(lapply(1:length(input$imported_files[, 1]), function(i) {
        input[[paste0("good_to_go_", i)]] > 0
      })),
    shiny::updateSelectInput(
      session = session,
      "selectedDataQuantization",
      choices = names(list_imported_df)
    )
  )
  shiny::observeEvent(
    (!is.null(input$imported_files)) |
      (!is.null(input$use_lendingClub)) |
      (!is.null(input$use_MNAR)) |
      (!is.null(input$use_MAR_well_specified)) |
      (!is.null(input$use_MAR_misspecified)) |
      unlist(lapply(1:length(input$imported_files[, 1]), function(i) {
        input[[paste0("good_to_go_", i)]] > 0
      })),
    shiny::updateSelectInput(
      session = session,
      "selectedDataLogisticRegressionTrees",
      choices = names(list_imported_df)
    )
  )

  shiny::observeEvent(
    {
      (!is.null(input$selectedDataRejectInference)) &
        (!is.null(colnames(list_imported_df[[input$selectedDataRejectInference]])))
    },
    shiny::updateSelectInput(
      session = session,
      "var_cible",
      choices = colnames(list_imported_df[[input$selectedDataRejectInference]])
    )
  )
  shiny::observeEvent(
    {
      (!is.null(input$selectedDataQuantization)) &
        (!is.null(colnames(list_imported_df[[input$selectedDataQuantization]])))
    },
    shiny::updateSelectInput(
      session = session,
      "var_cible_quantization",
      choices = colnames(list_imported_df[[input$selectedDataQuantization]])
    )
  )
  shiny::observeEvent(
    (!is.null(input$selectedDataRejectInference)) &
      (!is.null(colnames(
        list_imported_df[[input$selectedDataRejectInference]]
      ))),
    shiny::updateSelectInput(
      session = session,
      "var_reject",
      choices = colnames(list_imported_df[[input$selectedDataRejectInference]])
    )
  )
  shiny::observe({
    if (!is.null(input$imported_files)) {
      lapply(1:length(input$imported_files[, 1]), function(i) {
        shiny::observeEvent(
          list_imported_df[[input$imported_files[i, "name"]]],
          shiny::updateSelectInput(
            session = session,
            paste0("columns_", i),
            choices = c(All = "all", colnames(list_imported_df[[input$imported_files[i, "name"]]]))
          )
        )
      })
    }
  })

  output$data_import_options <- shiny::renderUI({
    shiny::fluidPage(
      shiny::conditionalPanel(
        condition = "input.use_lendingClub",
        # Horizontal line ----
        shiny::tags$hr(),
        "Settings for lendingClub",
        # Input: Checkbox if file has header ----
        shiny::radioButtons(
          paste0("disp_", "lendingClub"),
          "Display",
          choices = c(
            Head = "head",
            All = "all"
          ),
          selected = "head"
        ),
      ),
      shiny::conditionalPanel(
        condition = "input.use_MAR_well_specified",
        # Horizontal line ----
        shiny::tags$hr(),
        "Settings for MAR well-specified",
        # Input: Checkbox if file has header ----
        shiny::radioButtons(
          paste0("disp_", "MAR_well_specified"),
          "Display",
          choices = c(
            Head = "head",
            All = "all"
          ),
          selected = "head"
        ),
      ),
      shiny::conditionalPanel(
        condition = "input.use_MAR_misspecified",
        # Horizontal line ----
        shiny::tags$hr(),
        "Settings for MAR misspecified",
        # Input: Checkbox if file has header ----
        shiny::radioButtons(
          paste0("disp_", "MAR_misspecified"),
          "Display",
          choices = c(
            Head = "head",
            All = "all"
          ),
          selected = "head"
        ),
      ),
      shiny::conditionalPanel(
        condition = "input.use_MNAR",
        # Horizontal line ----
        shiny::tags$hr(),
        "Settings for MNAR",
        # Input: Checkbox if file has header ----
        shiny::radioButtons(
          paste0("disp_", "MNAR"),
          "Display",
          choices = c(
            Head = "head",
            All = "all"
          ),
          selected = "head"
        ),
      ),

      if (!is.null(input$imported_files)) {
        lapply(1:length(input$imported_files[, 1]), function(i) {
          shiny::fluidPage(
            # Horizontal line ----
            shiny::tags$hr(),
            paste0("Settings for file ", input$imported_files[i, "name"]),
            # Input: Checkbox if file has header ----
            shiny::checkboxInput(
              paste0("header_", i),
              "Header",
              TRUE
            ),

            # Input: Select separator ----
            shiny::radioButtons(
              paste0("sep_", i),
              "Separator",
              choices = c(
                Comma = ",",
                Semicolon = ";",
                Tab = "\t"
              ),
              selected = ","
            ),

            # Input: Select quotes ----
            shiny::radioButtons(
              paste0("quote_", i),
              "Quote",
              choices = c(
                None = "",
                "Double Quote" = '"',
                "Single Quote" = "'"
              ),
              selected = '"'
            ),

            # Input: Select number of rows to display ----
            shiny::radioButtons(
              paste0("disp_", i),
              "Display",
              choices = c(
                Head = "head",
                All = "all"
              ),
              selected = "head"
            ),

            shiny::selectInput(
              paste0("columns_", i),
              "Columns to keep",
              choices = c(All = "all"),
              selected = "all",
              multiple = TRUE
            ),

            shiny::actionButton(
              paste0("good_to_go_", i),
              "Test these settings!"
            )
          )
        })
      }
    )
  })

  shiny::observe({
    lapply(1:length(input$imported_files[, 1]), function(i) {
      shiny::observeEvent(
        {
          input[[paste0("good_to_go_", i)]]
        },
        {
          list_imported_df[[input$imported_files[[i, "name"]]]] <- read.csv(
            input$imported_files[[i, "datapath"]],
            header = input[[paste0("header_", i)]],
            sep = input[[paste0("sep_", i)]],
            quote = input[[paste0("quote_", i)]]
          )
          if (!"all" %in% input[[paste0("columns_", i)]]) {
            list_imported_df[[input$imported_files[[i, "name"]]]] <-
              list_imported_df[[input$imported_files[[i, "name"]]]][, input[[paste0("columns_", i)]]]
          }
        },
        event.quoted = FALSE,
        ignoreInit = FALSE
      )
    })
  })

  output$contents <- shiny::renderUI({
    shiny::fluidPage(
      lapply(list(
        "lendingClub",
        "MAR_well_specified",
        "MAR_misspecified",
        "MNAR"
      ), function(tab) {
        if (input[[paste0("use_", tab)]]) {
          shiny::renderTable({
            data <- list_imported_df[[tab]]
            if (!is.null(data)) {
              if (input[[paste0("disp_", tab)]] == "head") {
                return(head(data))
              } else {
                return(data)
              }
            }
          })
        }
      }),
      if (!is.null(input$imported_files)) {
        lapply(1:length(input$imported_files[, 1]), function(i) {
          shiny::renderTable({
            data <- list_imported_df[[input$imported_files[[i, "name"]]]]
            if (!is.null(data) &
              input[[paste0("good_to_go_", i)]] > 0) {
              if (input[[paste0("disp_", i)]] == "head") {
                return(head(data))
              }
              else {
                return(data)
              }
            }
          })
        })
      }
    )
  })

  # Réintégration des refusés
  ## Courbe ROC avec tout le monde
  data_reject_inference <- reactive({
    data <- list_imported_df[[input$selectedDataRejectInference]]
    int_f <- 1:nrow(data) %in% sample.int(nrow(data),
      size = (100 - input$bins_reject) / 100 * nrow(data)
    )
    int_test <- 1:nrow(data) %in% sample.int(nrow(data),
      size = (input$bins_test) / 100 * nrow(data)
    )
    int_nf <- !int_f
    int_train <- !int_test
    x_train <-
      data[int_train, !colnames(data) == input$var_cible, drop = FALSE]
    x_f_train <-
      data[int_f &
        int_train, !colnames(data) == input$var_cible, drop = FALSE]
    x_nf_train <-
      data[int_nf &
        int_train, !colnames(data) == input$var_cible, drop = FALSE]
    y_train <- data[int_train, input$var_cible]
    y_f_train <- data[int_f & int_train, input$var_cible]
    y_nf_train <- data[int_nf & int_train, input$var_cible]
    data_train <- data[int_train, ]
    data_f_train <- data[int_f & int_train, ]
    x_test <-
      data[int_test, !colnames(data) == input$var_cible, drop = FALSE]
    x_f_test <-
      data[int_f &
        int_test, !colnames(data) == input$var_cible, drop = FALSE]
    x_nf_test <-
      data[int_nf &
        int_test, !colnames(data) == input$var_cible, drop = FALSE]
    y_test <- data[int_test, input$var_cible]
    y_f_test <- data[int_f & int_test, input$var_cible]
    y_nf_test <- data[int_nf & int_test, input$var_cible]
    data_test <- data[int_test, ]
    data_f_test <- data[int_f & int_test, ]
    data_nf_test <- data[int_nf & int_test, ]
    data_nf_train <- data[int_nf & int_train, ]

    if (input$deleteSamplesRejectInference) {
      levels_in_train <-
        lapply(data_f_train[, sapply(data_f_train, is.factor)], function(fct) {
          levels(factor(fct))
        })
      levels_in_test <-
        lapply(data_test[, sapply(data_test, is.factor)], function(fct) {
          levels(factor(fct))
        })
      levels_in_train_nf_f <-
        lapply(data_train[, sapply(data_train, is.factor)], function(fct) {
          levels(factor(fct))
        })

      if (length(levels_in_train) > 0) {
        levels_to_delete <-
          sapply(1:length(levels_in_train), function(idx) {
            if (length(which(!(
              levels_in_test[[idx]] %in% levels_in_train[[idx]]
            ))) > 0) {
              return(levels_in_test[[idx]][which(!(levels_in_test[[idx]] %in% levels_in_train[[idx]]))])
            } else {
              return(NULL)
            }
          })
        levels_to_delete_train <-
          sapply(1:length(levels_in_train), function(idx) {
            if (length(which(!(
              levels_in_train_nf_f[[idx]] %in% levels_in_train[[idx]]
            ))) > 0) {
              return(levels_in_train_nf_f[[idx]][which(!(levels_in_train_nf_f[[idx]] %in% levels_in_train[[idx]]))])
            } else {
              return(NULL)
            }
          })

        rows_to_delete_train <- sapply(
          1:sum(sapply(data_train, is.factor)),
          function(idx) {
            data_train[, sapply(data_train, is.factor)][, idx] %in% levels_to_delete_train[[idx]]
          }
        )
        rows_to_delete <- sapply(
          1:sum(sapply(data_test, is.factor)),
          function(idx) {
            data_test[, sapply(data_test, is.factor)][, idx] %in% levels_to_delete[[idx]]
          }
        )
        rows_to_delete_f <- sapply(
          1:sum(sapply(data_f_test, is.factor)),
          function(idx) {
            data_f_test[, sapply(data_f_test, is.factor)][, idx] %in% levels_to_delete[[idx]]
          }
        )
        rows_to_delete_nf <- sapply(
          1:sum(sapply(data_nf_test, is.factor)),
          function(idx) {
            data_nf_test[, sapply(data_nf_test, is.factor)][, idx] %in% levels_to_delete[[idx]]
          }
        )
        rows_to_delete_nf_train <- sapply(
          1:sum(sapply(data_nf_train, is.factor)),
          function(idx) {
            data_nf_train[, sapply(data_nf_train, is.factor)][, idx] %in% levels_to_delete_train[[idx]]
          }
        )

        data_test <-
          data_test[Matrix::rowSums(rows_to_delete) == 0, ]
        data_train <-
          data_train[Matrix::rowSums(rows_to_delete_train) == 0, ]
        x_nf_train <-
          x_nf_train[Matrix::rowSums(rows_to_delete_nf_train) == 0, ,
            drop = FALSE
          ]
        x_test <-
          x_test[Matrix::rowSums(rows_to_delete) == 0, ]
        # x_train <-
        #   x_train[Matrix::rowSums(rows_to_delete_train) == 0,]
        x_f_test <-
          x_f_test[Matrix::rowSums(rows_to_delete_f) == 0, ,
            drop = FALSE
          ]
        x_nf_test <-
          x_nf_test[Matrix::rowSums(rows_to_delete_nf) == 0, ,
            drop = FALSE
          ]
        y_test <- y_test[Matrix::rowSums(rows_to_delete) == 0]
        y_train <- y_train[Matrix::rowSums(rows_to_delete_train) == 0]
        y_f_test <- y_f_test[Matrix::rowSums(rows_to_delete_f) == 0]
        y_nf_test <- y_nf_test[Matrix::rowSums(rows_to_delete_nf) == 0]
        data_f_test <- data_f_test[Matrix::rowSums(rows_to_delete_f) == 0, ]
        warning("Deleted samples due to factor levels in test set not in train financed set.")
      }
    }

    return(
      list(
        data_train = data_train,
        data_f_train = data_f_train,
        data_test = data_test,
        data_f_test = data_f_test,
        x_f_test,
        x_nf_test,
        y_f_test,
        y_nf_test,
        x_f_train,
        x_nf_train,
        y_f_train,
        y_nf_train
      )
    )
  })

  model_reject_inference <- reactive({
    list_to_parse <- data_reject_inference()
    data_train <- list_to_parse[[1]]
    data_f_train <- list_to_parse[[2]]
    data_test <- list_to_parse[[3]]
    data_f_test <- list_to_parse[[4]]
    x_f_test <- list_to_parse[[5]]
    x_nf_test <- list_to_parse[[6]]
    y_f_test <- list_to_parse[[7]]
    y_nf_test <- list_to_parse[[8]]
    x_f_train <- list_to_parse[[9]]
    x_nf_train <- list_to_parse[[10]]
    y_f_train <- list_to_parse[[11]]
    y_nf_train <- list_to_parse[[12]]

    list_models <- list()
    roc_curves <- list()
    roc_curves_financed <- list()
    list_gini_test <- list()
    list_gini_test_financed <- list()
    list_gini_train <- list()
    list_gini_train_financed <- list()
    for (model in (input$modelsRejectInference)) {
      switch(
        model,
        logistic = {
          list_models[[model]] <-
            stats::glm(
              as.formula(paste(input$var_cible, "~ .")),
              data = data_f_train,
              family = stats::binomial(link = "logit")
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            predict(list_models[[model]],
              data_test,
              type = "response"
            )
          )
          roc_curves_financed[[model]] <- pROC::roc(
            data_f_test[[input$var_cible]],
            predict(list_models[[model]],
              data_f_test,
              type = "response"
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            predict(list_models[[model]],
              data_test,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_test_financed[[model]] <- pROC::ci.auc(data_f_test[[input$var_cible]],
            predict(list_models[[model]],
              data_f_test,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(list_models[[model]],
              data_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train_financed[[model]] <- pROC::ci.auc(data_f_train[[input$var_cible]],
            predict(list_models[[model]],
              data_f_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
        },
        tree = {
          if (!requireNamespace("rpart", quietly = TRUE)) {
            print(warning(
              "Package rpart not installed, please install it to proceed."
            ))
          }
          list_models[[model]] <-
            rpart::rpart(as.formula(paste(input$var_cible, "~ .")),
              data = data_f_train,
              method = "class"
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test
            )[, 2]
          )
          roc_curves_financed[[model]] <- pROC::roc(
            data_f_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_f_test
            )[, 2]
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test
            )[, 2],
            conf.level = input$confidence_level_reject
          )
          list_gini_test_financed[[model]] <- pROC::ci.auc(data_f_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_f_test
            )[, 2],
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(
              list_models[[model]],
              data_train
            )[, 2],
            conf.level = input$confidence_level_reject
          )
          list_gini_train_financed[[model]] <- pROC::ci.auc(data_f_train[[input$var_cible]],
            predict(
              list_models[[model]],
              data_f_train
            )[, 2],
            conf.level = input$confidence_level_reject
          )
        },
        rforest = {
          if (!requireNamespace("randomForest", quietly = TRUE)) {
            print(warning(
              "Package randomForest not installed, please install it to proceed."
            ))
          }
          data_temp <- data_f_train
          data_temp[, input$var_cible] <-
            factor(data_f_train[, input$var_cible])
          list_models[[model]] <- randomForest::randomForest(
            as.formula(paste(input$var_cible, "~ .")),
            data = data_temp,
            ntree = input$rforestParam_ntree,
            mtry = input$rforestParam_mtry,
            replace = input$rforestParam_replace,
            maxnodes = input$rforestParam_maxnodes
          )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            predict(list_models[[model]],
              data_test,
              type = "prob"
            )[, 2]
          )
          roc_curves_financed[[model]] <- pROC::roc(
            data_f_test[[input$var_cible]],
            predict(list_models[[model]],
              data_f_test,
              type = "prob"
            )[, 2]
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            predict(list_models[[model]],
              data_test,
              type = "prob"
            )[, 2],
            conf.level = input$confidence_level_reject
          )
          list_gini_test_financed[[model]] <- pROC::ci.auc(data_f_test[[input$var_cible]],
            predict(list_models[[model]],
              data_f_test,
              type = "prob"
            )[, 2],
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(list_models[[model]],
              data_train,
              type = "prob"
            )[, 2],
            conf.level = input$confidence_level_reject
          )
          list_gini_train_financed[[model]] <- pROC::ci.auc(data_f_train[[input$var_cible]],
            predict(list_models[[model]],
              data_f_train,
              type = "prob"
            )[, 2],
            conf.level = input$confidence_level_reject
          )
        },
        svm = {
          if (!requireNamespace("e1071", quietly = TRUE)) {
            print(warning(
              "Package e1071 not installed, please install it to proceed."
            ))
          }
          list_models[[model]] <-
            e1071::svm(
              as.formula(paste(input$var_cible, "~ .")),
              data = data_f_train,
              kernel = input$svmParam_kernel,
              degree = input$svmParam_degree,
              gamma = input$svmParam_gamma,
              coef0 = input$svmParam_coef0,
              type = "C-classification",
              probability = TRUE
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            attr(
              predict(list_models[[model]],
                data_test,
                probability = TRUE
              ),
              "probabilities"
            )[, 1]
          )
          roc_curves_financed[[model]] <- pROC::roc(
            data_f_test[[input$var_cible]],
            attr(
              predict(list_models[[model]],
                data_f_test,
                probability = TRUE
              ),
              "probabilities"
            )[, 1]
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            attr(
              predict(list_models[[model]],
                data_test,
                probability = TRUE
              ),
              "probabilities"
            )[, 1],
            conf.level = input$confidence_level_reject
          )
          list_gini_test_financed[[model]] <- pROC::ci.auc(data_f_test[[input$var_cible]],
            attr(
              predict(list_models[[model]],
                data_f_test,
                probability = TRUE
              ),
              "probabilities"
            )[, 1],
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            attr(
              predict(list_models[[model]],
                data_train,
                probability = TRUE
              ),
              "probabilities"
            )[, 1],
            conf.level = input$confidence_level_reject
          )
          list_gini_train_financed[[model]] <- pROC::ci.auc(data_f_train[[input$var_cible]],
            attr(
              predict(list_models[[model]],
                data_f_train,
                probability = TRUE
              ),
              "probabilities"
            )[, 1],
            conf.level = input$confidence_level_reject
          )
        },
        nnet = {
          if (!requireNamespace("nnet", quietly = TRUE)) {
            print(warning(
              "Package nnet not installed, please install it to proceed."
            ))
          }
          list_models[[model]] <-
            nnet::nnet(
              as.formula(paste(input$var_cible, "~ .")),
              data = data_f_train,
              size = input$nnetParam_nnet,
              decay = input$nnetParam_decay,
              maxit = input$nnetParam_maxit
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test
            )[, 1]
          )
          roc_curves_financed[[model]] <- pROC::roc(
            data_f_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_f_test
            )[, 1]
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test
            )[, 1],
            conf.level = input$confidence_level_reject
          )
          list_gini_test_financed[[model]] <- pROC::ci.auc(data_f_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_f_test
            )[, 1],
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(
              list_models[[model]],
              data_train
            )[, 1],
            conf.level = input$confidence_level_reject
          )
          list_gini_train_financed[[model]] <- pROC::ci.auc(data_f_train[[input$var_cible]],
            predict(
              list_models[[model]],
              data_f_train
            )[, 1],
            conf.level = input$confidence_level_reject
          )
        },
        print("no model specified yet")
      )
    }
    for (model in (input$reject)) {
      switch(
        model,
        augmentation = {
          list_models[[model]] <- scoringTools::augmentation(
            x_f_train,
            x_nf_train,
            y_f_train
          )
          roc_curves[[model]] <- pROC::roc(
            c(y_f_test, y_nf_test),
            predict(list_models[[model]],
              rbind(x_f_test, x_nf_test),
              type = "response"
            )
          )
          roc_curves_financed[[model]] <- pROC::roc(
            y_f_test,
            predict(list_models[[model]],
              x_f_test,
              type = "response"
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(c(y_f_test, y_nf_test),
            predict(list_models[[model]],
              rbind(x_f_test, x_nf_test),
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_test_financed[[model]] <- pROC::ci.auc(y_f_test,
            predict(list_models[[model]],
              x_f_test,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(list_models[[model]],
              data_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train_financed[[model]] <- pROC::ci.auc(data_f_train[[input$var_cible]],
            predict(list_models[[model]],
              x_f_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
        },
        fuzzy = {
          list_models[[model]] <- scoringTools::fuzzy_augmentation(
            x_f_train,
            x_nf_train,
            y_f_train
          )
          roc_curves[[model]] <- pROC::roc(
            c(y_f_test, y_nf_test),
            predict(list_models[[model]],
              rbind(x_f_test, x_nf_test),
              type = "response"
            )
          )
          roc_curves_financed[[model]] <- pROC::roc(
            y_f_test,
            predict(list_models[[model]],
              x_f_test,
              type = "response"
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(c(y_f_test, y_nf_test),
            predict(list_models[[model]],
              rbind(x_f_test, x_nf_test),
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_test_financed[[model]] <- pROC::ci.auc(y_f_test,
            predict(list_models[[model]],
              x_f_test,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )

          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(list_models[[model]],
              data_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train_financed[[model]] <- pROC::ci.auc(data_f_train[[input$var_cible]],
            predict(list_models[[model]],
              x_f_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
        },
        parcelling = {
          if (!is.null(input$parcellingParam_probs)) {
            probs <- as.numeric(input$parcellingParam_probs)
          } else {
            probs <- seq(0, 1, 0.25)
          }
          if (!is.null(input$parcellingParam_alpha)) {
            alpha <- as.numeric(input$parcellingParam_alpha)
          } else {
            alpha <- rep(1, length(probs) - 1)
          }
          list_models[[model]] <- scoringTools::parcelling(x_f_train,
            x_nf_train,
            y_f_train,
            probs = probs,
            alpha = alpha
          )
          roc_curves[[model]] <- pROC::roc(
            c(y_f_test, y_nf_test),
            predict(list_models[[model]],
              rbind(x_f_test, x_nf_test),
              type = "response"
            )
          )
          roc_curves_financed[[model]] <- pROC::roc(
            y_f_test,
            predict(list_models[[model]],
              x_f_test,
              type = "response"
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(c(y_f_test, y_nf_test),
            predict(list_models[[model]],
              rbind(x_f_test, x_nf_test),
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_test_financed[[model]] <- pROC::ci.auc(y_f_test,
            predict(list_models[[model]],
              x_f_test,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(list_models[[model]],
              data_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train_financed[[model]] <- pROC::ci.auc(data_f_train[[input$var_cible]],
            predict(list_models[[model]],
              x_f_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
        },
        reclassification = {
          list_models[[model]] <- scoringTools::reclassification(x_f_train,
            x_nf_train,
            y_f_train,
            thresh = input$reclassificationParam_thresh
          )
          roc_curves[[model]] <- pROC::roc(
            c(y_f_test, y_nf_test),
            predict(list_models[[model]],
              rbind(x_f_test, x_nf_test),
              type = "response"
            )
          )
          roc_curves_financed[[model]] <- pROC::roc(
            y_f_test,
            predict(list_models[[model]],
              x_f_test,
              type = "response"
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(c(y_f_test, y_nf_test),
            predict(list_models[[model]],
              rbind(x_f_test, x_nf_test),
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_test_financed[[model]] <- pROC::ci.auc(y_f_test,
            predict(list_models[[model]],
              x_f_test,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(list_models[[model]],
              data_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train_financed[[model]] <- pROC::ci.auc(data_f_train[[input$var_cible]],
            predict(list_models[[model]],
              x_f_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
        },
        twins = {
          list_models[[model]] <- scoringTools::twins(
            x_f_train,
            x_nf_train,
            y_f_train
          )
          roc_curves[[model]] <- pROC::roc(
            c(y_f_test, y_nf_test),
            predict(list_models[[model]],
              rbind(x_f_test, x_nf_test),
              type = "response"
            )
          )
          roc_curves_financed[[model]] <- pROC::roc(
            y_f_test,
            predict(list_models[[model]],
              x_f_test,
              type = "response"
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(c(y_f_test, y_nf_test),
            predict(list_models[[model]],
              rbind(x_f_test, x_nf_test),
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_test_financed[[model]] <- pROC::ci.auc(y_f_test,
            predict(list_models[[model]],
              x_f_test,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(list_models[[model]],
              data_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train_financed[[model]] <- pROC::ci.auc(data_f_train[[input$var_cible]],
            predict(list_models[[model]],
              x_f_train,
              type = "response"
            ),
            conf.level = input$confidence_level_reject
          )
        },
        print("no model specified yet")
      )
    }
    return(list(
      roc_curves,
      roc_curves_financed,
      list_gini_test,
      list_gini_test_financed,
      list_gini_train,
      list_gini_train_financed
    ))
  })

  output$roc_tous_reject_inference <- plotly::renderPlotly({
    roc_curves <- model_reject_inference()[[1]]

    df_roc_curve_all <- data.frame(
      unlist(unname(
        lapply(roc_curves, function(roc_curve) {
          roc_curve$specificities
        })
      )),
      unlist(unname(
        lapply(roc_curves, function(roc_curve) {
          roc_curve$sensitivities
        })
      )),
      unlist(unname(lapply(1:length(roc_curves), function(index) {
        rep(names(roc_curves[index]), length(roc_curves[[index]]$specificities))
      })))
    )

    colnames(df_roc_curve_all) <-
      c("Specificity", "Sensitivity", "Model")

    plotly_plot <- plotly::plot_ly(
      df_roc_curve_all,
      x = ~ (1 - Specificity),
      y = ~Sensitivity,
      linetype = ~ as.factor(Model)
    ) %>%
      plotly::add_segments(
        x = 0,
        y = 0,
        xend = 1,
        yend = 1,
        line = list(
          dash = "7px",
          color = "#F35B25",
          width = 4
        ),
        name = "Random",
        showlegend = FALSE
      ) %>%
      plotly::add_lines(
        name = ~ as.factor(Model),
        line = list(
          shape = "spline",
          color = "#737373",
          width = 4
        )
      ) %>%
      plotly::layout(
        title = "ROC Curve on test set all applicants",
        xaxis = list(
          range = c(0, 1),
          zeroline = F,
          showgrid = F,
          title = "1 - Specificity"
        ),
        yaxis = list(
          range = c(0, 1),
          zeroline = F,
          showgrid = F,
          domain = c(0, 0.9),
          title = "Sensibility"
        )
      )
    plotly_plot
  })

  output$roc_tous_reject_inference_financed <-
    plotly::renderPlotly({
      roc_curves <- model_reject_inference()[[2]]

      df_roc_curve_all <- data.frame(
        unlist(unname(
          lapply(roc_curves, function(roc_curve) {
            roc_curve$specificities
          })
        )),
        unlist(unname(
          lapply(roc_curves, function(roc_curve) {
            roc_curve$sensitivities
          })
        )),
        unlist(unname(lapply(1:length(roc_curves), function(index) {
          rep(names(roc_curves[index]), length(roc_curves[[index]]$specificities))
        })))
      )

      colnames(df_roc_curve_all) <-
        c("Specificity", "Sensitivity", "Model")

      plotly_plot <- plotly::plot_ly(
        df_roc_curve_all,
        x = ~ (1 - Specificity),
        y = ~Sensitivity,
        linetype = ~ as.factor(Model)
      ) %>%
        plotly::add_segments(
          x = 0,
          y = 0,
          xend = 1,
          yend = 1,
          line = list(
            dash = "7px",
            color = "#F35B25",
            width = 4
          ),
          name = "Random",
          showlegend = FALSE
        ) %>%
        plotly::add_lines(
          name = ~ as.factor(Model),
          line = list(
            shape = "spline",
            color = "#737373",
            width = 4
          )
        ) %>%
        plotly::layout(
          title = "ROC Curve on test set financed applicants",
          xaxis = list(
            range = c(0, 1),
            zeroline = F,
            showgrid = F,
            title = "1 - Specificity"
          ),
          yaxis = list(
            range = c(0, 1),
            zeroline = F,
            showgrid = F,
            domain = c(0, 0.9),
            title = "Sensibility"
          )
        )
      plotly_plot
    })

  output$gini_reject <- DT::renderDT({
    list_gini_test <- model_reject_inference()[[3]]
    list_gini_test_financed <- model_reject_inference()[[4]]
    list_gini_train <- model_reject_inference()[[5]]
    list_gini_train_financed <- model_reject_inference()[[6]]

    if (input$report_train_reject) {
      if (input$CI_gini_reject) {
        df <- data.frame(
          "95 % low" = unlist(lapply(list_gini_train, function(auc) {
            (2 * auc[1] - 1) * 100
          })),
          "mean" = unlist(lapply(list_gini_train, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          "95 % high" = unlist(lapply(list_gini_train, function(auc) {
            (2 * auc[3] - 1) * 100
          })),
          "95 % low" = unlist(lapply(list_gini_train_financed, function(auc) {
            (2 * auc[1] - 1) * 100
          })),
          "mean" = unlist(lapply(list_gini_train_financed, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          "95 % high" = unlist(lapply(list_gini_train_financed, function(auc) {
            (2 * auc[3] - 1) * 100
          })),
          "95 % low" = unlist(lapply(list_gini_test, function(auc) {
            (2 * auc[1] - 1) * 100
          })),
          "mean" = unlist(lapply(list_gini_test, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          "95 % high" = unlist(lapply(list_gini_test, function(auc) {
            (2 * auc[3] - 1) * 100
          })),
          "95 % low" = unlist(lapply(list_gini_test_financed, function(auc) {
            (2 * auc[1] - 1) * 100
          })),
          "mean" = unlist(lapply(list_gini_test_financed, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          "95 % high" = unlist(lapply(list_gini_test_financed, function(auc) {
            (2 * auc[3] - 1) * 100
          })),
          check.names = FALSE
        )
        low_value <- paste0(input$confidence_level_reject * 100, " % low")
        high_value <- paste0(input$confidence_level_reject * 100, " % high")

        sketch <- htmltools::withTags(table(
          class = "display",
          thead(
            tr(
              th(rowspan = 3, style = "text-align:center", "Model"),
              th(colspan = 6, style = "text-align:center", "Train set"),
              th(colspan = 6, style = "text-align:center", "Test set"),
            ),
            tr(
              th(colspan = 3, style = "text-align:center", "Through-the-Door"),
              th(colspan = 3, style = "text-align:center", "Financed"),
              th(colspan = 3, style = "text-align:center", "Through-the-Door"),
              th(colspan = 3, style = "text-align:center", "Financed")
            ),
            tr(
              lapply(rep(c(
                low_value,
                "mean",
                high_value
              ), 4), th)
            )
          )
        ))
      } else {
        df <- data.frame(
          "mean train" = unlist(lapply(list_gini_train, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          "mean train financed" = unlist(lapply(list_gini_train_financed, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          "mean test" = unlist(lapply(list_gini_test, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          "mean test financed" = unlist(lapply(list_gini_test_financed, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          check.names = FALSE
        )
        sketch <- htmltools::withTags(table(
          class = "display",
          thead(tr(
            th(style = "text-align:center", "Model"),
            th(style = "text-align:center", "Train set - Through-the-Door"),
            th(style = "text-align:center", "Train set - Financed"),
            th(style = "text-align:center", "Test set - Through-the-Door"),
            th(style = "text-align:center", "Test set - Financed")
          ))
        ))
      }
    } else {
      if (input$CI_gini_reject) {
        df <- data.frame(
          "95 % low" = unlist(lapply(list_gini_test, function(auc) {
            (2 * auc[1] - 1) * 100
          })),
          "mean" = unlist(lapply(list_gini_test, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          "95 % high" = unlist(lapply(list_gini_test, function(auc) {
            (2 * auc[3] - 1) * 100
          })),
          "95 % low" = unlist(lapply(list_gini_test_financed, function(auc) {
            (2 * auc[1] - 1) * 100
          })),
          "mean" = unlist(lapply(list_gini_test_financed, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          "95 % high" = unlist(lapply(list_gini_test_financed, function(auc) {
            (2 * auc[3] - 1) * 100
          })),
          check.names = FALSE
        )

        low_value <- paste0(input$confidence_level_reject * 100, " % low")
        high_value <- paste0(input$confidence_level_reject * 100, " % high")

        sketch <- htmltools::withTags(table(
          class = "display",
          thead(
            tr(
              th(rowspan = 2, style = "text-align:center", "Model"),
              th(colspan = 3, style = "text-align:center", "Test set - Through-the-Door"),
              th(colspan = 3, style = "text-align:center", "Test set - Financed")
            ),
            tr(
              lapply(rep(c(
                low_value,
                "mean",
                high_value
              ), 2), th)
            )
          )
        ))
      } else {
        df <- data.frame(
          "mean test" = unlist(lapply(list_gini_test, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          "mean test financed" = unlist(lapply(list_gini_test_financed, function(auc) {
            (2 * auc[2] - 1) * 100
          })),
          check.names = FALSE
        )
        sketch <- htmltools::withTags(table(
          class = "display",
          thead(tr(
            th(style = "text-align:center", "Model"),
            th(style = "text-align:center", "Test set - Through-the-Door"),
            th(style = "text-align:center", "Test set - Financed")
          ))
        ))
      }
    }
    DT::datatable(df,
      rownames = TRUE,
      container = sketch
    ) %>%
      DT::formatRound(columns = c(1:ncol(df)), digits = 2)
  })


  # Quantization
  data_quantization <- reactive({
    data <- list_imported_df[[input$selectedDataQuantization]]
    int_test <- 1:nrow(data) %in% sample.int(nrow(data),
      size = (input$bins_test) / 100 * nrow(data)
    )
    int_train <- !int_test
    x_train <-
      data[int_train, !colnames(data) == input$var_cible, drop = FALSE]
    y_train <- data[int_train, input$var_cible]
    data_train <- data[int_train, ]
    x_test <-
      data[int_test, !colnames(data) == input$var_cible, drop = FALSE]
    y_test <- data[int_test, input$var_cible]
    data_test <- data[int_test, ]

    if (input$deleteSamplesQuantization) {
      levels_in_train <-
        lapply(data_train[, sapply(data_train, is.factor)], function(fct) {
          levels(factor(fct))
        })
      levels_in_test <-
        lapply(data_test[, sapply(data_test, is.factor)], function(fct) {
          levels(factor(fct))
        })

      if (length(levels_in_train) > 0) {
        levels_to_delete <-
          sapply(1:length(levels_in_train), function(idx) {
            if (length(which(!(
              levels_in_test[[idx]] %in% levels_in_train[[idx]]
            ))) > 0) {
              return(levels_in_test[[idx]][which(!(levels_in_test[[idx]] %in% levels_in_train[[idx]]))])
            } else {
              return(NULL)
            }
          })

        rows_to_delete <- sapply(
          1:sum(sapply(data_test, is.factor)),
          function(idx) {
            data_test[, sapply(data_test, is.factor)][, idx] %in% levels_to_delete[[idx]]
          }
        )

        data_test <-
          data_test[Matrix::rowSums(rows_to_delete) == 0, ]
        warning("Deleted samples due to factor levels in test set not in train financed set.")
      }
    }

    return(
      list(
        data_train = data_train,
        data_test = data_test
      )
    )
  })

  model_quantization <- reactive({
    list_to_parse <- data_quantization()
    data_train <- list_to_parse[[1]]
    data_test <- list_to_parse[[2]]

    list_models <- list()
    roc_curves <- list()
    list_gini_test <- list()
    list_gini_train <- list()
    for (model in (input$modelsQuantization)) {
      switch(
        model,
        linear = {
          list_models[[model]] <- stats::glm(
            as.formula(paste(input$var_cible_quantization, "~ .")),
            data = data_train,
            family = stats::binomial(link = "logit")
          )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible_quantization]],
            predict(list_models[[model]],
              data_test,
              type = "response"
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible_quantization]],
            predict(list_models[[model]],
              data_test,
              type = "response"
            ),
            conf.level = input$confidence_level_quantization
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible_quantization]],
            predict(list_models[[model]],
              data_train,
              type = "response"
            ),
            conf.level = input$confidence_level_quantization
          )
        },
        glmdisc = {
          library(glmdisc)
          list_models[[model]] <-
            glmdisc(
              data_train[, !colnames(data_train) == input$var_cible_quantization],
              data_train[[input$var_cible_quantization]],
              interact = input$glmdiscParam_interact,
              validation = input$glmdiscParam_validation,
              test = input$glmdiscParam_test,
              criterion = input$glmdiscParam_criterion,
              iter = input$glmdiscParam_iter,
              m_start = input$glmdiscParam_m_start
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible_quantization]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible_quantization]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_quantization
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible_quantization]],
            predict(
              list_models[[model]],
              data_train[, !colnames(data_train) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_quantization
          )
        },
        chi2 = {
          list_models[[model]] <-
            scoringTools::chi2_iter(
              data_train[, !colnames(data_train) == input$var_cible_quantization],
              data_train[[input$var_cible_quantization]],
              validation = input$chi2Param_validation,
              test = input$chi2Param_test,
              criterion = input$chi2Param_criterion
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(
              list_models[[model]],
              data_train[, !colnames(data_train) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
        },
        chiM = {
          list_models[[model]] <-
            scoringTools::chiM_iter(
              data_train[, !colnames(data_train) == input$var_cible_quantization],
              data_train[[input$var_cible_quantization]],
              validation = input$chiMParam_validation,
              test = input$chiMParam_test,
              criterion = input$chiMParam_criterion
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(
              list_models[[model]],
              data_train[, !colnames(data_train) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
        },
        echi2 = {
          list_models[[model]] <-
            scoringTools::echi2_iter(
              data_train[, !colnames(data_train) == input$var_cible_quantization],
              data_train[[input$var_cible_quantization]],
              validation = input$echi2Param_validation,
              test = input$echi2Param_test,
              criterion = input$echi2Param_criterion
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(
              list_models[[model]],
              data_train[, !colnames(data_train) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
        },
        modchi2 = {
          list_models[[model]] <-
            scoringTools::modchi2_iter(
              data_train[, !colnames(data_train) == input$var_cible_quantization],
              data_train[[input$var_cible_quantization]],
              validation = input$modchi2Param_validation,
              test = input$modchi2Param_test,
              criterion = input$modchi2Param_criterion
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(
              list_models[[model]],
              data_train[, !colnames(data_train) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
        },
        mdlp = {
          list_models[[model]] <-
            scoringTools::mdlp_iter(
              data_train[, !colnames(data_train) == input$var_cible_quantization],
              data_train[[input$var_cible_quantization]],
              validation = input$mdlpParam_validation,
              test = input$mdlpParam_test,
              criterion = input$mdlpParam_criterion
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(
              list_models[[model]],
              data_train[, !colnames(data_train) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
        },
        topdown = {
          list_models[[model]] <-
            scoringTools::topdown(
              data_train[, !colnames(data_train) == input$var_cible_quantization],
              data_train[[input$var_cible_quantization]],
              validation = input$topdownParam_validation,
              test = input$topdownParam_test,
              criterion = input$topdownParam_criterion
            )
          roc_curves[[model]] <- pROC::roc(
            data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            )
          )
          list_gini_test[[model]] <- pROC::ci.auc(data_test[[input$var_cible]],
            predict(
              list_models[[model]],
              data_test[, !colnames(data_test) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
          list_gini_train[[model]] <- pROC::ci.auc(data_train[[input$var_cible]],
            predict(
              list_models[[model]],
              data_train[, !colnames(data_train) == input$var_cible_quantization]
            ),
            conf.level = input$confidence_level_reject
          )
        },
        print("no model specified yet")
      )
    }
    return(list(
      roc_curves,
      list_gini_test,
      list_gini_train
    ))
  })

  output$roc_tous_quantization <- plotly::renderPlotly({
    roc_curves <- model_quantization()[[1]]

    df_roc_curve_all <- data.frame(
      unlist(unname(
        lapply(roc_curves, function(roc_curve) {
          roc_curve$specificities
        })
      )),
      unlist(unname(
        lapply(roc_curves, function(roc_curve) {
          roc_curve$sensitivities
        })
      )),
      unlist(unname(lapply(1:length(roc_curves), function(index) {
        rep(names(roc_curves[index]), length(roc_curves[[index]]$specificities))
      })))
    )

    colnames(df_roc_curve_all) <-
      c("Specificity", "Sensitivity", "Model")

    plotly_plot <- plotly::plot_ly(
      df_roc_curve_all,
      x = ~ (1 - Specificity),
      y = ~Sensitivity,
      linetype = ~ as.factor(Model)
    ) %>%
      plotly::add_segments(
        x = 0,
        y = 0,
        xend = 1,
        yend = 1,
        line = list(
          dash = "7px",
          color = "#F35B25",
          width = 4
        ),
        name = "Random",
        showlegend = FALSE
      ) %>%
      plotly::add_lines(
        name = ~ as.factor(Model),
        line = list(
          shape = "spline",
          color = "#737373",
          width = 4
        )
      ) %>%
      plotly::layout(
        title = "ROC Curve on test set all applicants",
        xaxis = list(
          range = c(0, 1),
          zeroline = F,
          showgrid = F,
          title = "1 - Specificity"
        ),
        yaxis = list(
          range = c(0, 1),
          zeroline = F,
          showgrid = F,
          domain = c(0, 0.9),
          title = "Sensibility"
        )
      )
    plotly_plot
  })

  # Logistic regression trees
  output$roc_tous_logistic_regression_trees <- shiny::renderPlot({
    for (model in (input$modelsLogisticRegressionTrees)) {
      switch(model,
        "glmtree",
        {
          if (!requireNamespace("glmtree", quietly = TRUE)) {
            print(warning(
              "Package glmtree not installed, please install it to proceed."
            ))
          }
        },
        "tree",
        {
          if (!requireNamespace("rpart", quietly = TRUE)) {
            print(warning(
              "Package rpart not installed, please install it to proceed."
            ))
          }
        },
        "mob",
        {
          if (!requireNamespace("partykit", quietly = TRUE)) {
            print(warning(
              "Package partykit not installed, please install it to proceed."
            ))
          }
        },
        "lmt",
        {
          if (!requireNamespace("RWeka", quietly = TRUE)) {
            print(warning(
              "Package RWeka not installed, please install it to proceed."
            ))
          }
        }
      )
    }
  })
}
