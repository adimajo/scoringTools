server <- function(input, output, session) {
  # Fonction d'import de données
  shiny::observe({
    shiny::req(input$imported_files)
    shiny::updateSelectInput(
      session = session,
      "selectedDataRejectInference",
      choices = c("lendingClub", paste(lapply(1:length(input$imported_files[, 1]), function(i) {
        input$imported_files[[i, "name"]]
      })))
    )
    shiny::updateSelectInput(
      session = session,
      "selectedDataQuantization",
      choices = c("lendingClub", paste(lapply(1:length(input$imported_files[, 1]), function(i) {
        input$imported_files[[i, "name"]]
      })))
    )
    shiny::updateSelectInput(
      session = session,
      "selectedDataLogisticRegressionTrees",
      choices = c("lendingClub", paste(lapply(1:length(input$imported_files[, 1]), function(i) {
        input$imported_files[[i, "name"]]
      })))
    )
  })

  shiny::observeEvent(
    input$use_lendingClub | (!is.null(colnames(list_imported_df[[input$selectedDataRejectInference]]))),
    shiny::updateSelectInput(
      session = session,
      "var_cible",
      choices = {
        if (input$selectedDataRejectInference == "lendingClub") {
          colnames(lendingClub)
        } else {
          colnames(list_imported_df[[input$selectedDataRejectInference]])
        }
      }
    )
  )
  shiny::observeEvent(
    input$use_lendingClub | (!is.null(colnames(list_imported_df[[input$selectedDataRejectInference]]))),
    shiny::updateSelectInput(
      session = session,
      "var_reject",
      choices = {
        if (input$selectedDataRejectInference == "lendingClub") {
          colnames(lendingClub)
        } else {
          colnames(list_imported_df[[input$selectedDataRejectInference]])
        }
      }
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
          choices = c(Head = "head",
                      All = "all"),
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
            shiny::checkboxInput(paste0("header_", i),
                                 "Header",
                                 TRUE),

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
              choices = c(Head = "head",
                          All = "all"),
              selected = "head"
            ),

            shiny::selectInput(
              paste0("columns_", i),
              "Columns to keep",
              choices = c(All = "all"),
              selected = "all",
              multiple = TRUE
            ),

            shiny::actionButton(paste0("good_to_go_", i),
                                "Test these settings!")
          )
        })
      }
    )
  })

  list_imported_df <- shiny::reactiveValues()

  shiny::observe({
    lapply(1:length(input$imported_files[, 1]), function(i) {
      shiny::observeEvent({
        input[[paste0("good_to_go_", i)]]
      },
      {
        list_imported_df[[input$imported_files[[i, "name"]]]] <- read.csv(
          input$imported_files[[i, "datapath"]],
          header = input[[paste0("header_", i)]],
          sep = input[[paste0("sep_", i)]],
          quote = input[[paste0("quote_", i)]]
        )
        if (!"all" %in% input[[paste0("columns_"
                                      , i)]]) {
          print("there")
          print(input[[paste0("columns_", i)]])
          list_imported_df[[input$imported_files[[i, "name"]]]] <-
            list_imported_df[[input$imported_files[[i, "name"]]]][, input[[paste0("columns_", i)]]]
        }
      },
      event.quoted = FALSE,
      ignoreInit = FALSE)
    })
  })
  # output$contents <- shiny::renderTable({head(lendingClub)})

  output$contents <- shiny::renderUI({
    shiny::fluidPage(shiny::renderTable({
      if (input$use_lendingClub) {
        if (input[[paste0("disp_", "lendingClub")]] == "head") {
          return(head(lendingClub))
        } else {
          return(lendingClub)
        }
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
    })
  })

  # Réintégration des refusés
  ## Courbe ROC avec tout le monde
  output$roc_tous_reject_inference <- plotly::renderPlotly({
    if (input$selectedDataRejectInference == "lendingClub") {
      data = lendingClub
    } else {
      data = list_imported_df[[input$selectedDataRejectInference]]
    }
    int_f = sample.int(nrow(data),
                       size = input$bins / 100 * nrow(data))
    int_nf = !int_f %in% 1:nrow(data)
    x_f = data[int_f, colnames(data) == input$var_cible]
    x_nf = data[int_nf, colnames(data) == input$var_cible]
    y_f = data[int_f, input$var_cible]
    y_nf = data[int_nf, input$var_cible]

    list_models = list()
    roc_curves = list()
    for (model in (input$modelsRejectInference)) {
      switch(model,
             log =
             {
               list_models[[model]] = stats::glm(as.formula(paste(input$var_cible, "~ .")),
                 data = data,
                 family = stats::binomial(link = "logit")
               )
               roc_curves[[model]] = pROC::roc(list_models[[model]]$y, list_models[[model]]$fitted.values)
             },
             tree =
             {
               if (!requireNamespace("rpart", quietly = TRUE)) {
                 print(warning(
                   "Package rpart not installed, please install it to proceed."
                 ))
               }
               list_models[[model]] = rpart::rpart(as.formula(paste(input$var_cible, "~ .")),
                                         data = data,
                                         method = "class")
               roc_curves[[model]] = pROC::roc(list_models[[model]]$y, predict(list_models[[model]], data)[, 1])

             },
             rforest =
             {
               if (!requireNamespace("randomForest", quietly = TRUE)) {
                 print(warning(
                   "Package randomForest not installed, please install it to proceed."
                 ))
               }
               list_models[[model]] = NULL

             },
             svm =
             {
               if (!requireNamespace("e1071", quietly = TRUE)) {
                 print(warning(
                   "Package e1071 not installed, please install it to proceed."
                 ))
               }
               list_models[[model]] = NULL

             },
             nnet =
             {
               if (!requireNamespace("nnet", quietly = TRUE)) {
                 print(warning(
                   "Package nnet not installed, please install it to proceed."
                 ))
               }
               list_models[[model]] = NULL

             },
             print("no model specified yet")
             )

    }

    df_roc_curve = data.frame(unlist(unname(lapply(roc_curves, function(roc_curve) roc_curve$specificities))),
                              unlist(unname(lapply(roc_curves, function(roc_curve) roc_curve$sensitivities))),
                              unlist(unname(lapply(1:length(roc_curves), function(index) rep(names(roc_curves[index]), length(roc_curves[[index]]$specificities))))))

    colnames(df_roc_curve) = c("Specificity", "Sensitivity", "Model")

    plotly_plot <- plotly::plot_ly(df_roc_curve,
                                   x = ~ (1 - Specificity),
                                   y = ~Sensitivity,
                                   linetype = ~as.factor(Model),
                                   hoverinfo = "none"
    ) %>%
      plotly::add_lines(
        name = ~as.factor(Model),
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
        )
      )
    plotly_plot
  })

  output$roc_tous_logistic_regression_trees <- shiny::renderPlot({
    for (model in (input$modelsLogisticRegressionTrees)) {
      switch(model,
             "glmtree", {
               if (!requireNamespace("glmtree", quietly = TRUE)) {
                 print(warning(
                   "Package glmtree not installed, please install it to proceed."
                 ))
               }
             },
             "tree", {
               if (!requireNamespace("rpart", quietly = TRUE)) {
                 print(warning(
                   "Package rpart not installed, please install it to proceed."
                 ))
               }

             },
             "mob", {
               if (!requireNamespace("partykit", quietly = TRUE)) {
                 print(warning(
                   "Package partykit not installed, please install it to proceed."
                 ))
               }

             },
             "lmt", {
               if (!requireNamespace("RWeka", quietly = TRUE)) {
                 print(warning(
                   "Package RWeka not installed, please install it to proceed."
                 ))
               }

             })
    }
  })
}
