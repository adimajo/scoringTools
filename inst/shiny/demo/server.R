server <- function(input, output, session) {
  # Fonction d'import de données
  shiny::observe({
          shiny::req(input$imported_files)
    shiny::updateSelectInput(
      session = session,
      "selectedDataRejectInference",
      choices = c("lendingClub", paste(
        lapply(1:length(input$imported_files[, 1]), function(i) {
          input$imported_files[[i, "name"]]
        })
      ))
    )
    shiny::updateSelectInput(
      session = session,
      "selectedDataQuantization",
      choices = c("lendingClub", paste(
        lapply(1:length(input$imported_files[, 1]), function(i) {
          input$imported_files[[i, "name"]]
        })
      ))
    )
    shiny::updateSelectInput(
            session = session,
            "selectedDataLogisticRegressionTrees",
            choices = c("lendingClub", paste(
                    lapply(1:length(input$imported_files[, 1]), function(i) {
                            input$imported_files[[i, "name"]]
                    })
            ))
    )
  })

        shiny::observeEvent(
                colnames(list_imported_df[[input$selectedDataRejectInference]]),
                shiny::updateSelectInput(
                        session = session,
                        "var_cible",
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
        )})}})

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
                    choices = c(
                            All = "all"
                    ),
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

  list_imported_df <- shiny::reactiveValues()

  shiny::observe({
    lapply(1:length(input$imported_files[, 1]), function(i) {
      shiny::observeEvent(
        {
          input[[paste0("good_to_go_", i)]]
        },
        {
                print("here")
          list_imported_df[[input$imported_files[[i, "name"]]]] <- read.csv(
            input$imported_files[[i, "datapath"]],
            header = input[[paste0("header_", i)]],
            sep = input[[paste0("sep_", i)]],
            quote = input[[paste0("quote_", i)]]
          )
          if (!"all" %in% input[[paste0("columns_", i)]]) {
                  print("there")
                  print(input[[paste0("columns_", i)]])
                  list_imported_df[[input$imported_files[[i, "name"]]]] <- list_imported_df[[input$imported_files[[i, "name"]]]][, input[[paste0("columns_", i)]]]
          }
        },
        event.quoted = FALSE,
        ignoreInit = FALSE
      )
    })
  })
  # output$contents <- shiny::renderTable({head(lendingClub)})

  output$contents <- shiny::renderUI({
    shiny::fluidPage(
      shiny::renderTable({
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
            if (!is.null(data) & input[[paste0("good_to_go_", i)]] > 0) {
              if (input[[paste0("disp_", i)]] == "head") {
                return(head(
                  data
                ))
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
  output$roc_tous_reject_inference <- shiny::renderPlot({
       data = list_imported_df[[input$selectedDataRejectInference]]
       int_f = sample.int(nrow(data),
                          size = input$bins/100 * nrow(data))
       int_nf = !int_f %in% 1:nrow(data)
       x_f = data[int_f, colnames(data_f) == input$var_cible]
       x_nf = data[int_nf,colnames(data_f) == input$var_cible]
       y_f = data[int_f, input$var_cible]
       y_nf = data[int_nf, input$var_cible]

       for (model in (input$models)) {
            switch(model,
                 "log", {
                      reglog_model = stats::glm( , data = input$selectedDataRejectInference, family = stats::binomial(link="logit"))
                 },
                 "tree", {
                         if (!requireNamespace("rpart", quietly = TRUE)) {
                                 print(warning("Package rpart not installed, please install it to proceed."))
                         }
                         tree_model = rpart::rpart( , data = input$selectedDataRejectInference, type = "class")
                 },
                 "rforest", {
                         if (!requireNamespace("randomForest", quietly = TRUE)) {
                                 print(warning("Package randomForest not installed, please install it to proceed."))
                         }

                 },
                 "svm", {},
                 "nnet", {},
                 NULL, {}
            )
       }
  })

  output$roc_tous_logistic_regression_trees <- shiny::renderPlot({
          for (model in (input$modelsLogisticRegressionTrees)) {
        switch(model,
               "glmtree", {
                        if (!requireNamespace("glmtree", quietly = TRUE)) {
                                print(warning("Package glmtree not installed, please install it to proceed."))
                        }
               },
               "tree", {
                       if (!requireNamespace("rpart", quietly = TRUE)) {
                               print(warning("Package rpart not installed, please install it to proceed."))
                       }

               },
               "mob", {
                       if (!requireNamespace("partykit", quietly = TRUE)) {
                               print(warning("Package partykit not installed, please install it to proceed."))
                       }

               },
               "lmt", {
                       if (!requireNamespace("RWeka", quietly = TRUE)) {
                               print(warning("Package RWeka not installed, please install it to proceed."))
                       }

               }
        )
          }
  })
}
