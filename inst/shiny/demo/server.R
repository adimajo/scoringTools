server <- function(input, output, session) {
  # Fonction d'import de données
  shiny::observe({
    req(input$imported_files)
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

  output$data_import_options <- shiny::renderUI({
    shiny::fluidPage(
      shiny::conditionalPanel(
        condition = "input.use_lendingClub",
        # Horizontal line ----
        tags$hr(),
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
            tags$hr(),
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

            shiny::actionButton(
              paste0("good_to_go_", i),
              "Test these settings!"
            )
          )
        })
      }
    )
  })

  list_imported_df <- reactiveValues()

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

  # output$roc_tous <- renderPlot({
  #
  #      int_f = sample.int(nrow(input$selectedData), size = input$bins/100 * nrow(input$selectedData))
  #      int_nf = !int_f %in% 1:nrow(input$selectedData)
  #      x_f = input$selectedData[int_f, colnames(data_f) == input$var_cible]
  #      x_nf = input$selectedData[int_nf,colnames(data_f) == input$var_cible]
  #      y_f = input$selectedData[int_f, input$var_cible]
  #      y_nf = input$selectedData[int_nf, input$var_cible]
  #
  #      for (model in (input$models)) {
  #           switch(model,
  #                "log", {
  #                     reglog_model = stats::glm( , data = input$selectedData, family = stats::binomial(link="logit"))
  #                },
  #                "tree", {
  #                     tree_model = rpart::rpart( , data = input$selectedData, type = "class")
  #                },
  #                "rforest", {},
  #                "svm", {},
  #                "nnet", {},
  #                NULL, {}
  #           )
  #      }
  # })

  output$distPlot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <-
      seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x,
      breaks = bins,
      col = "darkgray",
      border = "white"
    )
  })

  output$distPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <-
      seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x,
      breaks = bins,
      col = "darkgray",
      border = "white"
    )
  })
}
