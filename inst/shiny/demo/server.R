server <- function(input, output) {

     # Fonction d'import de données

     df <- reactive({
          inFile <- input$file1
          if (is.null(inFile)) return(NULL)
          data <- read.csv(input$file1$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote)
          return(data)
     })

     output$essai <- renderTable({input$reject})

     output$contents <- renderTable({

          # input$file1 will be NULL initially. After the user selects
          # and uploads a file, head of that data file by default,
          # or all rows if selected, will be shown.

          req(input$file1)

          if(input$disp == "head") {
               return(head(df()))
          }
          else {
               return(df())
          }

     })

     # Réintégration des refusés

     ## Courbe ROC avec tout le monde

     output$roc_tous <- renderPlot({

          int_f = sample.int(nrow(input$selectedData), size = input$bins/100 * nrow(input$selectedData))
          int_nf = !int_f %in% 1:nrow(input$selectedData)
          x_f = input$selectedData[int_f, colnames(data_f) == input$var_cible]
          x_nf = input$selectedData[int_nf,colnames(data_f) == input$var_cible]
          y_f = input$selectedData[int_f, input$var_cible]
          y_nf = input$selectedData[int_nf, input$var_cible]

          for (model in (input$models)) {
               switch(model,
                    "log", {
                         reglog_model = stats::glm( , data = input$selectedData, family = stats::binomial(link="logit"))
                    },
                    "tree", {
                         tree_model = rpart::rpart( , data = input$selectedData, type = "class")
                    },
                    "rforest", {},
                    "svm", {},
                    "nnet", {},
                    NULL, {}
               )
          }
     })

     output$distPlot1 <- renderPlot({
          # generate bins based on input$bins from ui.R
          x    <- faithful[, 2]
          bins <- seq(min(x), max(x), length.out = input$bins + 1)

          # draw the histogram with the specified number of bins
          hist(x, breaks = bins, col = 'darkgray', border = 'white')
     })

     output$distPlot2 <- renderPlot({
          # generate bins based on input$bins from ui.R
          x    <- faithful[, 2]
          bins <- seq(min(x), max(x), length.out = input$bins + 1)

          # draw the histogram with the specified number of bins
          hist(x, breaks = bins, col = 'darkgray', border = 'white')
     })
}
