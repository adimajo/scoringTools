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
          #
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
