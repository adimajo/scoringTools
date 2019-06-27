ui <- fluidPage(navbarPage("Scoring d'octroi",
                           tabPanel("Import de données",
                                    sidebarPanel(
                                         fileInput('file1', 'Choose CSV File',
                                                   multiple = TRUE,
                                                   accept=c('text/csv',
                                                            'text/comma-separated-values,text/plain',
                                                            '.csv')),
                                         # Horizontal line ----
                                         tags$hr(),

                                         # Input: Checkbox if file has header ----
                                         checkboxInput("header", "Header", TRUE),

                                         # Input: Select separator ----
                                         radioButtons("sep", "Separator",
                                                      choices = c(Comma = ",",
                                                                  Semicolon = ";",
                                                                  Tab = "\t"),
                                                      selected = ","),

                                         # Input: Select quotes ----
                                         radioButtons("quote", "Quote",
                                                      choices = c(None = "",
                                                                  "Double Quote" = '"',
                                                                  "Single Quote" = "'"),
                                                      selected = '"'),

                                         # Input: Select number of rows to display ----
                                         radioButtons("disp", "Display",
                                                      choices = c(Head = "head",
                                                                  All = "all"),
                                                      selected = "head")



                                    ),
                                    mainPanel(
                                         tableOutput('contents')
                                    )

                           ),


                           tabPanel("Réintégration des refusés",

                                    # Sidebar with a slider input for number of bins
                                    sidebarLayout(
                                         sidebarPanel(
                                              # Input: Select fraction of rejected applications to simulate ----

                                              sliderInput("bins",
                                                          "Fraction of rejected applications:",
                                                          min = 0,
                                                          max = 100,
                                                          value = 30),

                                              # Input: Select data ----

                                              selectInput("selectedData", "Sélection des données",
                                                          list("weights", "family", "gam", "loess", "rlm")),


                                              # Input: Select models to compute ----
                                              selectInput("models", "Modèles",
                                                          multiple = TRUE,
                                                          choices = c(Logistique = "log",
                                                                      Arbre = "tree",
                                                                      RandomForest = "rforest",
                                                                      SVM = "svm",
                                                                      Neurones = "nnet"),
                                                          selected = "log"),


                                              # Input: Select reject inference models to compute ----

                                              selectInput("reject", "Méthodes de réintégration",
                                                          multiple = TRUE,
                                                          choices = c(Augmentation = "augmentation",
                                                                      FuzzyAugmentation = "fuzzy",
                                                                      Parcelling = "parcelling",
                                                                      Reclassification = "reclassification",
                                                                      Twins = "twins"),
                                                          selected = NULL),

                                              # Input: Conditional panels for parameters of selected models ----

                                              conditionalPanel(
                                                   condition = "input.models.includes('rforest')",
                                                   selectInput("rforestParam", "Paramètres Random Forest",
                                                               list("weights", "family", "gam", "loess", "rlm"))
                                              ),

                                              conditionalPanel(
                                                   condition = "input.models.includes('svm')",
                                                   selectInput("svmParam", "Paramètres SVM",
                                                               list("lm", "glm", "gam", "loess", "rlm"))
                                              ),

                                              conditionalPanel(
                                                   condition = "input.models.includes('nnet')",
                                                   selectInput("nnetParam", "Paramètres réseaux de neurones",
                                                               list("lm", "glm", "gam", "loess", "rlm"))
                                              ),

                                              conditionalPanel(
                                                   condition = "input.reject.includes('parcelling')",
                                                   selectInput("parcellingParam", "Paramètres Parcelling",
                                                               list("lm", "glm", "gam", "loess", "rlm"))
                                              ),

                                              conditionalPanel(
                                                   condition = "input.reject.includes('reclassification')",
                                                   selectInput("reclassificationParam", "Paramètres Reclassification",
                                                               list("lm", "glm", "gam", "loess", "rlm"))
                                              )

                                         ),

                                         # Show a plot of the generated distribution
                                         mainPanel(
                                              tabsetPanel(
                                                   tabPanel("Courbes ROC",
                                                            fluidRow(
                                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot1"), plotOutput("distPlot2"))
                                                            )),
                                                   tabPanel("Statistiques", tableOutput("essai")),
                                                   tabPanel("Table")
                                              )
                                         )
                                    )
                           ),
                           tabPanel("Discrétisation"),
                           tabPanel("Recouvrement"),
                           tabPanel("Segmentation"),
                           tabPanel("Grande dimension")
))
