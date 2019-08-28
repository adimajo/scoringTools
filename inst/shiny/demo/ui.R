ui <- fluidPage(navbarPage("Statistical problems in Credit Scoring",
                           tabPanel("Data import",
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


                           tabPanel("Reject Inference",

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

                                              selectInput("selectedData", "Data selection",
                                                          list("lendingClub", "Imported Data")),


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
                                                   selectInput("rforestParam", "Random Forest-specific parameters",
                                                               list("ntree", "mtry", "replace", "maxnodes"))
                                              ),

                                              conditionalPanel(
                                                   condition = "input.models.includes('svm')",
                                                   selectInput("svmParam", "SVM-specific parameters",
                                                               list("kernel", "degree", "gamma", "coef0"))
                                              ),

                                              conditionalPanel(
                                                   condition = "input.models.includes('nnet')",
                                                   selectInput("nnetParam", "Neural network-specific parameters",
                                                               list("size", "decay", "maxit"))
                                              ),

                                              conditionalPanel(
                                                   condition = "input.reject.includes('parcelling')",
                                                   selectInput("parcellingParam", "Parcelling hyperparameters",
                                                               list("probs", "alpha"))
                                              ),

                                              conditionalPanel(
                                                   condition = "input.parcellingParam.includes('probs')",
                                                   textInput("parcellingParamProbs", "Parcelling hyperparameter probs (a numeric vector is expected)",
                                                             seq(0, 1, 0.25))
                                              ),

                                              conditionalPanel(
                                                   condition = "input.parcellingParam.includes('alpha')",
                                                   textInput("parcellingParamAlpha", "Parcelling hyperparameter alpha (a numeric vector is expected)",
                                                             rep(1,length(probs)-1))
                                              ),

                                              conditionalPanel(
                                                   condition = "input.reject.includes('reclassification')",
                                                   selectInput("reclassificationParam", "Reclassification hyperparameter",
                                                               list("thresh"))
                                              )

                                         ),

                                         # Show a plot of the generated distribution
                                         mainPanel(
                                              tabsetPanel(
                                                   tabPanel("ROC curves",
                                                            fluidRow(
                                                                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("distPlot1"), plotOutput("distPlot2"))
                                                            )),
                                                   tabPanel("Gini indices", tableOutput("gini_reject"))
                                              )
                                         )
                                    )
                           ),



                           tabPanel("Quantization",
                                    # Sidebar with a slider input for number of bins
                                    sidebarLayout(
                                         sidebarPanel(
                                              # Input: Select data ----

                                              selectInput("selectedData", "Data selection",
                                                          list("lendingClub"))
                                         ),

                                         # Show a plot of the generated distribution
                                         mainPanel())),



                           tabPanel("Logistic Regression Trees",
                                    # Sidebar with a slider input for number of bins
                                    sidebarLayout(
                                         sidebarPanel(
                                              # Input: Select data ----

                                              selectInput("selectedData", "Data selection",
                                                          list("lendingClub"))
                                         ),

                                         # Show a plot of the generated distribution
                                         mainPanel()))
))
