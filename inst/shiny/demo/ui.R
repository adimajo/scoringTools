ui <- fluidPage(
  navbarPage(
    "Statistical problems in Credit Scoring",
    tabPanel(
      "Data import",
      sidebarPanel(
        fileInput(
          "imported_files",
          "Choose CSV File(s)",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        checkboxInput(
          "use_lendingClub",
          "Use lendingClub dataset?",
          TRUE
        ),
        shiny::uiOutput("data_import_options")
      ),
      shiny::mainPanel(shiny::uiOutput("contents"))
    ),


    shiny::tabPanel(
      "Reject Inference",

      # Sidebar with a slider input for number of bins
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # Input: Select fraction of rejected applications to simulate ----

          shiny::sliderInput(
            "bins",
            "Fraction of rejected applications:",
            min = 0,
            max = 100,
            value = 30
          ),

          # Input: Select data ----

          shiny::selectInput(
            "selectedDataRejectInference",
            "Data selection",
            list("lendingClub")
          ),


          # Input: Select models to compute ----
          shiny::selectInput(
            "models",
            "Models",
            multiple = TRUE,
            choices = c(
              Logistique = "log",
              Arbre = "tree",
              RandomForest = "rforest",
              SVM = "svm",
              Neurones = "nnet"
            ),
            selected = "log"
          ),


          # Input: Select reject inference models to compute ----

          shiny::selectInput(
            "reject",
            "Reject Inference methods",
            multiple = TRUE,
            choices = c(
              Augmentation = "augmentation",
              FuzzyAugmentation = "fuzzy",
              Parcelling = "parcelling",
              Reclassification = "reclassification",
              Twins = "twins"
            ),
            selected = NULL
          ),

          # Input: Conditional panels for parameters of selected models ----

          # Parameters for rforest
          shiny::conditionalPanel(
            condition = "input.models.includes('rforest')",
            shiny::numericInput(
              "rforestParam_ntree",
              "Random Forest: ntree",
              100
            )
          ),
          shiny::conditionalPanel(
            condition = "input.models.includes('rforest')",
            shiny::numericInput(
              "rforestParam_mtry",
              "Random Forest: mtry",
              5
            )
          ),
          shiny::conditionalPanel(
            condition = "input.models.includes('rforest')",
            shiny::checkboxInput(
              "rforestParam_replace",
              "Random Forest: replace",
              TRUE
            )
          ),
          shiny::conditionalPanel(
            condition = "input.models.includes('rforest')",
            shiny::selectInput(
              "rforestParam_maxnodes",
              "Random Forest: maxnodes",
              10
            )
          ),

          # Parameters for svm
          shiny::conditionalPanel(
            condition = "input.models.includes('svm')",
            shiny::selectInput(
              "svmParam_kernel",
              "SVM: kernel",
              list(
                "linear",
                "polynomial",
                "radial basis",
                "gaussian"
              )
            )
          ),
          shiny::conditionalPanel(
            condition = "input.models.includes('svm') & input.svmParam_kernel == 'polynomial'",
            shiny::numericInput(
              "svmParam_degree",
              "SVM: degree",
              3
            )
          ),
          shiny::conditionalPanel(
            condition = "input.models.includes('svm') & input.svmParam_kernel != 'linear'",
            shiny::numericInput(
              "svmParam_gamma",
              "SVM: gamma",
              1
            )
          ),
          shiny::conditionalPanel(
            condition = "input.models.includes('svm') & (input.svmParam_kernel == 'polynomial' |  input.svmParam_kernel != 'sigmoid')",
            shiny::selectInput(
              "svmParam_coef0",
              "SVM: coef0",
              0
            )
          ),

          # Parameters for nnet
          shiny::conditionalPanel(
            condition = "input.models.includes('nnet')",
            shiny::numericInput(
              "nnetParam_nnet",
              "Neural network: size",
              10
            )
          ),
          shiny::conditionalPanel(
            condition = "input.models.includes('nnet')",
            shiny::numericInput(
              "nnetParam_decay",
              "Neural network: decay",
              0
            )
          ),
          shiny::conditionalPanel(
            condition = "input.models.includes('nnet')",
            shiny::numericInput(
              "nnetParam_maxit",
              "Neural network: maxit",
              100
            )
          ),

          # Parameters for Parcelling
          shiny::conditionalPanel(
            condition = "input.reject.includes('parcelling') & input.parcellingParam.includes('probs')",
            shiny::selectizeInput(
              "parcellingParam_probs",
              "Parcelling hyperparameter probs (a numeric vector is expected - default: seq(0, 1, 0.25))",
              choices = NULL,
              multiple = TRUE,
              options = list(create = TRUE)
            )
          ),
          shiny::conditionalPanel(
            condition = "input.reject.includes('parcelling') & input.parcellingParam.includes('alpha')",
            shiny::selectizeInput(
              "parcellingParam_alpha",
              "Parcelling hyperparameter alpha (a numeric vector is expected - default: rep(1, length(probs) - 1))",
              choices = NULL,
              multiple = TRUE,
              options = list(create = TRUE)
            )
          ),

          # Parameters for reclassification
          shiny::conditionalPanel(
            condition = "input.reject.includes('reclassification')",
            shiny::numericInput(
              "reclassificationParam_thresh",
              "Reclassification hyperparameter: threshold at which hard classification is done",
              0.5
            )
          )
        ),

        # Show a plot of the generated distribution
        shiny::mainPanel(tabsetPanel(
          shiny::tabPanel(
            "ROC curves",
            fluidRow(
              splitLayout(
                cellWidths = c("50%", "50%"),
                plotOutput("distPlot1"),
                plotOutput("distPlot2")
              )
            )
          ),
          shiny::tabPanel("Gini indices", tableOutput("gini_reject"))
        ))
      )
    ),



    shiny::tabPanel(
      "Quantization",
      # Sidebar with a slider input for number of bins
      shiny::sidebarLayout(
        sidebarPanel(
          # Input: Select data ----

          shiny::selectInput(
            "selectedDataQuantization",
            "Data selection",
            list("lendingClub")
          )
        ),

        # Show a plot of the generated distribution
        shiny::mainPanel()
      )
    ),



    shiny::tabPanel(
      "Logistic Regression Trees",
      # Sidebar with a slider input for number of bins
      shiny::sidebarLayout(
        sidebarPanel(
          # Input: Select data ----

          shiny::selectInput(
            "selectedDataLogisticRegressionTrees", "
                                Data selection",
            list("lendingClub")
          )
        ),

        # Show a plot of the generated distribution
        shiny::mainPanel()
      )
    )
  )
)
