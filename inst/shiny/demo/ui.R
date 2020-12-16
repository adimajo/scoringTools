ui <- shiny::fluidPage(
  shiny::navbarPage(
    "Statistical problems in Credit Scoring",
    shiny::tabPanel(
      "Data import",
      shiny::sidebarPanel(
        shiny::fileInput(
          "imported_files",
          "Choose CSV File(s)",
          multiple = TRUE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        shiny::checkboxInput(
          "use_lendingClub",
          "Use lendingClub dataset?",
          FALSE
        ),
        shiny::checkboxInput(
          "use_MAR_well_specified",
          "Generate MAR well-specified data?",
          FALSE
        ),
        shiny::conditionalPanel(
          "input.use_MAR_well_specified",
          shiny::numericInput(
            "n_MAR_well_specified",
            "Number of samples for MAR well-specified:",
            min = 100,
            value = 1000
          )
        ),
        shiny::conditionalPanel(
          "input.use_MAR_well_specified",
          shiny::numericInput(
            "dim_MAR_well_specified",
            "Dimension of samples for MAR well-specified:",
            min = 1,
            value = 3
          )
        ),
        shiny::checkboxInput(
          "use_MAR_misspecified",
          "Generate MAR misspecified data?",
          FALSE
        ),
        shiny::conditionalPanel(
          "input.use_MAR_misspecified",
          shiny::numericInput(
            "n_MAR_misspecified",
            "Number of samples for MAR misspecified:",
            min = 100,
            value = 1000
          )
        ),
        shiny::conditionalPanel(
          "input.use_MAR_misspecified",
          shiny::numericInput(
            "dim_MAR_misspecified",
            "Dimension of samples for MAR misspecified:",
            min = 1,
            value = 3
          )
        ),
        shiny::checkboxInput(
          "use_MNAR",
          "Generate MNAR data?",
          FALSE
        ),
        shiny::conditionalPanel(
          "input.use_MNAR",
          shiny::numericInput(
            "n_MNAR",
            "Number of samples for MNAR:",
            min = 100,
            value = 1000
          )
        ),
        shiny::conditionalPanel(
          "input.use_MNAR",
          shiny::numericInput(
            "dim_MNAR",
            "Dimension of samples for MNAR:",
            min = 1,
            value = 3
          )
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
            "bins_reject",
            "Fraction of rejected applications:",
            min = 0,
            max = 100,
            value = 30
          ),
          shiny::sliderInput(
            "bins_test",
            "Fraction of all applications for test:",
            min = 0,
            max = 100,
            value = 30
          ),
          shiny::checkboxInput(
            "deleteSamplesRejectInference",
            "Remove test samples for which there are unknown factor levels",
            TRUE
          ),

          # Input: Select data ----

          shiny::selectInput(
            "selectedDataRejectInference",
            "Data selection",
            list("lendingClub")
          ),

          shiny::selectInput(
            "var_cible",
            "Good / bad feature",
            list()
          ),
          "If there are NAs, these observations will count as 'not financed'",

          # shiny::selectInput(
          #   "var_reject",
          #   "Accept / Reject feature",
          #   list()
          # ),
          # "If a 'score', the fraction defined above will depend on it directly.",
          # "If a 0/1 feature, an Accept / Reject model (logistic regression) will be learnt.",

          # Input: Select models to compute ----
          shiny::selectInput(
            "modelsRejectInference",
            "Models",
            multiple = TRUE,
            choices = c(
              Logistic = "logistic",
              DecisionTree = "tree",
              RandomForest = "rforest",
              SVM = "svm",
              Neurones = "nnet"
            ),
            selected = "logistic"
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
            condition = "input.modelsRejectInference.includes('rforest')",
            tags$hr(),
            "randomForest options",
            shiny::numericInput(
              "rforestParam_ntree",
              "Random Forest: ntree",
              100
            ),
            shiny::numericInput(
              "rforestParam_mtry",
              "Random Forest: mtry",
              2
            ),
            shiny::checkboxInput(
              "rforestParam_replace",
              "Random Forest: replace",
              TRUE
            ),
            shiny::numericInput(
              "rforestParam_maxnodes",
              "Random Forest: maxnodes",
              10
            )
          ),

          # Parameters for svm
          shiny::conditionalPanel(
            condition = "input.modelsRejectInference.includes('svm')",
            tags$hr(),
            "svm options",
            shiny::selectInput(
              "svmParam_kernel",
              "SVM: kernel",
              list(
                "linear",
                "polynomial",
                "radial",
                "sigmoid"
              )
            )
          ),
          shiny::conditionalPanel(
            condition = "input.modelsRejectInference.includes('svm') & input.svmParam_kernel == 'polynomial'",
            shiny::numericInput(
              "svmParam_degree",
              "SVM: degree",
              3
            )
          ),
          shiny::conditionalPanel(
            condition = "input.modelsRejectInference.includes('svm') & input.svmParam_kernel != 'linear'",
            shiny::numericInput(
              "svmParam_gamma",
              "SVM: gamma",
              1
            )
          ),
          shiny::conditionalPanel(
            condition = "input.modelsRejectInference.includes('svm') & (input.svmParam_kernel == 'polynomial' |  input.svmParam_kernel != 'sigmoid')",
            shiny::numericInput(
              "svmParam_coef0",
              "SVM: coef0",
              0
            )
          ),

          # Parameters for nnet
          shiny::conditionalPanel(
            condition = "input.modelsRejectInference.includes('nnet')",
            tags$hr(),
            "nnet options",
            shiny::numericInput(
              "nnetParam_nnet",
              "Neural network: size",
              10
            ),
            shiny::numericInput(
              "nnetParam_decay",
              "Neural network: decay",
              0
            ),
            shiny::numericInput(
              "nnetParam_maxit",
              "Neural network: maxit",
              100
            )
          ),

          # Parameters for Parcelling
          shiny::conditionalPanel(
            condition = "input.reject.includes('parcelling')",
            tags$hr(),
            "Parcelling options",
            shiny::selectizeInput(
              "parcellingParam_probs",
              "Parcelling hyperparameter probs (a numeric vector is expected - default: seq(0, 1, 0.25))",
              choices = NULL,
              multiple = TRUE,
              options = list(create = TRUE)
            ),
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
            tags$hr(),
            "Reclassification option",
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
            plotly::plotlyOutput(
              "roc_tous_reject_inference"
            ),
            plotly::plotlyOutput(
              "roc_tous_reject_inference_financed"
            )
          ),
          shiny::tabPanel(
            "Gini indices",
            shiny::checkboxInput(
              "report_train_reject",
              "Report train metrics",
              FALSE
            ),
            shiny::checkboxInput(
              "CI_gini_reject",
              "Display confidence intervals",
              TRUE
            ),
            shiny::conditionalPanel(
              "input.CI_gini_reject",
              shiny::numericInput(
                "confidence_level_reject",
                "Confidence level",
                value = 0.95,
                min = 0.1,
                max = 0.9999
              )
            ),
            DT::DTOutput("gini_reject")
          )
        ))
      )
    ),

    shiny::tabPanel(
      "Quantization",
      # Sidebar with a slider input for number of bins
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # Input: Select data ----
          shiny::sliderInput(
            "bins_test_quantization",
            "Fraction of all applications for test:",
            min = 0,
            max = 100,
            value = 30
          ),
          shiny::checkboxInput(
            "deleteSamplesQuantization",
            "Remove test samples for which there are unknown factor levels",
            TRUE
          ),

          shiny::selectInput(
            "selectedDataQuantization",
            "Data selection",
            list("lendingClub")
          ),

          shiny::selectInput(
            "var_cible_quantization",
            "Good / bad feature",
            list()
          ),

          shiny::selectInput(
            "modelsQuantization",
            "Models",
            multiple = TRUE,
            choices = c(
              linear = "linear",
              glmdisc = "glmdisc",
              chi2 = "chi2",
              chiMerge = "chiM",
              echi2 = "extended chi2",
              modchi2 = "modified chi2",
              mdlp = "mdlp",
              topdown = "topdown"
            ),
            selected = "glmdisc"
          ),

          shiny::conditionalPanel(
            condition = "input.modelsQuantization.includes('glmdisc')",
            tags$hr(),
            "glmdisc options",
            shiny::checkboxInput(
              "glmdiscParam_interact",
              "glmdisc: interact",
              FALSE
            ),
            shiny::checkboxInput(
              "glmdiscParam_validation",
              "glmdisc: validation",
              FALSE
            ),
            shiny::checkboxInput(
              "glmdiscParam_test",
              "glmdisc: test",
              FALSE
            ),
            shiny::selectInput("glmdiscParam_criterion",
              "glmdisc: criterion",
              choices = c("gini", "aic", "bic"),
              selected = "bic"
            ),
            shiny::numericInput(
              "glmdiscParam_iter",
              "glmdisc: number of interations",
              100
            ),
            shiny::numericInput(
              "glmdiscParam_m_start",
              "glmdisc: maximum number of levels",
              10
            )
          ),

          shiny::conditionalPanel(
            condition = "input.modelsQuantization.includes('chi2')",
            tags$hr(),
            "chi2 options",
            shiny::checkboxInput(
              "chi2Param_validation",
              "chi2: validation",
              FALSE
            ),
            shiny::checkboxInput(
              "chi2Param_test",
              "chi2: test",
              FALSE
            ),
            shiny::selectInput("chi2Param_criterion",
              "chi2: criterion",
              choices = c("gini", "aic"),
              selected = "aic"
            ),
          ),

          shiny::conditionalPanel(
            condition = "input.modelsQuantization.includes('chiM')",
            tags$hr(),
            "chiMerge options",
            shiny::checkboxInput(
              "chiMParam_validation",
              "chiMerge: validation",
              FALSE
            ),
            shiny::checkboxInput(
              "chiMParam_test",
              "chiMerge: test",
              FALSE
            ),
            shiny::selectInput("chiMParam_criterion",
              "chiMerge: criterion",
              choices = c("gini", "aic"),
              selected = "aic"
            ),
          ),

          shiny::conditionalPanel(
            condition = "input.modelsQuantization.includes('extended chi2')",
            tags$hr(),
            "extended chi2 options",
            shiny::checkboxInput(
              "echi2Param_validation",
              "extended chi2: validation",
              FALSE
            ),
            shiny::checkboxInput(
              "echi2Param_test",
              "extended chi2: test",
              FALSE
            ),
            shiny::selectInput("echi2Param_criterion",
              "extended chi2: criterion",
              choices = c("gini", "aic"),
              selected = "aic"
            ),
          ),

          shiny::conditionalPanel(
            condition = "input.modelsQuantization.includes('modified chi2')",
            tags$hr(),
            "modified chi2 options",
            shiny::checkboxInput(
              "modchi2Param_validation",
              "modified Chi2: validation",
              FALSE
            ),
            shiny::checkboxInput(
              "modchi2Param_test",
              "modified Chi2: test",
              FALSE
            ),
            shiny::selectInput("modchi2Param_criterion",
              "modified Chi2: criterion",
              choices = c("gini", "aic"),
              selected = "aic"
            ),
          ),

          shiny::conditionalPanel(
            condition = "input.modelsQuantization.includes('mdlp')",
            tags$hr(),
            "mdlp options",
            shiny::checkboxInput(
              "mdlpParam_validation",
              "mdlp: validation",
              FALSE
            ),
            shiny::checkboxInput(
              "mdlpParam_test",
              "mdlp: test",
              FALSE
            ),
            shiny::selectInput("mdlpParam_criterion",
              "mdlp: criterion",
              choices = c("gini", "aic"),
              selected = "aic"
            ),
          ),

          shiny::conditionalPanel(
            condition = "input.modelsQuantization.includes('topdown')",
            tags$hr(),
            "topdown options",
            shiny::checkboxInput(
              "topdownParam_validation",
              "topdown: validation",
              FALSE
            ),
            shiny::checkboxInput(
              "topdownParam_test",
              "mdlp: test",
              FALSE
            ),
            shiny::selectInput("topdownParam_criterion",
              "topdown: criterion",
              choices = c("gini", "aic"),
              selected = "aic"
            ),
          ),
        ),
        shiny::mainPanel(shiny::tabsetPanel(
          shiny::tabPanel(
            "ROC curves",
            plotly::plotlyOutput(
              "roc_tous_quantization"
            )
          ),
          shiny::tabPanel(
            "Gini indices",
            shiny::checkboxInput(
              "report_train_quantization",
              "Report train metrics",
              FALSE
            ),
            shiny::checkboxInput(
              "CI_gini_quantization",
              "Display confidence intervals",
              TRUE
            ),
            shiny::conditionalPanel(
              "input.CI_gini_quantization",
              shiny::numericInput(
                "confidence_level_quantization",
                "Confidence level",
                value = 0.95,
                min = 0.1,
                max = 0.9999
              )
            ),
            DT::DTOutput("gini_quantization")
          )
        ))
      )
    ),


    shiny::tabPanel(
      "Logistic Regression Trees",
      # Sidebar with a slider input for number of bins
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # Input: Select data ----

          shiny::selectInput(
            "selectedDataLogisticRegressionTrees",
            "Data selection",
            list("lendingClub")
          ),

          shiny::selectInput(
            "modelsLogisticRegressionTrees",
            "Models",
            multiple = TRUE,
            choices = c(
              glmtree = "glmtree",
              tree = "tree",
              mob = "mob",
              lmt = "lmt"
            ),
            selected = "glmtree"
          )
        ),

        # Show a plot of the generated distribution
        shiny::mainPanel()
      )
    )
  )
)
