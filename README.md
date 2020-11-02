![CRAN package](https://www.r-pkg.org/badges/version-ago/scoringTools)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/scoringTools)
![R package](https://github.com/adimajo/scoringTools/workflows/R%20package/badge.svg)
[![Travis build status](https://travis-ci.org/adimajo/scoringTools.svg?branch=master)](https://travis-ci.org/adimajo/scoringTools)
[![Coverage status](https://codecov.io/gh/adimajo/scoringTools/branch/master/graph/badge.svg)](https://codecov.io/github/adimajo/scoringTools?branch=master)

# Credit Scoring Tools

This package has been developed as part of a CIFRE PhD, a special PhD contract in France which is for the most part financed by a company. This company subsequently gets to choose which subject(s) are tackled.

This research has been financed by Crédit Agricole Consumer Finance (CA CF), subsidiary of the Crédit Agricole Group which provides all kinds of banking and insurance services. CA CF focuses on consumer loans, ranging from luxury cars to small electronics.

In order to accept / reject loan applications more efficiently (both quicker and to select better applicants), most financial institutions resort to Credit Scoring: given the applicant's characteristics he/she is given a Credit Score, which has been statistically designed using previously accepted applicants, and which partly decides whether the financial institution will grant the loan or not.

Two subjects are tackled in this package:

* Reject Inference: using not financed clients' information to build a scorecard (see `vignette("scoringTools")`)
* Quantization (discretization and grouping of levels)

The package's website (obtained from `pkgdown`) is available respectively at:

* [adimajo.github.io/scoringTools](https://adimajo.github.io/scoringTools)

Two other packages were made public during this PhD, which tackle more specifically:

* Quantization (discretization and grouping of levels) and interaction screening: package [glmdisc](https://cran.r-project.org/package=glmdisc) (see [adimajo.github.io/glmdisc](https://adimajo.github.io/glmdisc))
* Logistic regression trees: package [glmdisc](https://cran.r-project.org/package=glmdtree) (see [adimajo.github.io/glmtree](https://adimajo.github.io/glmtree))

## TODO: 

* A Shiny app explaining all aspects of this work is also accessible by running `runDemo()`.
     * allow column selection? + target selection
     * allow custom model + text input for call
     * parameters of model in reject inference panel (as blocks for vectors)
     * single ROC Curve for all selected models
     * Gini tab with confidence intervals
     * Quantization
          * which algorithms? (check glmdisc installed) + their parameters
          * Histogram of each feature with default rate
          * Histogram of quantized features with default rate
     * Logistic regression trees
          * check packages are installed (mob, party, glmtree)
          * which algorithms + their parameters
          * show tree structure
          * tabpanel for each segment

* plot methods for reject_infered class
