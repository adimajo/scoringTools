#' Discretize a dataset using a trained SEM discretization scheme
#'
#' This function discretizes a user-specified dataset using a pre-trained SEM discretization scheme.
#' @param link A multinomial logit model.
#' @param df The dataframe, containing the same variables as the one used to train the discretization scheme, to be discretized.
#' @keywords discretization, predict
#' @importFrom stats predict
#' @export
#' @references
#' Asad Hasan, Wang Zhiyu and Alireza S. Mahani (2015). mnlogit: Multinomial Logit Model. R package version 1.2.4. \url{https://CRAN.R-project.org/package=mnlogit}
#' @examples
#' set.seed(1)
#' x = matrix(runif(100), nrow = 100, ncol = 1)
#' cuts = seq(0,1,length.out = 4)
#' xd = as.numeric(cut(x,cuts))
#'
#' long_dataset <- data.frame(e = as.vector(sapply(xd,function(var) (seq(1:3)[seq(1:3)]==var))),x = as.vector(sapply(x[,1], function(var) rep(var,3))),
#' names = as.character(as.vector(rep(seq(1:3)[seq(1:3)],length(x)))),stringsAsFactors=FALSE)
#' link <- mnlogit::mnlogit(e ~ 1 | x | 1, data=long_dataset, choiceVar = "names")
#' discretize_link(link,as.data.frame(x))


discretize_link <- function(link,df) {

     n = nrow(df)
     d = ncol(df)
     types_data <- sapply(df,class)
     if (sum(!(types_data %in% c("numeric","factor")))>0) {
          stop("Unsupported data types. Columns of predictors must be numeric or factor.")
     }
     emap = array(0,c(n,d))

     if (d>1) {
          for (j in sample(1:d)) {
               if (types_data[j]=="numeric") {
                    m = length(link[[j]]$coefficients)/2 + 1
                    lev = c("1",sapply(names(link[[j]]$coefficients[seq(2,length(link[[j]]$coefficients),2)]), function(lev_name) substr(lev_name,start=3,stop=nchar(lev_name))))
                    long_dataset <- data.frame(x = as.vector(sapply(df[,j], function(var) rep(var,m))), names = as.character(as.vector(rep(lev[seq(1:m)],n))))
                    t = predict(link[[j]], newdata = long_dataset, choiceVar = "names", type="probs")
               } else {
                    t = prop.table(t(sapply(df[,j],function(row) link[[j]][,row])),1)
               }
               emap[,j] <- apply(t,1,function(p) names(which.max(p)))
          }
     } else if (types_data=="numeric") {
          m = length(link$coefficients)/2 + 1
          lev = c("1",sapply(names(link$coefficients[seq(2,length(link$coefficients),2)]), function(lev_name) substr(lev_name,start=3,stop=nchar(lev_name))))
          long_dataset <- data.frame(x = as.vector(sapply(df, function(var) rep(var,m))), names = as.character(as.vector(rep(lev[seq(1:m)],n))))
          t = predict(link, newdata = long_dataset, choiceVar = "names", type="probs")
          emap[,1] <- apply(t,1,function(p) names(which.max(p)))
     } else {
          t = prop.table(t(sapply(df,function(row) link[,row])),1)
          emap[,1] <- apply(t,1,function(p) names(which.max(p)))
     }

     return(emap)

}
