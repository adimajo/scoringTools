#' Discretize a dataset using a trained SEM discretization scheme
#'
#' This function discretizes a user-specified dataset using a pre-trained SEM discretization scheme.
#' @param link A multinomial logit model.
#' @param df The dataframe, containing the same variables as the one used to train the discretization scheme, to be discretized.
#' @keywords discretization, predict
#' @export
#' @examples
#' discretize.link()


discretize.link <- function(link,df) {

     n = nrow(df)
     d = ncol(df)
     emap = array(0,c(n,d))

     for (j in sample(1:d)) {
          m = length(link[[j]]$coefficients)-1
          lev = c("1",sapply(names(link[[j]]$coefficients[seq(2,length(link[[j]]$coefficients),2)]), function(lev_name) substr(lev_name,start=3,stop=nchar(lev_name))))
          long_dataset <- data.frame(x = as.vector(sapply(df[,j], function(var) rep(var,m))), names = as.character(as.vector(rep(lev[seq(1:m)],n))))
          t = predict(link[[j]], newdata = long_dataset, choiceVar = "names", type="probs")
          emap[j,] <- apply(t,1,function(p) names(which.max(p)))
     }

     return(emap)

}
