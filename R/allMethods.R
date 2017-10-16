

methods::setMethod("show", signature(object = "reject_infered"), function(object) {
     print(object@infered_model)
})


methods::setMethod("show",
                   signature(object = "discretization"),
                   function(object) {
     print(object@best.disc[[1]])
})

print.discretization <- function(object) {
     print(object@best.disc[[1]])
}

# methods::setMethod("summary", signature(object = "discretization"), function(object) {
#      summary(object@best.disc[[1]])
# })

summary.discretization <- function(object) {
     summary(object@best.disc[[1]])
}

# methods::setMethod("predict", signature(object = "discretization"), function(object) {
#      object@sides
# })

predict.discretization <- function(object, predictors) {
     predict(object@best.disc[[1]],as.data.frame(discretize_link(object@best.disc[[2]],predictors)))
}

# methods::setMethod("plot", signature(object = "discretization"), function(object) {
#      plot(object@best.disc[[1]])
# })

plot.discretization <- function(object) {
     for (j in 1:length(object@best.disc[[2]])) {
          graphics::plot(object@best.disc[[1]]$data[j],object@best.disc[[1]][[j]]$data)
     }
     # plot()
     # plot(object@best.disc[[1]])
}
