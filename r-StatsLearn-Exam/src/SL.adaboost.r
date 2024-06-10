SL.adaboost <- function(Y, X, newX, family, obsWeights, id, ada.trees=100, shrinkage=0.1, bag.fraction=0.5, base.depth=1) {
  require("ada")
  require("rpart")
  
  if (base.depth == 1) {
    control <- rpart.control(maxdepth=1, cp=-1, minsplit=0, xval=0)
  } else {
    control <- rpart.control(maxdepth=base.depth, cp=-1, maxcompete=1, xval=0)
  }
  
  # if(family$family == 'gaussian') {
  #   
  # }
  if(family$family == 'binomial') {
    fit.ada <- ada::ada(x=as.matrix(X), y=Y, test.x=as.matrix(newX),
                        loss="exponential", type="discrete",
                        iter=ada.trees, nu=shrinkage, bag.frac=bag.fraction,
                        control=control)
  }
  
  # optimal number of iterations
  # best.iter <- 
  # pred is the predicted responses for newX (on the scale of the outcome)
  pred <- predict(fit.ada, newdata=newX, type="prob")[,2]
  # fit returns all objects needed for predict.SL.template
  fit <- list(object = fit.ada)
  # return a list with pred and fit
  out <- list(pred = pred, fit = fit)
  # declare class of fit for predict.SL.template
  class(out$fit) <- "SL.adaboost"
  
  return(out)
}

predict.SL.adaboost <- function(object, newdata, family, X = NULL, Y = NULL, ...) {
  pred <- predict(object$object, newdata = newdata, type="prob")[,2]
  return(pred)
}