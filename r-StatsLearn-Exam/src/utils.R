
# ********************** #
## plotting
# ********************** #

# histograms
manyhist <- function(df, window=c(2,2)) {
  par(mfrow = window)
  for (i in 1:ncol(df)) {
    name = names(df)[i]
    hist(df[,i], main=name, breaks="FD", ylab="", xlab="",
         cex.main=2, cex.axis=1.5, las=1)
  }
}


# plot adaboost performance over iterations
plot.ada <- function(ada.err, ...) {
  # ada.err: errors that one wants to plot
  
  n.perf <- dim(ada.err)[2]  # number of performances
  ada.palette <- colorRampPalette(brewer.pal(6, "Dark2"))(n.perf)
  
  matplot(ada.err, type="l", lty=1, lwd=3, log="x",
          xlab="Rounds of boosting", ylab="Error rate",
          col=ada.palette, cex.main=1.5, ...)
  
  # abline(h=weak.err, lty=3, lwd=2)
  # text(x=60, y=weak.err*0.98, cex=1.3, paste0("stump"))
  
  # abline(h=tree.err, lty=3, lwd=2)
  # text(x=20, y=tree.err*0.96, cex=1.3,
  #      paste0(sum(tree.pr$frame$var == "<leaf>"), "-node tree"))
  
  abline(v=sapply(seq(n.perf), function(i) {which.min(ada.err[,i])}),
         lty=2, lwd=3, col=ada.palette)
  
  legend("topright", legend=c(colnames(ada.err)),
         col=c(ada.palette), lty=1, lwd=3, cex=1.5, bg="white")
}


# plot adaboost average variable importance
plot.ada.vip <- function(scores, ...) {
  dotchart(as.numeric(scores), names(scores), pch=19, xlab="Score",
           cex.main=1.5, pt.cex=1.5, ...)
}


# plot strong learners cv error
plot.sl.cverr <- function(sl.summary) {
  n.learners <- length(sl.summary$Table$Algorithm)
  # CV errors
  err <- sl.summary$Table$Ave
  lab <- sl.summary$Table$Algorithm[order(err, decreasing=TRUE)]

  # 95% confidence intervals
  CIinf <- sl.summary$Table$Ave - qnorm(0.975) * sl.summary$Table$se
  CIsup <- sl.summary$Table$Ave + qnorm(0.975) * sl.summary$Table$se

  # place the errors as dots
  dotchart(sort(err, decreasing=TRUE), labels=lab,
           main="Learners 5-fold CV error", pch=19, xlab="CV error", pt.cex=1.5)

  # add 95% confidence intervals
  arrows(
    x0=CIinf[order(err, decreasing=TRUE)],
    y0=1:n.learners,
    x1=CIsup[order(err, decreasing=TRUE)],
    y1=1:n.learners,
    code=3, angle=90, length=0.05)
}


# ********************** #
## Cross-Validation
# ********************** #

# knn cross-validation
myknn.cv <- function(full.train, nfolds, target, k.grid=NULL) {
  if (is.null(k.grid)) {
    k.grid <- seq(sqrt(nrow(full.train) * 2))
  }

  ## routine for training error
  train.err <- sapply(k.grid,
    function(k) {
      # fit model
      mod.knn <- FNN::knn(train=full.train[,-target], test=full.train[,-target],
                          cl=full.train[,target], k=k)

      # compute training error
      mean(mod.knn != full.train[,target])
    })

  ## routine for cross-validation error
  myfolds <- cut(1:nrow(full.train), breaks=nfolds, labels=FALSE)
  folds.err <- matrix(NA, length(k.grid), nfolds)

  for (i in 1:nfolds) {
    data.train <- full.train[myfolds != i,]  # training data
    data.val  <- full.train[myfolds == i,]  # validation data

    # per ogni fold calcola per ogni scenario il MSE
    folds.err[,i] <- sapply(k.grid,
      function(k) {
        # fit model
        mod.knn <- FNN::knn(train=data.train[,-target], test=data.val[,-target],
                            cl=data.train[,target], k=k)

        # compute CV error
        mean(mod.knn != data.val[,target])
      })
  }

  # returns train error and CV error
  cv.err <- rowMeans(folds.err)

  # training error & cv error
  return(cbind(train.err, cv.err))
}


# decision tree cross-validation
tree.cv <- function(full.train, formula, target, control, nfolds=5, seed=111) {
  myfolds <- cut(1:nrow(full.train), breaks=nfolds, labels=FALSE)

  folds.metrics <- c()

  for (i in 1:nfolds) {

    data.train <- full.train[myfolds != i,]  # training data
    data.val <- full.train[myfolds == i,]  # validation data

    # control <- rpart.control(maxdepth=d, cp=-1, minsplit=0, xval=0)
    tree <- rpart(formula, data=data.train, method="class", control=control)

    err <- 1 - accuracy.score(data.val[,target],
                              predict(tree, type="class",
                                      newdata=data.val[,-target]))

    folds.metrics <- c(folds.metrics, err)

  }

  return(mean(folds.metrics))
}


# random forest cross-validation
rf.cv <- function(B.max, full.train, nfolds, target, seed=111) {
  myfolds <- cut(1:nrow(full.train), breaks=nfolds, labels=FALSE)

  folds.metrics <- matrix(NA, B.max, nfolds)

  for (i in 1:nfolds) {
    set.seed(seed)

    data.train <- full.train[myfolds != i,]  # training data
    data.val <- full.train[myfolds == i,]  # validation data

    mod.rf <- randomForest(x=data.train[,-target], y=as.factor(data.train[,target]),
                           xtest=data.val[,-target], ytest=as.factor(data.val[,target]),
                           ntree=B.max, importance=FALSE)

    folds.metrics[,i] <- mod.rf$test$err.rate[,1]
  }

  return(rowMeans(folds.metrics))
}


# adaboost cross-validation
adaboost.cv <- function(M.max, full.train, nfolds, target, control=rpart.control(),
                        lam=1, bag=1, seed=111) {
  myfolds <- cut(1:nrow(full.train), breaks=nfolds, labels=FALSE)

  folds.metrics <- matrix(NA, M.max, nfolds)

  # reproducibility due to subsampling
  # if (bag < 1) {set.seed(seed)}
  for (i in 1:nfolds) {
    if (bag < 1) {set.seed(seed)}

    data.train <- full.train[myfolds != i,]  # training data
    data.val <- full.train[myfolds == i,]  # validation data

    # compute validation error for each fold for each round of boosting
    folds.metrics[,i] <- {
      # fit model, package ada
      mod.adaboost <- ada::ada(x=as.matrix(data.train[,-target]),
                               y=data.train[,target],
                               test.x=as.matrix(data.val[,-target]),
                               test.y=data.val[,target],
                               loss="exponential", type="discrete",
                               iter=M.max, nu=lam, bag.frac=bag,
                               control=control)

      mod.adaboost$model$errs[,3]
    }

  }

  # compute error rate for each value in grid
  return(rowMeans(folds.metrics))
}


# ********************** #
## AdaBoost average variable importance
# ********************** #

adaboost.vip <- function(target, train, M, nu, bag, control=rpart.control(), seed=111) {
  vars.boost <- rep(0, ncol(train) - 1)  # sum of importances
  avg.run <- 20  # total runs

  set.seed(seed)
  for (i in 1:avg.run) {
    # for each m the bootstrap is different
    boost.vip.mod <- ada::ada(x=as.matrix(train[,-target]), y=train[,target],
                              loss="exponential", type="discrete",
                              iter=M, nu=nu, bag.frac=bag, control=control)
    # get current importances
    boost.vip <- varplot(boost.vip.mod, plot.it=FALSE, type="scores")
    # set scores order by variable name
    vars.boost <- vars.boost + as.numeric(boost.vip[order(names(boost.vip))]) / avg.run
  }

  names(vars.boost) <- sort(names(train[,-target]))  # add variable names
  return(vars.boost)
}


# ********************** #
## Data generating process generation
# ********************** #

binary1 <- function(n, p1=0.5, mu0=0, mu1=2, sigma=1) {
  set.seed(1)
  Y <- rbinom(n, 1, p1)
  # Y <- 2 * Y - 1
  # Y <- factor(Y)
  n1 <- sum(Y == 1)

  # if Y == 1 generate one sample from X|Y=1
  # otherwise generate one sample from X|Y=-1
  X <- matrix(NA, n, 1)
  X[Y == 1,] <- rnorm(n1, mu1, sigma)
  X[Y == 0,] <- rnorm(n - n1, mu0, sigma)

  return(as.data.frame(cbind(Y=Y, X1=X[,1])))
}
