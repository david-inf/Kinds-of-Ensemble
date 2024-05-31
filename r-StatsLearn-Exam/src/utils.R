
## Explore dataset

# class distribution
dataset.distrib <- function(y) {
  return(table(y) / length(y))
}


## Binary classification metrics

# confusion matrix
confusion.matrix <- function(y.true, y.pred) {
  # if (prob) {
  #   y.pred <- ifelse(y.prob > 0.5, 1, -1)
  # } else {
  #   y.pred <- y.prob
  # }
  # values <- unique(y.true)

  true.pos <- sum(y.pred == 1 & y.true == 1)
  false.pos <- sum(y.pred == 1 & y.true == -1)
  false.neg <- sum(y.pred == -1 & y.true == 1)
  true.neg <- sum(y.pred == -1 & y.true == -1)

  # horizontal: predicted class
  # vertical: actual class
  #      1  & -1
  #  1 & TP & FN
  # -1 & FP & TN
  mat <- matrix(c(true.pos, false.pos, false.neg, true.neg), 2, 2)
  rownames(mat) <- colnames(mat) <- c("1", "-1")

  return(mat)
}

# accuracy
accuracy.score <- function(y.true, y.pred) {
  correct <- sum(diag(confusion.matrix(y.true, y.pred)))

  return(correct / length(y.true))
}

# recall
recall.score <- function(y.true, y.pred) {
  true.pos <- confusion.matrix(y.true, y.pred)[1, 1]
  false.neg <- confusion.matrix(y.true, y.pred)[1, 2]

  return(true.pos / (true.pos + false.neg))
}

# specificity
specificity.score <- function(y.true, y.pred) {
  true.neg <- confusion.matrix(y.true, y.pred)[2, 2]
  false.pos <- confusion.matrix(y.true, y.pred)[2, 1]

  return(true.neg / (true.neg + false.pos))
}

# balanced accuracy
balaccuracy.score <- function(y.true, y.pred) {
  return(0.5 * (recall.score(y.true, y.pred) +
                  specificity.score(y.true, y.pred)))
}

# F1 score
f1.score <- function(y.true, y.pred) {
  true.pos <- confusion.matrix(y.true, y.pred)[1, 1]
  false.pos <- confusion.matrix(y.true, y.pred)[2, 1]
  false.neg <- confusion.matrix(y.true, y.pred)[1, 2]

  return(2 * true.pos / (2 * true.pos + false.pos + false.neg))
}

# misclassification error rate
misclass.error <- function(y.true, y.pred) {
  N <- length(y.true)
  # if (prob) {
  #   y.pred <- ifelse(y.prob > 0.5, 1, -1)
  # } else {
  #   y.pred <- y.prob
  # }

  return(sum(y.true != y.pred) / N)
}


## Ensemble metrics

# confusion matrix, accuracy, balanced accuracy
first.metrics <- function(train.true, train.pred, test.true=NULL, test.pred=NULL) {
  print(confusion.matrix(train.true, train.pred))

  print(paste0("Train score: ", accuracy.score(train.true, train.pred)))
  print(paste0("Train balanced score: ", balaccuracy.score(train.true, train.pred)))

  if (!is.null(test.true)) {
    print(paste0("Test score: ", accuracy.score(test.true, test.pred)))
    print(paste0("Test balanced score: ", balaccuracy.score(test.true, test.pred)))
  }
}


## Cross-Validation

# knn cross-validation
myknn.cv <- function(full.train, nfolds, target, k.grid=NULL) {
  if (is.null(k.grid)) {
    k.grid <- seq(sqrt(nrow(full.train) * 2))
  }

  myfolds <- cut(1:nrow(full.train), breaks=nfolds, labels=FALSE)
  
  folds.err <- matrix(NA, length(k.grid), nfolds)
  
  for (i in 1:nfolds) {
    data.train <- full.train[myfolds != i,]  # training data
    data.val  <- full.train[myfolds == i,]  # validation data
    
    # per ogni fold calcola per ogni scenario il MSE
    folds.err[,i] <- sapply(k.grid,
                            function(k) {
                              # fit model
                              mod.knn <- FNN::knn(train=data.train[,-target],
                                                  test=data.val[,-target],
                                                  cl=data.train[,target], k=k)

                              # compute CV error
                              mean(mod.knn != data.val[,target])
                            })
  }
  
  rowMeans(folds.err)
}

# adaboost cross-validation
adaboost.cv <- function(M.max, full.train, nfolds, target, lam=1, bag=1, seed=111) {
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
                               iter=M.max, nu=lam, bag.frac=bag)

      mod.adaboost$model$errs[,3]
    }

  }

  # compute error rate for each value in grid
  rowMeans(folds.metrics)
}


## AdaBoost average variable importance

adaboost.vip <- function(nvars, target, train, M, nu, bag, seed=111) {
  vars.boost <- rep(0, nvars - 1)  # sum of importances
  avg.run <- 30  # runs

  set.seed(seed)
  for (i in 1:avg.run) {
    # for each m the bootstrap is different
    boost.vip.mod <- ada::ada(x=as.matrix(train[,-target]), y=train[,target],
                              loss="exponential", type="discrete",
                              iter=M, nu=nu, bag.frac=bag)
    # get current importances
    boost.vip <- varplot(boost.vip.mod, plot.it=FALSE, type="scores")
    # set scores order by variable name
    vars.boost <- vars.boost + as.numeric(boost.vip[order(names(boost.vip))]) / avg.run
  }

  names(vars.boost) <- sort(names(train[,-target]))  # add variable names
  return(vars.boost)
}


## Data generating process generation

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
