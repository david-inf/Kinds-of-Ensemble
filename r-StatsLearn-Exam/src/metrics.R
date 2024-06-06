
## Explore dataset

# class distribution
dataset.distrib <- function(y) {
  return(table(y) / length(y))
}

# histograms
manyhist <- function(df) {
  par(mfrow = c(2,2))
  for (i in 1:ncol(df)) {
    name = names(df)[i]
    hist(df[,i], main=name, breaks="FD", ylab="", xlab="",
         cex.main=2, cex.axis=1.5, las=1)
  }
}


## Binary classification metrics

# confusion matrix
confusion.matrix <- function(y.true, y.pred, pos=1, neg=0) {
  true.pos <- sum(y.pred == pos & y.true == pos)
  false.pos <- sum(y.pred == pos & y.true == neg)
  false.neg <- sum(y.pred == neg & y.true == pos)
  true.neg <- sum(y.pred == neg & y.true == neg)
  
  # horizontal: predicted class
  # vertical: actual class
  #      1  & -1
  #  1 & TP & FN
  # -1 & FP & TN
  mat <- matrix(c(true.pos, false.pos, false.neg, true.neg), 2, 2)
  rownames(mat) <- colnames(mat) <- as.character(c(pos, neg))
  
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
