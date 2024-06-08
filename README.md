# Statistical Learning final project

## Topic description and learning methods

Binary classification on apple quality dataset:
- **Size**: size of the apple
- **Weight**: weight of the apple
- **Sweetness**: how sweet an apple is
- **Crunchiness**: how crisp the apple is
- **Juiciness**: how juicy an apple is
- **Ripeness**: how ripe an apple is
- **Acidity**: how acid an apple is
- **Quality**: the quality is *good* or *bad*

Methods:
- **kNN**, package `FNN`
    - CV (from scratch) for optimal `k*`
    - Bias-Variance tradeoff
- **Regularized logistic regression**, package `glmnet`
    - Ridge
    - Lasso
    - Elastic net
- **CART** with pruning, package `rpart`
    - Grow the tree without restriction
    - Prune the tree at optimal CV `cp*`
- **Random forest**, package `randomForest`
    - `mtry`=`floor(sqrt(ncol(x)))`
    - Grow `B=500` trees, choose optimal `B*` based on OOB error, plot Bias-Variance tradeoff
    - Variable importance
- **AdaBoost** with subsampling and shrinkage, package `ada`
    - Run algorithm for `M=1000` iterations, choose optimal `M*` based on CV error (from scratch)
    - Additive model with `d=1` (decision stump)
        - Bias-Variance tradeoff
        - Variable importance
    - d>1 like `d=4`
        - Bias-Variance tradeoff
        - Variable importance
- **Super learner**, package `SuperLearner`
    - Strong learners:
        - `SL.mean`: mean over all outcomes, naive guess
        - `SL.rpart`: grown CART
        - `SL.knn`: k-nearest neighbors
        - `SL.randomForest`: random forest
        - `SL.glm`: logistic regression
        - `SL.glmnet`: lasso/ridge/elastic net logistic regression
        - `SL.gbm`: gradient boosting machine with bernoulli deviance with `d=1` and `d=4`
    - Compute external CV error with `CV.SuperLearner`

Performance:
| Model         | Train score       | Test score        |
|---------------|-------------------|-------------------|
| kNN           | 0.902887139107612 | 0.894973743435859 |
| Grown CART    | 0.884139482564679 | 0.828957239309827 |
| Pruned CART   | 0.884139482564679 | 0.829014138412355 |
| Random Forest | 0.880764904386952 | 0.888972243060765 |
| AdaBoost d=1  | 0.79827521559805  | 0.792198049512378 |
| AdaBoost d=4  | 0.999625046869141 | 0.884471117779445 |
| SL (full)     | 0.930258717660292 | 0.889722430607652 |
| SL (reduced)  | 0.989876265466817 | 0.878469617404351 |
|               | 0.                | 0.                |
|               | 0.                | 0.                |
