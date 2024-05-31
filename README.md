# Statistical Learning final project

## Topic description

Binary classification on Heart disease dataset

Methods:
- kNN with CV for optimal k
- Regularized logistic regression?
- **CART** with pruning
    - Grow the tree without restriction
    - Prune the tree at optimal CV cp
- **Random forest**. Grow the forest with 500 trees. Set mtry as floor(sqrt(ncol(x))). Choose the optimal number of trees based on OOB error.
    - Variable importance
- **AdaBoost** with subsampling and shrinkage. CV for selecting optimal number of trees based on CV error
    - Additive model with d=1 (decision stump)
        - Variable importance
    - d>1 like d=2
        - Variable importance
- **Super learner**
    - `SL.mean`: mean over all outcomes, naive guess
    - `SL.rpart`
    - `SL.knn`
    - `SL.randomForest`
    - `SL.glm`
    - `SL.glmnet`
    - `SL.xgboost`

Model performance

| Model         | Train score       | Test score        |
|---------------|-------------------|-------------------|
| kNN           | 0.902887139107612 | 0.894973743435859 |
| CART          | 0.89238845144357  | 0.822205551387847 |
| Random forest | 0.880764904386952 | 0.888972243060765 |
| AdaBoost      | 0.983127109111361 | 0.887471867966992 |
| Super learner | 0.                | 0.                |
|               | 0.                | 0.                |
|               | 0.                | 0.                |
|               | 0.                | 0.                |
