1. Regression
# Fit linear model using 10-fold CV
model <- train(
  form = price ~ .,
  data = diamonds,
  method = 'lm',
  trControl = trainControl(
    method = 'cv',
    number = 10,
    verboseIter = TRUE
  )
)
2. Classification
# Fit logistic model with AUC performance metric
model <- train(
  form = Class ~ .,
  data = Sonar,
  method = 'glm',
  trControl = trainControl(
    method = 'cv',
    number = 10,
    summaryFunction = twoClassSummary,
    classProbs = TRUE, # necessary
    verboseIter = TRUE
  )
)
# Confusion matrix
confusionMatrix(predicted, actual)

# Make ROC curve
colAUC(predicted, actual, plotROC = T)
3. Random Forest

# Fit random forest with CV-based parameter tuning
model <- train(
  quality ~ .,
  tuneLength = 3,
  data = wine,
  method = 'ranger',
  trControl = trainControl(
    method = 'cv',
    number = 5,
    verboseIter = TRUE
  )
)

# Fit random forest with CV-based parameter tuning
model <- train(
  quality ~ .,
  tuneGrid = data.frame(mtry = c(2, 3, 7)),
  data = wine,
  method = 'ranger',
  trControl = trainControl(
    method = 'cv',
    number = 5,
    verboseIter = TRUE
  )
)

# show model performance
plot(model)
#Regularized Linear Regression
# trainControl object
trCont <- trainControl(
  method = 'cv',
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # necessary
  verboseIter = TRUE
)

# Fit glmnet model
model <- train(
  y ~ .,
  data = overfit,
  method = 'glmnet',
  trControl = trCont
)

# Train glmnet with custom trainControl and tuning
model <- train(
  y ~ .,
  data = overfit,
  tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 20)),
  method = 'glmnet',
  trControl = trCont
)

# show model performance
plot(model)
#Preprocessing

# Fit glm with median imputation
model <- train(
  x = breast_cancer_x,
  y = breast_cancer_y,
  method = 'glm',
  trControl = myControl,
  preProcess = 'medianImpute'
)

# Fit glm with KNN imputation
model <- train(
  x = breast_cancer_x,
  y = breast_cancer_y,
  method = 'glm',
  trControl = myControl,
  preProcess = 'knnImpute'
)

# Fit glm with median imputation and standardization
model <- train(
  x = breast_cancer_x,
  y = breast_cancer_y,
  method = 'glm',
  trControl = myControl,
  preProcess = c('medianImpute', 'center', 'scale')
)

# Fit glm with near zero var. removal, median imput. and standardization
model  median/knn imputation -> center -> scale -> pca/spatialSign
# Random forest: zv/nzv -> median imputation

# Identify and remove near zero variance predictors
remove <- nearZeroVar(x, names = TRUE, freqCut = 2, uniqueCut = 20)
x_small <- x[ , setdiff(names(x), remove)]
```r

### 6. Model Comparision

```r
# Create custom indices
myFolds <- createFolds(churn_y, k = 5)

# Create reusable trainControl object
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # necessary
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

# Fit glmnet model
model_glmnet <- train(
  x = churn_x, y = churn_y,
  metric = 'ROC',
  method = 'glmnet',
  trControl = myControl
)

# Fit random forest
model_rf <- train(
  x = churn_x, y = churn_y,
  metric = 'ROC',
  method = 'ranger',
  trControl = myControl
)

# Compare models
models <- list(lm = model_glmnet, rf = model_rf)
models_comp <- resamples(models)
summary( models_comp )
bwplot( models_comp , metric = 'ROC')
xyplot( models_comp , metric = 'ROC')
7. Ensembling
1
2
3
4
# Create ensemble model
library(caretEnsemble)
stack <- caretStack(caretList(...), method = 'glm')
summary(stack)