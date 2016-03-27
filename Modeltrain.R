# .73025 Kaggle score
library(Matrix)
library(xgboost)

# imputation of age
full_imp <- rbindlist(list(dt.train[, -1, with = F], dt.test))

# impute missing values with linear regresion
imput_age <- lm(age~.-(year+day), data = full_imp)
summary(imput_age)
imp_age <- predict(imput_age, full_imp[which(is.na(age), arr.ind = T), ])
full_imp[is.na(full_imp[, age]), age := .(imp_age)]

# create matrices
to_train <- full_imp[1:nrow(dt.train), ]
to_test <- full_imp[-c(1:nrow(dt.train)), ]
X <- sparse.model.matrix(OutcomeType~.-1, 
                         data = to_train[, OutcomeType := dt.train[, OutcomeType]])
Y <- as.numeric(dt.train[, OutcomeType]) - 1
numclass <- range(Y)[2] + 1

# set the parameter
params <- list("objective" = "multi:softprob",
               "eta" = .4,
               "max_depth" = 8,
               "eval_metric" = "mlogloss",
               "num_class" = numclass,
               "subsample" = .8)

# cross-validation
nround = 50
set.seed(123)
bst.cv <-  xgb.cv(params = params, data = X, label = Y, nfold = 10, nround = nround, verbose = T)

# cv error plot
cv_error <- bst.cv$test.mlogloss.mean
min <- which.min(cv_error)
print(paste(min, cv_error[min]))
plot(1:nround, cv_error, type = "l")
points(min, cv_error[min], col = "red")

# train
set.seed(123)
bst <- xgboost(data = X, label = Y, params = params, nround = min)

# apply prediction
mtest <- sparse.model.matrix(~.-1, data = to_test)
pred <- predict(bst, newdata = mtest)
result <- matrix(pred, nrow = 11456, ncol = numclass, byrow = T)

result <- data.frame(sample$ID, result)
colnames(result) <- names(sample)

# write output
write.csv(result, file = "solution.csv", row.names = F)

