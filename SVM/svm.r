#importing the library

library(e1071)

#importing the dataset

file<-read.csv("SVM_Classifier.csv")

#selecting the columns of concern

file<-file[-1]
x<-file[[1]]
y<-file[[2]]

#plotting the data points

train = data.frame(x,y)
plot(train,pch = 50)

#conducting AB test

model <- lm(y ~ x, train)
abline(model)

#fitting SVM classifier

model_svm <- svm(y ~ x , train)
pred <- predict(model_svm, train)
points(train$x, pred, col = "blue", pch=4)

#calculating error

error <- model$residuals
lm_error <- sqrt(mean(error^2)) # 3.832974

error_2 <- train$y - pred
svm_error <- sqrt(mean(error_2^2))

#tuning the SVM model as per the error calculation

svm_tune <- tune(svm, y ~ x, data = train,
                 ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9)))
print(svm_tune)

#best SVM model
best_mod <- svm_tune$best.model
best_mod_pred <- predict(best_mod, train) 

#calculating error

error_best_mod <- train$y - best_mod_pred 
best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 

#plotting and visualising the results

plot(svm_tune)
plot(train,pch=16)
points(train$x, best_mod_pred, col = "blue", pch=4)
