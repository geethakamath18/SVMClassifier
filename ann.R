library(caret) #Classification and REgression Training (caret) provides functions for SVM, Machine Learning Algorithms etc

read.csv2("heart_tidy.csv")

#Loading the dataset into a data frame
heart_df <- read.csv("heart_tidy.csv", sep = ',', header = FALSE)

heart_df

str(heart_df) #shows value of each variable ie index

#Data Slicing
set.seed(3033)
intrain <- createDataPartition(y = heart_df$V14, p= 0.7, list = FALSE)
training <- heart_df[intrain,]
testing <- heart_df[-intrain,]

#Checking dimensionality of each of the partitions
dim(training)
dim(testing)

#Data Processing
anyNA(heart_df)

#basic idea 
summary(heart_df)

#converting to categorical variables
training[["V14"]] = factor(training[["V14"]])

#TRAINING!
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
svm_Linear <- train(V14 ~., data = training, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)

#Trained SVM Result
svm_Linear

#PREDICTION FOR TEST CASES
test_pred <- predict(svm_Linear, newdata = testing)
test_pred

#Accuracy
confusionMatrix(test_pred, testing$V14 )

#Changing values of c
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(3233)
svm_Linear_Grid <- train(V14 ~., data = training, method = "svmLinear", trControl=trctrl,preProcess = c("center", "scale"),tuneGrid = grid, tuneLength = 10)

svm_Linear_Grid

plot(svm_Linear_Grid)

#Prediction for tthe grid of values
test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid


#Accuracy function for the grid
confusionMatrix(test_pred_grid, testing$V14 )