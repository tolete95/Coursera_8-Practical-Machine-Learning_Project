# Introduction

In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here:
http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).


# Preparing the data
First we will need to load the required libraries
```{r, cache = T}
library(caret)
library(randomForest)
```
## Loading
```{r, cache = T}
Otraining <- read.csv("~./pml-training.csv",sep = ",")
Otesting <- read.csv("~./pml-testing.csv",sep = ",")
```
## Cleaning
First we remove the first 6 columns (rownumber, user, and time stamp). We want to predict accordign to the sensor information only. 
```{r, cache = T}
Otraining <- Otraining[,6:length(Otraining)]
Otesting <- Otesting[,6:length(Otesting)]
```
Then we find colums with NA and empty values in more than 85% of the rows
```{r, cache = T}
rmindx <- which(colSums(is.na(Otraining) |Otraining=="")>0.85*nrow(Otraining))    
```
Finally we remove the selected columns for training and testing datasets
```{r, cache = T}
Otraining<-Otraining[,-rmindx]
Otesting<-Otesting[,-rmindx]
```
## Slicing
Now we will proceed with the data slicing for cross validation
```{r, cache = T}
inTrain <- createDataPartition(y=Otraining$classe, p=0.70, list=FALSE)
training <- Otraining[inTrain,]
testing <- Otraining[-inTrain,]
```

## Data Modelling
First for reproductibility reasons, we will set seed.
Then we fit two different models  RandomForest and Gradient Boosting Machine. Use train control to set crossvlidation  to 3 partitions.
Usually 10 folds are used but even 5 folds gave a high computation time so I have stayed with 3 partitions.
VerboseIter is set to TRUE to follow the  model fitting
```{r, cache = T}
set.seed(41119)
modRf <- train(classe~.,data=training,method = "rf",
              trControl=trainControl(method = "cv",number=3,verboseIter = T))
modGbm <- train(classe~.,data=training,method = "gbm",
               trControl=trainControl(method = "cv",number=3,verboseIter = T))
```               
Now we predict the results using the fittet models in the subseted testing data set. 
We can see the confusion  matrix with the results of the prediction. 
```{r, cache = T}
predRf <- predict(modRf,testing)
confusionMatrix(testing$classe, predRf)
Confusion Matrix and Statistics
          Reference
Prediction    A    B    C    D    E
         A 1674    0    0    0    0
         B    4 1132    3    0    0
         C    0    0 1026    0    0
         D    0    1    3  960    0
         E    0    0    0    1 1081
Overall Statistics
                                          
               Accuracy : 0.998           
                 95% CI : (0.9964, 0.9989)
    No Information Rate : 0.2851          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9974          
                                          
 Mcnemar's Test P-Value : NA              
Statistics by Class:
                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9976   0.9991   0.9942   0.9990   1.0000
Specificity            1.0000   0.9985   1.0000   0.9992   0.9998
Pos Pred Value         1.0000   0.9939   1.0000   0.9959   0.9991
Neg Pred Value         0.9991   0.9998   0.9988   0.9998   1.0000
Prevalence             0.2851   0.1925   0.1754   0.1633   0.1837
Detection Rate         0.2845   0.1924   0.1743   0.1631   0.1837
Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
Balanced Accuracy      0.9988   0.9988   0.9971   0.9991   0.9999
```
```{r, cache = T}
predGbm <- predict(modGbm,testing)
confusionMatrix(testing$classe, predGbm)
Confusion Matrix and Statistics
          Reference
Prediction    A    B    C    D    E
         A 1669    4    0    1    0
         B   14 1117    7    1    0
         C    0    5 1020    1    0
         D    1    5    8  949    1
         E    0    2    5    9 1066
Overall Statistics
                                          
               Accuracy : 0.9891          
                 95% CI : (0.9861, 0.9916)
    No Information Rate : 0.2862          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9862          
                                          
 Mcnemar's Test P-Value : NA              
Statistics by Class:
                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9911   0.9859   0.9808   0.9875   0.9991
Specificity            0.9988   0.9954   0.9988   0.9970   0.9967
Pos Pred Value         0.9970   0.9807   0.9942   0.9844   0.9852
Neg Pred Value         0.9964   0.9966   0.9959   0.9976   0.9998
Prevalence             0.2862   0.1925   0.1767   0.1633   0.1813
Detection Rate         0.2836   0.1898   0.1733   0.1613   0.1811
Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
Balanced Accuracy      0.9950   0.9906   0.9898   0.9922   0.9979
```
We compare the results of both models. 
as RF method is better we will used on the test set. As accuracy is high(0.998), no need to stack predictors from both models
```{r, cache = T}
print(paste("RF accuracy = ", confusionMatrix(testing$classe, predRf)$overall['Accuracy']))
print(paste("GBM accuracy = ", confusionMatrix(testing$classe, predGbm)$overall['Accuracy']))
```
# Predicting the Original Test Set
```{r, cache = T}
Quizz<- predict(modRf, Otesting)
Quizz
 [1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
```
