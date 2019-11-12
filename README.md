---
title: "Project"
author: "MaryM"
date: "11/11/2019"


# Loading Required Packages

```{r}

library(ggplot2)

library(lattice)

library(caret)

library(corrplot)

library(knitr)
```

# Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

For more information please refer to:  http://groupware.les.inf.puc-rio.br/har#ixzz3xsbS5bVX


# Project Goal

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.


# Reading and Cleaning the Data

## Reading the Data

```{r}

# Load the training and testing data

TrainingData <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
TestingData  <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))

# Partitioning the training dataset

inTrain  <- createDataPartition(TrainingData$classe, p=0.7, list=FALSE)

training <- TrainingData[inTrain, ] ; testing <- TrainingData[-inTrain, ]


```

```{r}

dim(training)

```

```{r}

dim(testing)
```

## Checking for NAs

Now, we should check and remove the NA values for cleaning the data.

```{r}

NAs  <- sapply(training, function(x) mean(is.na(x))) > 0.95
training <- training[, NAs==FALSE]
testing  <- testing[, NAs==FALSE]

```

```{r}

dim(training)

```

```{r}

dim(testing)

```

In addition, the near zero variance variables will be removed from the data.

```{r}

NZV <- nearZeroVar(training)
training <- training[, -NZV]
testing  <- testing[, -NZV]
```

```{r}

dim(training)

```

```{r}

dim(testing)

```

Now, we can check the dat ahead to make sure that all the variables are needed to the final analysis.

```{r}

head(training)

```

As seen, the first five colums are introductory and are not required for the analysis; therefore, those columns can be removed from the data:

```{r}

training <- training[, -(1:5)]
testing  <- testing[, -(1:5)]

```

```{r}

dim(training)

```

```{r}

dim(testing)

```

Eventually, after cleaning the data, 54 variables are left.

# Performing Correlation Analysis

Before conducting the machine learning model, we have to test for the correlation between the variables:

```{r}

CorVar <- cor(training[, -54])  # the dependent variable should be removed from the correlation test
corrplot(CorVar,   type = "lower", method = 'color', #type only keeps a part of the data
         tl.cex = 0.6, tl.col = rgb(0, 0, 0))

#method = "number" : gives numbers

```

As depicted in the figure, there are a few variables that are highly correlated. But, if there were more correlated variables, a PCA could be conducted to be able to develop the model.


# Developing the Prediction Model

## Random Forest

Random forest will be used to develop the prediction model.

```{r}

set.seed(1111)

cntrl <- trainControl(method="cv", 5)

RFModel <- train(classe ~ ., data=training, method="rf", trControl=cntrl)

RFModel$finalModel

```

```{r}

predictRF <- predict(RFModel, newdata = testing)
confMatRF<- confusionMatrix(predictRF, testing$classe)
confMatRF

```

To calculate the model accuracy and overall out of sample error, the following code will be used.

```{r}

accuracy <- postResample(predictRF, testing$classe)
acc.out <- accuracy[1]

overall.ose <-
        1 - as.numeric(confusionMatrix(testing$classe, predictRF)
                       $overall[1])

acc.out

```

```{r}

acc.out

```

```{r}

overall.ose

```

Therefore, the moel accuracy is 0.9972812, and the overall out of sample error is 0.002718777.

# Data Validation

For validation process, the test dataset will be used.

```{r}

predictTEST <- predict(RFModel, newdata=TestingData)
predictTEST
