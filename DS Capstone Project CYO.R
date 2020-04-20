

#Downloading libraries#

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr",repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl",repos = "http://cran.us.r-project.org")


#Loading libraries#

library(caret)
library(tidyverse)
library(dplyr)
library(RCurl)
```



#Downloading data#


x <- getURL("https://raw.githubusercontent.com/mhmdamrhamza/dscapstoneharvardxcyo/master/framingham.csv")
framingham <- read.csv(text = x)
```


#Cleansing the Data#


#removing education and smoking column#

fullData <- framingham[,-c(3,4)]

#adding sex column#

fullData$sex <- ifelse(fullData$male == 1, "male", "female")

#removing rows with NA values in any of the columns#

fullData <- subset(fullData, complete.cases(fullData))


#defining result parameter#

fullData <- fullData %>% 
  mutate(TenYearCHD = as.character(TenYearCHD)) %>% 
  mutate(TenYearCHD = replace(TenYearCHD, TenYearCHD == '0', 'No')) %>%
  mutate(TenYearCHD = replace(TenYearCHD, TenYearCHD == '1', 'Yes')) %>%
  mutate(TenYearCHD = as.factor(TenYearCHD))
```


#generating test and train sets#

set.seed(1)
trainIndex <- createDataPartition(fullData$TenYearCHD, p=.7, list=FALSE)
train <- fullData[trainIndex,]
test <- fullData[-trainIndex,]


#summary of dataet#

fullData %>%
  count(TenYearCHD)

#Exploration of each potential risk factor#


#Age#


train %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 20) + 
  theme_minimal() +
  facet_grid(~ TenYearCHD)

#Sex#


train %>% 
  ggplot(aes(x = sex)) + 
  geom_bar() + 
  theme_minimal() +
  facet_grid(~ TenYearCHD)



#Smoking#


  ```{r eploration-smoking}
train %>% 
  ggplot(aes(x = cigsPerDay)) + 
  geom_histogram(binwidth = 5, color = "white") + 
  theme_minimal() +
  facet_grid(~ TenYearCHD)
```

#Diabetes#



train %>% 
  ggplot(aes(x = diabetes)) + 
  geom_histogram(binwidth = 0.5) + 
  theme_minimal() +
  facet_grid(~ TenYearCHD)



#Cholestrol#


train %>% 
  ggplot(aes(x = totChol)) + 
  geom_histogram(binwidth = 100, color = "white") + 
  theme_minimal() +
  facet_grid(~ TenYearCHD)

#SysBP#

train %>% 
  ggplot(aes(x = sysBP)) + 
  geom_histogram(binwidth = 10, color = "white") + 
  theme_minimal() +
  facet_grid(~ TenYearCHD)

#DiaBP# 

train %>% 
  ggplot(aes(x = diaBP)) + 
  geom_histogram(binwidth = 5, color = "white") + 
  theme_minimal() +
  facet_grid(~ TenYearCHD)


#BMI#


train %>% 
  ggplot(aes(x = BMI)) + 
  geom_histogram(binwidth = 5, color = "white") +
  scale_x_discrete(limits = c(seq(20,40,5))) +
  theme_minimal() +
  facet_grid(~ TenYearCHD)

#Heartrate#

train %>% 
  ggplot(aes(x = heartRate)) + 
  geom_histogram(binwidth = 5, color = "white") + 
  theme_minimal() +
  facet_grid(~ TenYearCHD)


#Glucose#

train %>% 
  ggplot(aes(x = glucose)) + 
  geom_histogram(binwidth = 5, color = "white")  +
  theme_minimal() +
  facet_grid(~ TenYearCHD)


#finding Correleation#


options(digits = 5)
cor(subset(train, select = -c(sex, TenYearCHD,male,age)))



#examining the correlated parameters#

train %>% 
  ggplot(aes(x = sysBP, y = diaBP, color = prevalentHyp, shape = TenYearCHD,  )) + 
  geom_point() + 
  theme_minimal() +
  facet_grid(~ TenYearCHD)


#removing columns not required#

train <- train %>% subset(select = -c(sysBP))
test <- test %>% subset(select = -c(sysBP))
```

#setting up results data frame#

  
  ```{r setup-storage}
results <- data.frame(Model = character(), 
                      Accuracy = double(), 
                      Sensitivity = double(), 
                      Specificity = double(), 
                      stringsAsFactors = FALSE)
```


#Naive Bayes#



```{r nb-train, info=FALSE, warning=FALSE}
nb_model = train(TenYearCHD ~ ., data = train, method = "nb")
predictions = predict(nb_model, newdata = test)
confusionMatrix <- confusionMatrix(predictions, test$TenYearCHD)
results[nrow(results) + 1, ] <- c(as.character('Naive Bayes (nb)'), 
                                  confusionMatrix$overall['Accuracy'],  
                                  confusionMatrix$byClass['Sensitivity'], 
                                  confusionMatrix$byClass['Specificity'])
rm(nb_model, predictions)
confusionMatrix

#Linear Classifier#


```{r linc-train, info=FALSE, warning=FALSE}
lc_model = train(TenYearCHD ~ ., data = train, method = "glmboost")
predictions = predict(lc_model, newdata = test)
confusionMatrix <- confusionMatrix(predictions, test$TenYearCHD)
results[nrow(results) + 1, ] <- c(as.character('Linear Classifier (glmboost)'), 
                                  confusionMatrix$overall['Accuracy'],  
                                  confusionMatrix$byClass['Sensitivity'], 
                                  confusionMatrix$byClass['Specificity'])
rm(lc_model, predictions)
confusionMatrix



#Logistic Regression#



  
lr_model = train(TenYearCHD ~ ., data = train, method = "bayesglm")
predictions = predict(lr_model, newdata = test)
confusionMatrix <- confusionMatrix(predictions, test$TenYearCHD, prevalence = 0.06)
results[nrow(results) + 1, ] <- c(as.character('Logistic Regression (bayesglm)'), 
                                  confusionMatrix$overall['Accuracy'],  
                                  confusionMatrix$byClass['Sensitivity'], 
                                  confusionMatrix$byClass['Specificity'])
rm(lr_model, predictions)
confusionMatrix


#K-Nearest Neighbours#


```{r knn-train, info=FALSE, warning=FALSE}
knn_model = train(TenYearCHD ~ ., data = train, method = "knn", preProcess=c('knnImpute'))
knn_model
```

```{r knn-predict}
predictions = predict(knn_model, newdata = test)
confusionMatrix <- confusionMatrix(predictions, test$TenYearCHD, prevalence = 0.06)
results[nrow(results) + 1, ] <- c(as.character('K-nearest neighbours (knn)'), 
                                  confusionMatrix$overall['Accuracy'],  
                                  confusionMatrix$byClass['Sensitivity'], 
                                  confusionMatrix$byClass['Specificity'])
rm(knn_model, predictions)
confusionMatrix
```

#Random Forest#



```{r rf-train, info=FALSE, warning=FALSE}
rf_model = train(TenYearCHD ~ ., data = train, method = "rf")
predictions = predict(rf_model, newdata = test)
confusionMatrix <- confusionMatrix(predictions, test$TenYearCHD, prevalence = 0.06)
results[nrow(results) + 1, ] <- c(as.character('Random Forest (rf)'), 
                                  confusionMatrix$overall['Accuracy'],  
                                  confusionMatrix$byClass['Sensitivity'], 
                                  confusionMatrix$byClass['Specificity'])
rm(rf_model, predictions)
confusionMatrix


rm(confusionMatrix)


#Results#


options(digits = 5)
results %>% arrange(Accuracy)




