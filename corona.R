
setwd("C:/Users/diego/OneDrive/Área de Trabalho/corona_data")

# Loading libraries --------------------------------

library(openxlsx)
library(fastDummies)
library(tidyverse)
library(Rtsne)
library(Hmisc)
library(factoextra)
library(caret)
library(permute)
library(xgboost)
library(h2o)
library(stringi)
library(FactoMineR)
library(e1071)
library(plotROC)
library(ROCR)
library(ggrepel)
library(hrbrthemes)
library(GGally)
library(corrplot)

# Importing data --------------------------------------

corona <- read.xlsx("dataset.xlsx")

# Manipulating data ------------------------------------------

# - Separating numerical data from non numerical data

non_numerical_corona <- corona[, !(names(corona) %in% names(corona %>% select_if(is.numeric)))]

numerical_corona <- cbind(select(corona, Patient.ID), corona[, (names(corona) %in% names(corona %>% select_if(is.numeric)))])


# - Replacing NA by whitout exam

for (i in c(3:37)){
  
  non_numerical_corona[,i] <- ifelse(is.na(non_numerical_corona[,i]), "no exam", non_numerical_corona[,i])
  
}

# - Tramsforming categorical variables into dummies

non_numerical_corona <- dummy_cols(non_numerical_corona, select_columns = colnames(non_numerical_corona)[3:37], remove_selected_columns = TRUE, remove_first_dummy = TRUE) 



# - Cutting numerical variables 

for (i in c(6:30, 34:40, 42:44, 46, 51, 52,54:72, 74, 75)){
  
  quantiles <- quantile(numerical_corona[,i], prob = seq(0, 1, length = 5), type = 5, na.rm = TRUE)
  numerical_corona[,i] <- cut(numerical_corona[,i], breaks = as.numeric(quantiles))
  numerical_corona[,i] <- as.character(numerical_corona[,i])
  
  
  
}

# - Replacing NA by whitout exam

for (i in c(2:75)){
  
  numerical_corona[,i] <- ifelse(is.na(numerical_corona[,i]), "no exam", numerical_corona[,i])
  
}


# - Tranforming factor (ex numerical variables) in dummies 

numerical_corona <- dummy_cols(numerical_corona, select_columns = colnames(numerical_corona)[6:75], remove_selected_columns = TRUE, remove_first_dummy = TRUE) 



# - Bindign the data

corona_final <- left_join(non_numerical_corona, numerical_corona)

# - Removing Intermediate data

rm(corona, numerical_corona, non_numerical_corona)

# Dimensionality reduction --------------------------------




# PCA

pca_output <- prcomp(corona_final[,-c(1,2)], center=TRUE, scale=FALSE)

# - Visualizaing the PC's 

fviz_eig(pca_output)

# - Creating a dataset with the 10 first PC's

pca_data <- data.frame(pca_output$x[,c(1:10)], corona_final %>% select(exam = `SARS-Cov-2.exam.result`, Patient.ID))

# - Removing pca model

rm(pca_output)



# TSNE

# - Selecting the number of iter

# - Selecting perplexity

set.seed(666)

tsne <- Rtsne(corona_final[,-c(1,2)], check_duplicates = FALSE, max_iter = 1000)

plot(tsne$itercosts, type = "l",  ylab="Total KL-Divergence Cost", xlab="Gradient Descent (Each 50 steps)")

# - Perplexity between 2 and 100

par(mfrow=c(3,3))

perplexity <- c(2, 5, 10, 30, 50, 70, 80, 90, 100)

for (i in perplexity){
  
  set.seed(666)
  
  tsne <- Rtsne(corona_final[,-c(1,2)], check_duplicates = FALSE, max_iter = 500, perplexity = i)
  
  tsne_data <- data.frame(tsne$Y,
                          corona_final %>%
                            select(exam = `SARS-Cov-2.exam.result`)) %>%
    mutate(color = ifelse(exam == "positive","Red", "Blue"))
  
  plot(tsne_data$X1, tsne_data$X2, col = tsne_data$color, xlab = "X1", ylab = "X2", 
       main = paste("Perplexity:", i))
  
}

# - Perplexity between 5 and 400

par(mfrow=c(3,3))

perplexity <- c( 150, 200, 250, 300, 350, 400, 450, 500)

for (i in perplexity){
  
  set.seed(666)
  
  tsne <- Rtsne(corona_final[,-c(1,2)], check_duplicates = FALSE, max_iter = 500, perplexity = i)
  
  tsne_data <- data.frame(tsne$Y,
                          corona_final %>%
                            select(exam = `SARS-Cov-2.exam.result`)) %>%
    mutate(color = ifelse(exam == "positive","Red", "Blue"))
  
  plot(tsne_data$X1, tsne_data$X2, col = tsne_data$color, xlab = "X1", ylab = "X2", 
       main = paste("Perplexity:", i))
  
}



# Creating datasets using different perplexities and dimensions


dim_red_data <- list()

for (i in c(100, 150, 200, 250)){
  
  for (j in c(2,3)){
    
    set.seed(666)
    
    tsne <- Rtsne(corona_final[,-c(1,2)], check_duplicates = FALSE, max_iter = 500, perplexity = i, dims = j)
    
    
    tsne_result <- data.frame(tsne$Y,
                                    corona_final %>%
                                      select(exam = `SARS-Cov-2.exam.result`,
                                             Patient.ID))
    
    dim_red_data <- append(dim_red_data, list(tsne_result))
    
    
  }
  
  
}





# GLRM

# - Initiating H2O

h2o.init()

# - Selecting data to dimensionality reduction

glrm_adjusted <- corona_final[,-c(1,2)]

# - Changing colnames to avoid problems in the algorithm

colnames(glrm_adjusted) <- stringi::stri_rand_strings(442,4)

# - Transfering data to h2o

corona_final.hex <- as.h2o(glrm_adjusted, "corona_final.hex")

# - Applyng GLRM model



model_glrm <- h2o.glrm(training_frame = corona_final.hex,
                       cols = 1:ncol(glrm_adjusted), k = 10,
                       max_iterations = 1000, seed = 666)



# - Creating data with glrm results


glrm_data <- cbind(as.data.frame(h2o.getFrame(model_glrm@model$representation_name)), 
                   corona_final %>% select(exam = `SARS-Cov-2.exam.result`, Patient.ID))

# - Bringing glrm data and pca data to tsne datas

dim_red_data <- append(dim_red_data, list(pca_data))
dim_red_data <- append(dim_red_data, list(glrm_data))

# Removing dataframes that we will not use

rm(corona_final.hex, glrm_adjusted, glrm_data, model_glrm, pca_data, tsne, tsne_result)


# Testing models in different data sets -----------------------------


# ADABOOST

max_auc <- c()
mean_auc <- c()

for (i in 1:10){

# Create training and test data

data <- data.frame(dim_red_data[i])

set.seed(666)
trainRowNumbers <- createDataPartition(shuffle(data$exam), p=0.80, list=FALSE)

# Create the training  dataset

train <- data[trainRowNumbers,]

# Create the test dataset

test <- data[-trainRowNumbers,] 

set.seed(666)

model <- train(
  exam ~ ., train %>% select(-Patient.ID),
  method = "ada",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  metric = 'ROC')



max_auc <- c(max_auc, max(model$results$ROC))
mean_auc <- c(mean_auc, mean(model$results$ROC))

}

adaboost_results <- data.frame(alg = rep("adaboost", 10),
           model = c("tsne_100_2", "tsne_100_3", "tsne_150_2", "tsne_150_3", "tsne_200_2", "tsne_200_3",
                     "tsne_250_2", "tsne_250_3", "pca", "glrm"),
           max_auc = max_auc,
           mean_auc = mean_auc)



# GLM

max_auc <- c()
mean_auc <- c()

for (i in 1:10){
  
  # Create training and test data
  
  data <- data.frame(dim_red_data[i])
  
  set.seed(666)
  trainRowNumbers <- createDataPartition(shuffle(data$exam), p=0.80, list=FALSE)
  
  # Create the training  dataset
  
  train <- data[trainRowNumbers,]
  
  # Create the test dataset
  
  test <- data[-trainRowNumbers,] 
  
  set.seed(666)
  
  model <- train(
    exam ~ ., train %>% select(-Patient.ID),
    method = "glmnet",
    preProcess = c("scale", "center"),
    trControl = trainControl(
      method = "cv",
      search = "grid",
      number = 5,
      verboseIter = TRUE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      sampling = "smote"
    ),
    metric = 'ROC')
  
  
  
  max_auc <- c(max_auc, max(model$results$ROC))
  mean_auc <- c(mean_auc, mean(model$results$ROC))
  
}

glm_results <- data.frame(alg = rep("glm", 10),
                              model = c("tsne_100_2", "tsne_100_3", "tsne_150_2", "tsne_150_3", "tsne_200_2", "tsne_200_3",
                                        "tsne_250_2", "tsne_250_3", "pca", "glrm"),
                              max_auc = max_auc,
                              mean_auc = mean_auc)



# Discriminant Analysis

max_auc <- c()
mean_auc <- c()

for (i in 1:10){
  
  # Create training and test data
  
  data <- data.frame(dim_red_data[i])
  
  set.seed(666)
  trainRowNumbers <- createDataPartition(shuffle(data$exam), p=0.80, list=FALSE)
  
  # Create the training  dataset
  
  train <- data[trainRowNumbers,]
  
  # Create the test dataset
  
  test <- data[-trainRowNumbers,] 
  
  set.seed(666)
  
  model <- train(
    exam ~ ., train %>% select(-Patient.ID),
    method = "rda",
    preProcess = c("scale", "center"),
    trControl = trainControl(
      method = "cv",
      search = "grid",
      number = 5,
      verboseIter = TRUE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      sampling = "smote"
    ),
    metric = 'ROC')
  
  
  
  max_auc <- c(max_auc, max(model$results$ROC))
  mean_auc <- c(mean_auc, mean(model$results$ROC))
  
}

rda_results <- data.frame(alg = rep("rda", 10),
                          model = c("tsne_100_2", "tsne_100_3", "tsne_150_2", "tsne_150_3", "tsne_200_2", "tsne_200_3",
                                    "tsne_250_2", "tsne_250_3", "pca", "glrm"),
                          max_auc = max_auc,
                          mean_auc = mean_auc)




# KNN

max_auc <- c()
mean_auc <- c()

for (i in 1:10){
  
  # Create training and test data
  
  data <- data.frame(dim_red_data[i])
  
  set.seed(666)
  trainRowNumbers <- createDataPartition(shuffle(data$exam), p=0.80, list=FALSE)
  
  # Create the training  dataset
  
  train <- data[trainRowNumbers,]
  
  # Create the test dataset
  
  test <- data[-trainRowNumbers,] 
  
  set.seed(666)
  
  model <- train(
    exam ~ ., train %>% select(-Patient.ID),
    method = "knn",
    preProcess = c("scale", "center"),
    trControl = trainControl(
      method = "cv",
      search = "grid",
      number = 5,
      verboseIter = TRUE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      sampling = "smote"
    ),
    metric = 'ROC')
  
  
  
  max_auc <- c(max_auc, max(model$results$ROC))
  mean_auc <- c(mean_auc, mean(model$results$ROC))
  
}

knn_results <- data.frame(alg = rep("knn", 10),
                               model = c("tsne_100_2", "tsne_100_3", "tsne_150_2", "tsne_150_3", "tsne_200_2", "tsne_200_3",
                                         "tsne_250_2", "tsne_250_3", "pca", "glrm"),
                               max_auc = max_auc,
                               mean_auc = mean_auc)





# SVM

max_auc <- c()
mean_auc <- c()

for (i in 1:10){
  
  # Create training and test data
  
  data <- data.frame(dim_red_data[i])
  
  set.seed(666)
  trainRowNumbers <- createDataPartition(shuffle(data$exam), p=0.80, list=FALSE)
  
  # Create the training  dataset
  
  train <- data[trainRowNumbers,]
  
  # Create the test dataset
  
  test <- data[-trainRowNumbers,] 
  
  set.seed(666)
  
  model <- train(
    exam ~ ., train %>% select(-Patient.ID),
    method = "svmRadial",
    preProcess = c("scale", "center"),
    trControl = trainControl(
      method = "cv",
      search = "grid",
      number = 5,
      verboseIter = TRUE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      sampling = "smote"
    ),
    metric = 'ROC')
  
  
  
  max_auc <- c(max_auc, max(model$results$ROC))
  mean_auc <- c(mean_auc, mean(model$results$ROC))
  
}

svm_results <- data.frame(alg = rep("svm", 10),
                          model = c("tsne_100_2", "tsne_100_3", "tsne_150_2", "tsne_150_3", "tsne_200_2", "tsne_200_3",
                                    "tsne_250_2", "tsne_250_3", "pca", "glrm"),
                          max_auc = max_auc,
                          mean_auc = mean_auc)



glm_results %>%
  bind_rows(svm_results) %>%
  bind_rows(rda_results) %>%
  bind_rows(adaboost_results) %>%
  bind_rows(knn_results) %>%
  select(-mean_auc) %>%
  spread(key = alg, value = max_auc) %>%
  ggparcoord(columns = 2:6, groupColumn = 1, order = "anyClass",
             showPoints = TRUE, 
             title = "Scaled AUC by model",
             alphaLines = 1
  ) + 
  scale_color_manual(values=c( "#69b3a2", "#2E9AFE",rep("#E8E8E8", 8)) ) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=10)
  ) +
  xlab("algorithm") + ylab("scaled auc")




# Comparing overfit over multiple models -------------------------------

models <- c('adaboost', 'AdaBoost.M1', 'vglmAdjCat',
            'AdaBag', 'treebag', 'bagFDA','bartMachine',
            'bayesglm',  'gamboost', 'glmboost',
            'LogitBoost', 'blackboost', 'J48', 'C5.0', 'rpart',
            'rpart1SE',  'cforest', 
            'dwdPoly',  'dwdRadial', 'randomGLM')

data <- data.frame(dim_red_data[10])[,-12]

set.seed(666)
trainRowNumbers <- createDataPartition(shuffle(data$exam), p=0.80, list=FALSE)

# Create the training  dataset

train <- data[trainRowNumbers,]

# Create the test dataset

test <- data[-trainRowNumbers,] 

train_fix <- train

overfit_data <- data.frame(model = "x",
                           train = 1,
                           test = 1)

for (model_name in models){
  
  set.seed(666)
  
  model <- train(
    exam ~ ., train_fix ,
    method = model_name,
    preProcess = c("scale", "center"),
    trControl = trainControl(
      method = "cv",
      search = "grid",
      number = 5,
      verboseIter = TRUE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary,
      sampling = "smote"
    ),
    metric = 'ROC')
  
  test$prob <- caret::predict.train(model, test, type = "prob")[,2]
  train$prob <- caret::predict.train(model, train, type = "prob")[,2]
  
  
  pred <- prediction(test$prob, test$exam)
  perf <- performance(pred,"auc")
  test_perf<-  perf@y.values[[1]]
  
  pred <- prediction(train$prob, train$exam)
  perf <- performance(pred,"auc")
  train_perf<- perf@y.values[[1]]
  
  overfit_data <- rbind(overfit_data, c(model_name, train_perf, test_perf))
  
}

overfit_data <- overfit_data[-1,]
overfit_data$model <- models

overfit_data <- overfit_data %>%
  mutate(train = as.numeric(train),
         test = as.numeric(test),
         dif = train - test)






# Applyng models and tuning hyperparameters on glrm data ------------------------------------------

# Create training and test data

data <- data.frame(dim_red_data[10])[,-12]

set.seed(666)
trainRowNumbers <- createDataPartition(shuffle(data$exam), p=0.80, list=FALSE)

# Create the training  dataset

train <- data[trainRowNumbers,]

# Create the test dataset

test <- data[-trainRowNumbers,] 

# bagFDA



set.seed(666)

bagFDA_model <- train(
  exam ~ ., train ,
  method = "bagFDA",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  metric = 'ROC')

# Adabag



set.seed(666)

adabag_model <- train(
  exam ~ ., train ,
  method = "AdaBag",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  metric = 'ROC')

# C50



set.seed(666)

C5_model <- train(
  exam ~ ., train ,
  method = "C5.0",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  metric = 'ROC')




# GAMBOOST



set.seed(666)

gamboost_model <- train(
  exam ~ ., train ,
  method = "gamboost",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  metric = 'ROC')

# RDA

tuneGrid <- expand.grid(
  lambda = seq(0, 1, 0.1),
  gamma = seq(0, 1, 0.1))


set.seed(666)

rda_model <- train(
  exam ~ ., train ,
  method = "rda",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  tuneGrid = tuneGrid,
  metric = 'ROC')

# Logit boost estimation


set.seed(666)

logit_model <- train(
  exam ~ ., train ,
  method = "LogitBoost",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  metric = 'ROC')

# Ada estimation


set.seed(666)

ada_model <- train(
  exam ~ ., train ,
  method = "ada",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  metric = 'ROC')

# Blackboost estimation

set.seed(666)

blackboost_model <- train(
  exam ~ ., train ,
  method = "blackboost",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  metric = 'ROC')


# GLM

tuneGrid <- expand.grid(
  lambda = seq(0, 1, 0.01),
  alpha = seq(0, 1, 0.1))


set.seed(666)

glm_model <- train(
  exam ~ ., train ,
  method = "glmnet",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  tuneGrid = tuneGrid,
  metric = 'ROC')

# KNN

tuneGrid <- expand.grid(
  k = c(1:70))


set.seed(666)

knn_model <- train(
  exam ~ ., train ,
  method = "knn",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  tuneGrid = tuneGrid,
  metric = 'ROC')

# svm

set.seed(666)

svm_model <- train(
  exam ~ ., train,
  method = "svmRadial",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  metric = 'ROC')




# Bringing the probabilities

# - Test

test$prob_logit <- caret::predict.train(logit_model, test, type = "prob")[,2]
test$prob_knn <- caret::predict.train(knn_model, test, type = "prob")[,2]
test$prob_glm <- caret::predict.train(glm_model, test, type = "prob")[,2]
test$prob_rda <- caret::predict.train(rda_model, test, type = "prob")[,2]
test$prob_ada <- caret::predict.train(ada_model, test, type = "prob")[,2]
test$prob_blackboost <- caret::predict.train(blackboost_model, test, type = "prob")[,2]
test$prob_bagfda <- caret::predict.train(bagFDA_model, test, type = "prob")[,2]
test$prob_gamboost <- caret::predict.train(gamboost_model, test, type = "prob")[,2]
test$prob_svm <- caret::predict.train(svm_model, test, type = "prob")[,2]
test$prob_c50 <- caret::predict.train(C5_model, test, type = "prob")[,2]
test$prob_adabag <- caret::predict.train(adabag_model, test, type = "prob")[,2]


# - Train

train$prob_logit <- caret::predict.train(logit_model, train, type = "prob")[,2]
train$prob_knn <- caret::predict.train(knn_model, train, type = "prob")[,2]
train$prob_glm <- caret::predict.train(glm_model, train, type = "prob")[,2]
train$prob_rda <- caret::predict.train(rda_model, train, type = "prob")[,2]
train$prob_ada <- caret::predict.train(ada_model, train, type = "prob")[,2]
train$prob_blackboost <- caret::predict.train(blackboost_model, train, type = "prob")[,2]
train$prob_bagfda <- caret::predict.train(bagFDA_model, train, type = "prob")[,2]
train$prob_gamboost <- caret::predict.train(gamboost_model, train, type = "prob")[,2]
train$prob_svm <- caret::predict.train(svm_model, train, type = "prob")[,2]
train$prob_c50 <- caret::predict.train(C5_model, train, type = "prob")[,2]
train$prob_adabag <- caret::predict.train(adabag_model, train, type = "prob")[,2]


# - Does the models overfitted?


train_perf <- c()
test_perf <- c()

for (i in c(12:22)){
  
  
  pred <- prediction(test[,i], test$exam)
  perf <- performance(pred,"auc")
  test_perf<- c(test_perf, perf@y.values[[1]])
  
  pred <- prediction(train[,i], train$exam)
  perf <- performance(pred,"auc")
  train_perf<- c(train_perf, perf@y.values[[1]])
  
  
  
}

overfit <- data.frame(model = colnames(test[,c(12:22)]),
                      train = train_perf,
                      test = test_perf)


overfit <- overfit %>%
  mutate(dif = test - train)

# Ensemble (And final) model --------------------------------

# - Creating new train and test data

ensemble_train <- train[,c(11, 12:19, 21, 22)]
ensemble_test <- test[,c(11, 12:19, 21, 22)]

# - Does the data is correlated? Probably yes

cor_matrix <- cor(ensemble_train[,-1])
corrplot(cor_matrix, method = "shade", type = "lower", tl.col = '#424242', tl.srt = 45,
         addCoef.col = "black", tl.cex = 0.7, number.cex = 0.7)

# - Implementing KNN

tuneGrid <- expand.grid(
  k = c(1:70))

set.seed(666)

ensemble_model <- train(
  exam ~ ., ensemble_train ,
  method = "knn",
  preProcess = c("scale", "center"),
  trControl = trainControl(
    method = "cv",
    search = "grid",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    sampling = "smote"
  ),
  tuneGrid = tuneGrid,
  metric = 'ROC')

# - Hyperparamenter results plot

plot(ensemble_model)

# - Predicting on train and test data

ensemble_test$prediction <- caret::predict.train(ensemble_model, test, type = "prob")[,2]
ensemble_train$prediction <- caret::predict.train(ensemble_model, train, type = "prob")[,2]

# - How the model separated the data?

ggplot(ensemble_test, aes(x = prediction, fill = exam)) + geom_density(alpha = 0.3) + theme_light()


# - ROC Curve

library(plotROC)
rocplot <- ggplot(ensemble_test, aes(m = prediction, d = exam))+ geom_roc(n.cuts=20,labels=FALSE)
rocplot + style_roc(theme = theme_grey) + geom_rocci(fill="pink") + theme_classic()

# - ROC Value


pred <- prediction(ensemble_test$prediction, ensemble_test$exam)
perf <- performance(pred,"auc")

perf@y.values


# - Tradeoff between specificity and sensitivity

specificity <- c()
sensitivity <- c()
accuracy <- c()

for (i in seq(0.1,0.9,0.01)){
  
  test <- ensemble_test %>%
    mutate(class = ifelse(prediction > i, "positive", "negative"))
  
  conf <- table(test$exam, test$class)
  
  conf
  
  conf[1,1]/sum(conf[1,])
  
  specificity <- c(specificity, conf[2,2]/sum(conf[2,]))
  sensitivity <- c(sensitivity, conf[1,1]/sum(conf[1,]))
  accuracy <- c(accuracy, (conf[1,1] + conf[2,2])/sum(conf))
}

data <- data.frame(prob = seq(0.1,0.9,0.01),
                   sensitivity = sensitivity,
                   specificity = specificity,
                   accuracy = accuracy)



data <- data %>%
  gather(key = "metric", value = "value", -prob)

ggplot(data, aes(x = prob, y = value, color = metric)) + geom_line(size = 0.8) +
  theme_bw(base_size = 14) + scale_x_continuous(breaks = seq(0,1,0.05)) +
  labs(x = "probability", y = "value")

