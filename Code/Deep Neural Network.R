install.packages("keras")
install.packages("mlbench")
install.packages("magrittr")
install.packages("neuralnet")


library(tidyverse)
library(keras)
library(mlbench)
library(neuralnet)
library(magrittr)
library(tensorflow)

data("BostonHousing")

boston <- BostonHousing
str(boston)
attach(boston)

boston <- boston %>% 
  mutate_if(is.factor, as.numeric)


#------- Neural Network Viz!

n <- neuralnet(medv~ crim+zn+ indus+ chas + nox +rm + age + dis + rad + tax + ptratio + b + lstat, 
               data = boston, hidden = c(10,5),
               linear.output = F, lifesign = "full", 
               rep = 1) # 10 neurons in the first hiddel layer and 5 in the second hidden layer

 
plot(n, col.hidden = "darkgreen",
     col.hidden.synapse = "darkgreen",
     show.weights = F,
     information = F,
     fill = "lightblue")

boston <- as.matrix(boston)
dimnames(boston) <- NULL



# data partitioning

set.seed(1234)
ind <- sample(2, nrow(boston), replace = T, prob = c(0.7, 0.3))
            # --  prepares a row number of 1 and 2, randomly!

DNN_train <-boston[ind==1, 1:13] # anywhere the row number 1 appears
DNN_test <- boston[ind==2, 1:13]

DNN_train_target <-boston[ind==1, 14]
DNN_test_target <-boston[ind==2, 14]

# calculate mean and standard deviations gotten from the training data
mean_values <- colMeans(DNN_train)  # mean of all columns
SD_values <- apply(DNN_train,2, sd)  #  2 = column; 1 =rows; 
                                      # sd is the function for standard dev.

norm_train <- scale(DNN_train, center = mean_values,
                    scale = SD_values)

norm_test <- scale(DNN_test, center = mean_values,
                   scale = SD_values)


# model creation using Keras

Dnn_model <- keras_model_sequential()




