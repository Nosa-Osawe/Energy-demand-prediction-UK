install.packages("keras")
install.packages("mlbench")
install.packages("magrittr")
install.packages("neuralnet")


library(tidyverse)
library(keras)
library(mlbench)
library(neuralnet)
library(magrittr)

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
ind <- sample()


