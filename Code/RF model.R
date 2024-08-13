install.packages("party")
library(party)
library(flexplot)

set.seed(1010)

data(avengers)
View(avengers)
attach(avengers)
rf_Data <- avengers

rf_model <- cforest(ptsd~ ., data = rf_Data)

estimates(rf_model)

rf_new <- cforest(ptsd~ agility+ shots.taken + injuries+ damage.resistance, data = rf_Data)
estimates(rf_new)

