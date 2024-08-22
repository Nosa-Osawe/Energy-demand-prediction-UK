library(tidyverse)

SLMdata <- read.csv("C:\\Users\\DELL\\Documents\\Git in R\\Energy-demand-prediction-UK\\Data\\Energy Data\\EWH.csv")

attach(SLMdata)

SLMdata1 <-SLMdata %>% 
  filter(Acorn_grouped %in% c("Affluent", "Comfortable","Adversity"))


SLMdata1$Acorn_grouped <- factor(SLMdata1$Acorn_grouped, 
                                levels = c("Adversity", "Comfortable", "Affluent"))


slm <- lm(energy_sum~Acorn_grouped, data = SLMdata1)
summary(slm)


