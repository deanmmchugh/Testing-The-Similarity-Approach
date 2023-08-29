library(tidyverse)
library(broom)
library(lme4)

data <- read.csv('example_Data.csv')
data <- filter(data, type != 'training')
data

data$numerical_answer <- data$pred_ans
data

data$numerical_answer[data$numerical_answer == 3] <- 2
data

data$textual_answer[data$numerical_answer == 0] <- 'true'
data
data$textual_answer[data$numerical_answer == 1] <- 'indeterminate'
data
data$textual_answer[data$numerical_answer == 2] <- 'false'
data
data$indeterminate_status <- 0
data
