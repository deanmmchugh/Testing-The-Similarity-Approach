library(tidyverse)
library(broom)
library(lme4)

answers <- c(1,1,1,1,1,1)
conditions <- c('good', 'bad', 'good','bad','good','bad')
data <- data.frame(conditions, answers)
data
