library(tidyverse)
library(broom)
library(lme4)
library(mclogit)
library(nnet)

### INdeterminate model (further investigation)
#Loading data
data <- read.csv('example_Data.csv')
data <- filter(data, type != 'training')
data

#Assume that the answers are as predicted
data$numerical_answer <- data$pred_ans
data

# Say that all the T4 cases are answered "indeterminate"
data$numerical_answer[data$numerical_answer == 3] <- 1
data
#Fix textual descriptions of the answers
data$textual_answer[data$numerical_answer == 0] <- 'true'
data
data$textual_answer[data$numerical_answer == 1] <- 'indeterminate'
data
data$textual_answer[data$numerical_answer == 2] <- 'false'
data

# Create data type to have true and false fillers together
data$type_tr <- paste(data$type, data$pred_ans, sep="_")

data$type_tr

data$type_tr <- ifelse(data$type == 'test', data$condition, data$type_tr)
data$type_tr

#Create the indeterminate dataset
int_data<-data

# Copy the data to have 50 participants
i=1
n =50
for (i in 1:n-1){
  data$participant_id <- i 
  int_data <- rbind(int_data,data)
  i <- i+1
}
unique(int_data$participant_id)

test <- int_data$numerical_answer

#Adding 10% error
error <- rbinom(length(int_data$numerical_answer),1,0.8)

err_ans <- rep(NA, length(int_data$numerical_answer))
err_ans

#Fill in the vector with values 
for (i in 1:length(int_data$numerical_answer)) {
  if (error[i] == 1) {
    int_data$numerical_answer[i] <- int_data$numerical_answer[i]
  } else {
  # new <-2
  new <- sample(c(0,1,2)[!c(0,1,2) == int_data$numerical_answer[i]], 1)
  int_data$numerical_answer[i] <- new
  }
}



int_data$numerical_answer

sum(int_data$numerical_answer != test)

#Fix textual descriptions of the answers
int_data$textual_answer[int_data$numerical_answer == 0] <- 'true'
int_data$textual_answer[int_data$numerical_answer == 1] <- 'indeterminate'
int_data$textual_answer[int_data$numerical_answer == 2] <- 'false'


model<- multinom(textual_answer~ type_tr,data=int_data)
summary(model)

(exp(coef(model))-1)*100

z <- summary(model)$coefficients/summary(model)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

sing_model<- multinom(textual_answer ~ 1 ,data=int_data)
anova(sing_model, model)




#####
#False model (against similarity)
#Loading data
data <- read.csv('example_Data.csv')
data <- filter(data, type != 'training')
data

#Assume that the answers are as predicted
data$numerical_answer <- data$pred_ans
data

# Say that all the T4 cases are answered "indeterminate"
data$numerical_answer[data$numerical_answer == 3] <- 2
data
#Fix textual descriptions of the answers
data$textual_answer[data$numerical_answer == 0] <- 'true'
data
data$textual_answer[data$numerical_answer == 1] <- 'indeterminate'
data
data$textual_answer[data$numerical_answer == 2] <- 'false'
data

# Create data type to have true and false fillers together
data$type_tr <- paste(data$type, data$pred_ans, sep="_")

data$type_tr

data$type_tr <- ifelse(data$type == 'test', data$condition, data$type_tr)
data$type_tr

#Create the indeterminate dataset
int_data<-data

# Copy the data to have 50 participants
i=1
n =50
for (i in 1:n-1){
  data$participant_id <- i 
  int_data <- rbind(int_data,data)
  i <- i+1
}
unique(int_data$participant_id)

test <- int_data$numerical_answer

#Adding 10% error
error <- rbinom(length(int_data$numerical_answer),1,0.9)

err_ans <- rep(NA, length(int_data$numerical_answer))
err_ans

#Fill in the vector with values 
for (i in 1:length(int_data$numerical_answer)) {
  if (error[i] == 1) {
    int_data$numerical_answer[i] <- int_data$numerical_answer[i]
  } else {
    #new <-2
    new <- sample(c(0,1,2)[!c(0,1,2) == int_data$numerical_answer[i]], 1)
    int_data$numerical_answer[i] <- new
  }
}



int_data$numerical_answer

sum(int_data$numerical_answer != test)

#Fix textual descriptions of the answers
int_data$textual_answer[int_data$numerical_answer == 0] <- 'true'
int_data$textual_answer[int_data$numerical_answer == 1] <- 'indeterminate'
int_data$textual_answer[int_data$numerical_answer == 2] <- 'false'

int_data

model<- multinom(cbind(numerical_answer,1)~ type_tr + (1|participant_id),data=int_data)
summary(model)

exp(coef(model))

z <- summary(model)$coefficients/summary(model)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

model<- multinom(cbind(numerical_answer,1)~ type_tr,data=int_data)
summary(model)

exp(coef(model))

z <- summary(model)$coefficients/summary(model)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

sing_model<- multinom(textual_answer ~ 1 ,data=int_data)
anova(sing_model, model)




#####
#True model (confirming similarity)
#Loading data
data <- read.csv('example_Data.csv')
data <- filter(data, type != 'training')
data

#Assume that the answers are as predicted
data$numerical_answer <- data$pred_ans
data

# Say that all the T4 cases are answered "true"
data$numerical_answer[data$numerical_answer == 3] <- 0
data
#Fix textual descriptions of the answers
data$textual_answer[data$numerical_answer == 0] <- 'true'
data
data$textual_answer[data$numerical_answer == 1] <- 'indeterminate'
data
data$textual_answer[data$numerical_answer == 2] <- 'false'
data

# Create data type to have true and false fillers together
data$type_tr <- paste(data$type, data$pred_ans, sep="_")

data$type_tr

data$type_tr <- ifelse(data$type == 'test', data$condition, data$type_tr)
data$type_tr

#Create the indeterminate dataset
int_data<-data

# Copy the data to have 50 participants
i=1
n =5
for (i in 1:n-1){
  data$participant_id <- i 
  int_data <- rbind(int_data,data)
  i <- i+1
}
unique(int_data$participant_id)

test <- int_data$numerical_answer

#Adding 10% error
error <- rbinom(length(int_data$numerical_answer),1,0.7)
error
err_ans <- rep(NA, length(int_data$numerical_answer))
err_ans

#Fill in the vector with values 
for (i in 1:length(int_data$numerical_answer)) {
  if (error[i] == 1) {
    int_data$numerical_answer[i] <- int_data$numerical_answer[i]
  } else {
    #new <- 1
    new <- sample(c(0,1,2))
    #new <- sample(c(0,1,2)[!c(0,1,2) == int_data$numerical_answer[i]], 1)
    int_data$numerical_answer[i] <- new
  }
}



int_data$numerical_answer

sum(int_data$numerical_answer != test)

#Fix textual descriptions of the answers
int_data$textual_answer[int_data$numerical_answer == 0] <- 'true'
int_data$textual_answer[int_data$numerical_answer == 1] <- 'indeterminate'
int_data$textual_answer[int_data$numerical_answer == 2] <- 'false'

model<- multinom(textual_answer ~ type_tr,data=int_data)
summary(model)

(exp(coef(model))-1)*100

z <- summary(model)$coefficients/summary(model)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


sing_model<- multinom(textual_answer ~ 1 ,data=int_data)
summary(sing_model)

anova(model, sing_model)



