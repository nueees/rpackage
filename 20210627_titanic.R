#setwd("C:/Users/Administrator/Documents/R/Kaggle/titanic")
library(dplyr);library(magrittr);library(caret);library(recipes);
rm(list=ls())
read.csv("./data/gender_submission.csv") -> y_test
bind_rows(read.csv("./data/train.csv") %>%  mutate(index = "train")-> train, 
          read.csv("./data/test.csv")  %>%  mutate(index = "test") -> x_test ) -> full 


str(y_test)
str(full)
summary(full)


head(full)

full$survived <- ifelse(full$survived == 1, "Y", "N")
full$Survived %<>% as.factor()
