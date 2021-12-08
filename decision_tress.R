setwd("/home/nikola/new_def_SL_SV/featues/")
data = read.csv("model_input_revise2_slsvsdl2.csv")
dim(data)
data = na.omit(data)
library(dplyr)
#use part of teh adat set
data = data[, c(1:6, 23)]
data2 = sample_frac(data, 0.10)
#use the entire data set
data2 = data
set.seed(1234)
dim(data2)
data2 = na.omit(data2)
data2$pair = as.factor(data2$pair)
data2$gi = as.factor(data2$gi)

s1 = sample(2, nrow(data2), replace = T, prob = c(0.7, 0.3))
train_dt = data2[s1 == 1,]
test_dt = data2[s1 == 2,]
train_dt = na.omit(train_dt)
test_dt = na.omit(test_dt)
library(party)
library(caTools)
library(rpart)
library(e1071)
library(caret)
m = gi ~ degree+shortest_path+bp_count+cc_count+mf_count
tree = rpart(m, data = train_dt)
plot(tree)
p = predict(tree, test_dt, type = "class")
table(p)
confusionMatrix(p, test_dt$gi, positive = "1")
table(predict(tree), train_dt$gi)
#with 0.01 percent of the entire data
#63% accuracy with 70% training and 30% test_dt
#65% accuracy with 60% training and 40% test
#68% '' with 0.10 percent of entire data
#68% peccent accuracy with entire data set with 60% training and 40% test
#68% peccent accuracy with entire data set with 70% training and 30% test

