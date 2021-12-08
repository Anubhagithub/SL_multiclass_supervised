setwd("/home/nikola/new_def_SL_SV/featues/")
bin = read.csv("model_input_revise2_slsvsdl2.csv")
is.na(bin)
bin_fltr = bin[which(bin$shortest_path != Inf),]
bin_fltr2 = na.omit(bin_fltr)
library(stats)
dim(bin_fltr2)
library(mltools)
library(data.table)
library(caret)
library(reshape2)
library(randomForest)
index = sample(2, nrow(bin_fltr2), replace = TRUE, prob = c(0.60, 0.40))
training = bin_fltr2[index == 1,]
testing = bin_fltr2[index == 2,]
class(bin_fltr2$gi)
#bin_fltr2$gi = as.factor(bin_fltr2$gi)
training$gi = as.factor(training$gi)
class(training$gi)
model1 = randomForest(gi ~ . , data = training)
prediction = predict(model1, newdata = testing[-23])
predict_gi = predict(model1, newdata = testing)
testing$predict_gi = predict_gi
#cm = table(test_set[,7], pred2)
cnf_mat = table(testing$gi, testing$predict_gi)
cnf_mat
classification_acc = sum(diag(cnf_mat)/sum(cnf_mat)) #accuracy = 70.98% == 71%
#76% for 70/30
#77% 60/40