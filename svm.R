setwd("/home/nikola/new_def_SL_SV/featues/")
getwd()
library(caret)
bin = read.csv("model_input_revise2_slsvsdl2.csv")
is.na(bin)
bin_fltr = bin[which(bin$shortest_path != Inf),]
bin_fltr2 = na.omit(bin_fltr)
set.seed(3033)
library(dplyr)
data = sample_frac(bin_fltr2, 0.02)
intrain = createDataPartition(y = data$gi, p = 0.7, list = FALSE)
svm_train = data[intrain,]
svm_test = data[-intrain,]
dim(svm_train)
dim(svm_test)
anyNA(data)
svm_train[["gi"]] = factor(svm_train[["gi"]])
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_linear = train(gi ~., data = svm_train, method = "svmLinear", 
                   trControl = trctrl, preProcess = c("center", "scale"),
                   tuneLength = 10)
#another trial
library(ggplot2)
install.packages("e1071")
library(e1071)
data = data[, -1]
svm_model = svm(factor(gi) ~ . , data, type = "C")
summary(svm_model)
plot(svm_model, data = data, degree ~ shortest_path, slice = list(bp_count = 1, cc_count = 3))
svm_predict = predict(svm_model, data)
library(data.table)
svm_cnfmat = table(Predicted = svm_predict, Actual = data$gi)
svm_acc = sum(diag(svm_cnfmat)/sum(svm_cnfmat)) #accuracy 65%
# the misclassification rate would be 1 - svm_acc
svm_cnfmat
data$gi = as.factor(data$gi)
svm_tune = tune(svm, gi ~ . , data, type = "C", ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
tuned <- tune(svm, gi ~ ., data = data, type = "C", ranges = list(gamma = 10 ^ (-6:-1), 
cost = 10 ^ (-1:1)), tunecontrol = tune.control(sampling = "cross",cross=10))
tuned <- tune(svm, gi ~ ., data = data, scale = FALSE, type = "C",ranges = list(gamma = 2^(-1:1), 
cost = 2^(2:4)),tunecontrol = tune.control(sampling = "fix"))
#again
library(e1071)
data = sample_frac(bin_fltr2, 0.20)
#data = bin_fltr2
data = data[,c(1, 2, 3, 4, 5, 6, 23)]
dim(data)
#ind = sample(2, nrow(data), replace = TRUE, prob = c(0.60, 0.40))
#s = sample(12017, 7210)
s = sample(12017, 8411)
#s = sample(120166, 72099)
svm_train = data[s,]
svm_test = data[-s,]
svm_fit = svm(factor(gi) ~., data = svm_train, type = "C", cost = 0.1, scale = FALSE)
#tuned = tune(svm, factor(gi) ~., data = svm_train, ranges = list(cost = c(0.001, 0.01, 0.1, 1, 10, 100)))
p = predict(svm_fit, svm_train, type = "class")
table(p)
plot(p)
svm_predict2 = predict(svm_fit, svm_train)
svm_cnfmatri = table(Predicted = svm_predict2, Actual = svm_train$gi)
svm_acc2 = sum(diag(svm_cnfmatri)/sum(svm_cnfmatri))
#accuracy 68%
plot(svm_fit, svm_train, cc_count ~ bp_count, splice = list(degree = ))

library(kernlab)
model.ksvm = ksvm(factor(gi) ~ degree + shortest_path + bp_count + cc_count + mf_count, data = svm_train, type="C-svc")
p2 = predict(model.ksvm, svm_train, type = "decision")
table(p)
plot(p)
svm_predict3 = predict(model.ksvm, svm_train)
svm_cnfmatri3 = table(Predicted = svm_predict3, Actual = svm_train$gi)
svm_acc3 = sum(diag(svm_cnfmatri)/sum(svm_cnfmatri))
#plot(model.ksvm, data = svm_train)
