library(caret)
library(class)
library(dplyr)
setwd("/home/nikola/new_def_SL_SV/featues/")
data = read.csv("model_input_revise2_slsvsdl2.csv")
data = data[which(data$shortest_path != Inf),]
data = na.omit(data)
data[["pair"]] = factor(data[["pair"]])
data[["gi"]] = factor(data[["gi"]])
data2 = data[, c(1:6, 23)]
data2 = data
#normalise = function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))
#}
dim(data2)
#data.subset = as.data.frame(lapply(data[, 1:6], normalise))
set.seed(123)
anyNA(data2)
#is.na(data2)
data2 = as.matrix(data2)
data2 = na.omit(data2)
dim(data2)
ind = sample(1:nrow(data2), size = nrow(data2) * 0.7, replace = F)
train.knear = data2[ind,]
test.knear = data2[-ind,]
train.klabels = data2[ind, 7]
test.klabels = data2[-ind, 7]
#to solve NAN problem
train.knear2 = train.knear[,-c(1, 7)]
rownames(train.knear2) = train.knear[,1]
test.knear2 = test.knear[,-c(1, 7)]
rownames(test.knear2) = test.knear[,1]
nrow = NROW(train.klabels)
sqrt = sqrt(nrow)
knn_model_70_per_partial_col = knn(train = train.knear2, test = test.knear2, cl = train.klabels, k = 268)
confusionMatrix(table(knn_model_70_per_partial_col, test.knear[,7]))
#for 70% train and 30% test accuracy was 70.37% and k was 290
#for 60% train and 40 % test accuracy was 70.06% k was 268
#for 60 40 ration if only GO terms and degree and shortest path is used 
#then accuracy is 68.45%
#for 70 30 ration if only GO terms and degree and shortest path is used 
#then accuracy is 68.43%