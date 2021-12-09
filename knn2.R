#caution whenever come across NAN error convert dat frame into matrix and conver
#the rownames as gene name pair
#change character to factor, inter to factor or numeric
#remove NAs and Infs
library(caret)
library(class)
library(dplyr)
setwd("/home/nikola/new_def_SL_SV/featues/")
info = read.csv("model_input_revise2_slsvsdl2.csv")
info = info[which(info$shortest_path != Inf),]
info = na.omit(info)
info[["pair"]] = factor(info[["pair"]])
info[["gi"]] = factor(info[["gi"]])
info2 = info

#normalise = function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))
#}
dim(info2)
#info.subset = as.info.frame(lapply(info[, 1:6], normalise))
set.seed(123)
anyNA(info2)

info2 = na.omit(info2)
info2 = as.matrix(info2)
ind3 = sample(1:nrow(info2), size = nrow(info2) * 0.7, replace = F)
tr.knear = info2[ind,]
te.knear = info2[-ind,]
tr.klabels = info2[ind, 23]
te.klabels = info2[-ind, 23]
#to solve NAN problem
tr.knear2 = tr.knear[,-c(1, 23)]
View(tr.knear)
rownames(tr.knear2) = tr.knear[,1]
te.knear2 = te.knear[,-c(1, 23)]
rownames(te.knear2) = te.knear[,1]
nrow = NROW(tr.klabels)
sqrt = sqrt(nrow)
knn_model2 = knn(train = tr.knear2, test = te.knear2, cl = tr.klabels, k = 290)
confusionMatrix(table(knn_model2, te.knear[,23]))
