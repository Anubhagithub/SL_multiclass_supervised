setwd("/home/nikola/new_def_SL_SV/featues/")
fet = read.csv("sl_string.csv", sep = "\t")
fet$pair = paste(fet$symbol1, fet$symbol2, sep = "_")
fet$gi = "SL"
main = fet[, c(68, 9, 52, 61, 64, 67, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 53, 54, 55, 69)]
uni_main = unique(main)
fet2 = read.csv("sv_string.csv", sep = "\t")
fet2$pair = paste(fet2$symbol1, fet2$symbol2, sep = "_")
fet2$gi = "SV"
main2 = fet2[, c(68, 9, 52, 61, 64, 67, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 53, 54, 55, 69)]
uni_main2 = unique(main2)
#include sdl
sdl = read.csv("sdl_string.csv", sep = "\t")
sdl$pair = paste(sdl$a, sdl$b, sep = "_")
sdl$gi = "SDL"
main3 = sdl[, c(64, 5, 48, 57, 60, 63, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 49, 50, 51, 65)]
uni_main3 = unique(main3)
sl_sv_sdl = rbind(uni_main, uni_main2, uni_main3)
uni_sl_sv_sdl = unique(sl_sv_sdl)
write.csv(uni_sl_sv_sdl, "model_input_revise2_slsvsdl2.csv", row.names = F, quote = F)
#sl_sv = rbind(uni_main, uni_main2)
#uni_sl_sv = unique(sl_sv)
#write.csv(uni_sl_sv, "model_input_revise.csv", row.names = F, quote = F)
#model_build
library(tidyverse)
library(modelr)
library(ModelMetrics)
library(Metrics)
is.na(uni_sl_sv)
sl_sv_fltr = uni_sl_sv[which(uni_sl_sv$shortest_path != Inf),]
new_sl_sv = na.omit(sl_sv_fltr)
modelgi = model_matrix(new_sl_sv, degree ~ gi)
modelgi = model_matrix(new_sl_sv, degree ~ gi-1)
sl_sv_sub = sl_sv_fltr[c(1:1000), c(1:7)]
modllm = lm(degree ~ . , data = sl_sv_sub)
#random forest
library(randomForest)
library(ggplot2)
library(dplyr)
inptrain = sample_frac(sl_sv_fltr, 0.10)
inptest = sample_frac(sl_sv_fltr, 0.05)
mod = randomForest(degree ~ . , data = inptrain)
rmse(mod, inptrain)
mae(mod, inptrain)
qae(mod, inptrain)
rsquare(mod, inptrain)
rsquare(mod, inptest)
#use shortest_path as response variable
mod_path = randomForest(shortest_path ~ . , data = inptrain)
qae(mod_path, inptrain)
rsquare(mod_path, inptrain)
rsquare(mod_path, inptest)
#use GO(bp) term
mod_bp = randomForest(bp_count ~ . , data = inptrain)
qae(mod_bp, inptrain)
rsquare(mod_bp, inptrain)
rsquare(mod_bp, inptest)
#use GO(cc) term
mod_cc = randomForest(cc_count ~ . , data = inptrain)
qae(mod_cc, inptrain)
rsquare(mod_cc, inptrain)
rsquare(mod_cc, inptest)
#use GO(mf) term
mod_mf = randomForest(mf_count ~ . , data = inptrain)
qae(mod_mf, inptrain)
rsquare(mod_mf, inptrain)
rsquare(mod_mf, inptest)
#again
mod_bp_test = randomForest(bp_count ~ . , data = inptest)
rsquare(mod_bp_test, inptest)
rsquare(mod_bp_test, inptrain)
qae(mod_bp_test, inptest)
qae(mod_bp_test, inptrain)

#model_build
library(tidyverse)
library(modelr)
library(ModelMetrics)
library(Metrics)
big = read.csv("model_input_revise2_slsvsdl2.csv")
is.na(big)
big_fltr = big[which(big$shortest_path != Inf),]
big_fltr2 = na.omit(big_fltr)
library(randomForest)
library(ggplot2)
library(dplyr)
tr_gi = sample_frac(sl_sv_sdl_fltr2, 0.60)
te_gi = sample_frac(sl_sv_sdl_fltr2, 0.40)
mod_gi = randomForest(degree ~ . , data = tr_gi)
rmse(mod_gi, tr_gi)
mae(mod_gi, tr_gi)
qae(mod_gi, tr_gi)
rsquare(mod_gi, tr_gi)
rsquare(mod_gi, te_gi)
getTree(mod_gi, 1)
BiocManager::install("reprtree")
BiocManager::install("lattice")
BiocManager::install("mgcv")
BiocManager::install("nlme")
BiocManager::install("survival")
library(lattice)
library(mgcv)
library(nlme)
library(survival)
BiocManager::install("rminer")
library(rminer)
ls("package:rminer")
library(party)
plot(mod_gi, type = "simple")
tree = cforest(gi~., data = sl_sv_sdl_fltr2)
library(caret)
plot_rf_
#use shortest_path as response variable
mod_pathl = randomForest(shortest_path ~ . , data = tr_gi)
qae(mod_pathl, tr_gi)
rsquare(mod_pathl, tr_gi)
rsquare(mod_pathl, te_gi)
#use GO(bp) term
mod_bp2 = randomForest(bp_count ~ . , data = tr_gi)
qae(mod_bp2, tr_gi)
rsquare(mod_bp2, tr_gi)
rsquare(mod_bp2, te_gi)
#use GO(cc) term
mod_cc2 = randomForest(cc_count ~ . , data = tr_gi)
qae(mod_cc2, tr_gi)
rsquare(mod_cc2, tr_gi)
rsquare(mod_cc2, te_gi)
#use GO(mf) term
mod_mf2 = randomForest(mf_count ~ . , data = tr_gi)
qae(mod_mf2, tr_gi)
rsquare(mod_mf2, tr_gi)
rsquare(mod_mf2, te_gi)
#again
mod_bp_test = randomForest(bp_count ~ . , data = inptest)
rsquare(mod_bp_test, inptest)
rsquare(mod_bp_test, tr_gi)
qae(mod_bp_test, inptest)
#closeness
mod_gi_close = randomForest(closeness ~ . , data = tr_gi)
rsquare(mod_gi_close, tr_gi)
rsquare(mod_gi_close, te_gi)
class(mod_gi)
#new model

index = sample(2, nrow(sl_sv_sdl_fltr2), replace = TRUE, prob = c(0.70, 0.30))


#one-hot encoding
big = read.csv("model_input_revise2_slsvsdl2.csv")
is.na(big)
big_fltr = big[which(big$shortest_path != Inf),]
big_fltr2 = na.omit(big_fltr)
write.csv(big_fltr2, "sl_sv_sdl_deg_path_GO.csv", quote = F, row.names = F)
inp_file = read.csv("sl_sv_sdl_deg_path_GO.csv", sep = ",")
big_fltr2 = inp_file
#big_fltr2 = sample_frac(big_fltr2, 0.50)
#data = big_fltr2[c(1,2,3,4, 45000, 45001, 45002, 45003, 120161, 120162, 120163, 120164), c(1:6, 23)]
library(stats)
#index = sample(2, nrow(data), replace = TRUE, prob = c(0.60, 0.40))
#train = data[index == 1,]
#test = data[index ==2,]
dim(big_fltr2)
library(mltools)
library(data.table)
#new_data = one_hot(as.data.table(data))
library(caret)
#dummy = dummyVars("~ .", data = data)
#new_data = data.frame(predict(dummy, newdata = data))
library(reshape2)
#newdata <- dcast(data = data, degree ~ gi, length)
#try predicting with training and test data sets
#onehot_train <- dcast(data = tarin_set, degree ~ gi, length)
library(randomForest)
library(stats)
#big_fltr2 = as.factor(big_fltr2)
ind = sample(2, nrow(big_fltr2), replace = TRUE, prob = c(0.60, 0.40))
train_set = big_fltr2[ind == 1,]
test_set = big_fltr2[ind == 2,]
#as.factor(big_fltr2$gi)
#as.factor(inp_file$gi)
class(big_fltr2$gi)
big_fltr2$gi = as.factor(big_fltr2$gi)
train_set$gi = as.factor(train_set$gi)
#test_set$gi = as.factor(test_set$gi)
modl_pathlen = randomForest(gi ~ . , data = train_set)
#pred = randomForest(gi ~ . , data = test_set)
pred2 = predict(modl_pathlen, newdata = test_set[-7])
gi_pred = predict(modl_pathlen, newdata = test_set)
test_set$gi_pred = gi_pred
cm = table(test_set[,7], pred2)
cm2 = table(test_set$gi, test_set$gi_pred)
cm2
class_acc = sum(diag(cm2)/sum(cm2)) #accuracy = 70.98% == 71%
precision(actual = modl_pathlen, predicted = pred2)
library(caret)
qae(modl_pathlen, train_set)
rsquare(modl_pathlen, train_set)
stats::predict(modl_pathlen, train_set)
ls("package:caret")

#one-hot again
new_data = one_hot(as.data.table(train_set))
library(caret)
library(Matrix)
dummy = dummyVars("~ .", data = train_set)
new_data = data.frame(predict(dummy, newdata = train_set))
library(reshape2)
newdata <- dcast(data = train_set, degree ~ gi, length)
#try predicting with training and test data sets
onehot_train <- dcast(data = train_set, degree ~ gi, length)
BiocManager::install("pryr")
library(pryr)
mem_used()
dim(big_fltr2)
write.csv(big_fltr2, "sl_sv_sdl_deg_path_GO.csv", quote = F, row.names = F)
class(big_fltr2$gi)
