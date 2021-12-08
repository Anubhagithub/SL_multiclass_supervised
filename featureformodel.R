setwd("/home/nikola/new_def_SL_SV/featues/")
#deg feature
sl = read.csv("SL_degree.csv", sep = ",")
sv = read.csv("SV_degree.csv", sep = ",")
sl$pair = paste(sl$symbol1, sl$symbol2, sep = "_")
sl$gi = "SL"
sub_sl = sl[, c(7, 6, 8)]
sv$pair = paste(sv$symbol1, sv$symbol2, sep = "_")
sv$gi = "SV"
sub_sv = sv[, c(7,6,8)]
uni_sl = unique(sub_sl)
uni_sv = unique(sub_sv)
sl_sv = rbind(uni_sl, uni_sv)
uni_sl_sv = unique(sl_sv)
#path length
path = read.csv("sl_string.csv", sep = "\t")
path2 = read.csv("sv_string.csv", sep = "\t")
path$pair = paste(path$symbol1, path$symbol2, sep = "_")
path2$pair = paste(path2$symbol1, path2$symbol2, sep = "_")
path$gi = "SL"
path2$gi = "SV"
path_sub = path[, c(68, 52, 69)]
path2_sub = path2[, c(68, 52, 69)]
uni_path = unique(path_sub)
uni_path2 = unique(path2_sub)
path_path2 = rbind(uni_path, uni_path2)
uni_path_path2 = unique(path_path2)
library(dplyr)
#diff = anti_join(path_path2, uni_path_path2)
#diff = uni_path_path2[!uni_path_path2$pair %in% path_path2$pair,]
#diff = setdiff(path_path2$pair, uni_path_path2$pair)
dim(path_path2)
dim(uni_path_path2)
cmn_deg_path = as.data.frame(intersect(uni_path_path2$pair, uni_sl_sv$pair))
new_df = merge(uni_sl_sv, uni_path_path2, by.x = "pair", by.y = "pair")
is.na(new_df)
#new_df = merge(uni_sl_sv, uni_path_path2, by.x = "pair", by.y = "gi")
#new_df <- full_join(uni_sl_sv, uni_path_path2, by=c("pair", "gi"))
#a = left_join(uni_sl_sv, uni_path_path2, by = c("pair", "gi"))
uni_new_df = na.omit(new_df)
all(uni_new_df$shortest_path == uni_new_df$gi.y)
dual = uni_new_df[which(uni_new_df$gi.x != uni_new_df$gi.y),]
inp = uni_new_df[which(uni_new_df$gi.x == uni_new_df$gi.y),]
#write in csv if want inp(SL, SV distinct) and dual(dual nature), uni_new_df(have both) 
getwd()
write.csv(uni_new_df, "model_input2.csv", quote = F, row.names = F)
#model making
library(tidyverse)
library(modelr)
library(ModelMetrics)
GI_MM = model_matrix(uni_new_df, deg2 ~ gi)
summary(GI_MM)
GI_MM = model_matrix(uni_new_df, deg2 ~ gi-1)
summary(GI_MM)
df2 = cbind(uni_new_df[, c(2,4)], GI_MM) 
head(df2)
modl = lm(shortest_path ~ . , data = uni_new_df)
memory.limit()
install.packages("Metrics")
library(Metrics)
rmse(modl, uni_new_df)
#model making again
library(tidyverse)
library(modelr)
library(ModelMetrics)
library(Metrics)
inp_sub = inp[c(1:10000), c(1, 2 , 3, 4, 5)]
inp_filtr = inp_sub[which(inp_sub$shortest_path != Inf),]
GI_model = model_matrix(inp_filtr, deg2 ~ gi.x)
summary(GI_model)
GI_model = model_matrix(inp_filtr, deg2 ~ gi.x-1)
summary(GI_model)
inp2 = cbind(inp_filtr[, c(2,4)], GI_model) 
head(inp2)
modl = lm(deg2 ~ . , data = inp_filtr)
rmse(modl, inp_filtr)
mae(modl, inp_filtr)
rsquare(modl, inp_filtr)
qae(modl, inp_filtr)
install.packages("randomForest")
library(randomForest)
library(ggplot2)
library(dplyr)
noinf = inp[which(inp$shortest_path != Inf),]
inp_train = sample_frac(noinf, 0.10)
inp_test = sample_frac(noinf, 0.05)
mod = randomForest(deg2 ~ . , data = inp_train)
mod
rmse(mod, inp_train)
mae(mod, inp_train)
qae(mod, inp_train)
rsquare(mod, inp_train)
rsquare(mod, inp_test)
mod2 = randomForest(shortest_path ~ . , data = inp_train)
qae(mod2, inp_train)
rsquare(mod2, inp_train)
rsquare(mod2, inp_test)
