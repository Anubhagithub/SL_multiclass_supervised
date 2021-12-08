setwd("/home/nikola/new_def_SL_SV/featues/")
bin = read.csv("model_input_revise2_slsvsdl2.csv")
is.na(bin)
bin_fltr = bin[which(bin$shortest_path != Inf),]
bin_fltr2 = na.omit(bin_fltr)
library(dplyr)
data = bin_fltr2
data = sample_frac(bin_fltr2, 0.10)
data = data[, c(1,23)]
library(mltools)
library(data.table)
newdata <- one_hot(as.data.table(data))
library(caret)
dummy <- dummyVars(" ~ .", data=data, fullRank = T)
newdata <- data.frame(predict(dummy, newdata = data)) 
library(reshape2)
newdata <- dcast(data = data,  pair ~ gi, length)
write.csv(newdata, "one_hot_allnetwprop_10percent.csv", row.names = F, quote = F)

