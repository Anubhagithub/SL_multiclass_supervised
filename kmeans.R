library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
View(iris)
setwd("/home/nikola/new_def_SL_SV/featues/")
library(caret)
bin = read.csv("model_input_revise2_slsvsdl2.csv")
bin = bin[which(bin$shortest_path != Inf),]
bin = na.omit(bin)
data = bin
anyNA(data)
data[["pair"]] = factor(data[["pair"]])
data[["gi"]] = factor(data[["gi"]])
dim(data)
data = data %>% mutate_if(is.character, as.factor)
#data = as.matrix(data)
mydata = select(data, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22))

#data = mydata
anyNA(mydata)
dim(mydata)
mydata[["pair"]] = factor(mydata[["pair"]])
data = as.matrix(data)
wssplot = function(data, nc = 15, seed = 1234){
  wss = (nrow(data)-1)*sum(apply(data, 2, var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] = sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}
class(mydata)
mydata = as.matrix(mydata)
#mydata = as.vector(mydata)
anyNA(data)
wssplot(mydata2)
dim(mydata)
mydata2 = mydata[,-c(1)]
rownames(mydata2) = mydata[,1]
KM = kmeans(mydata2[!is.na(mydata2)], 2)
KM = kmeans(mydata2, 2)
autoplot(KM2, mydata2, frame = TRUE)
KM$centers

#below this is not required
summary(data)
class(data$pair)
class(data$degree)
class(data$shortest_path)
class(data$bp_count)
data[["bp_count"]] = factor(data[["bp_count"]])
class(data$cc_count)
data[["cc_count"]] = factor(data[["cc_count"]])
class(data$mf_count)
data[["mf_count"]] = factor(data[["mf_count"]])
class(data$betweenness)
class(data$closeness)
class(data$coreness)
class(data$constraint)
class(data$eccentricity)
class(data$eigen_centrality)
class(data$hub_score)
class(data$neighborhood_n1)
class(data$neighborhood_n2)
class(data$neighborhood_n3)
class(data$neighborhood_n4)
class(data$neighborhood_n5)
class(data$neighborhood_n6)
class(data$cohesion)
data[["cohesion"]] = factor(data[["cohesion"]])
class(data$adhesion)
data[["adhesion"]] = factor(data[["adhesion"]])
class(data$mutual_neighbours)
data[["mutual_neighbours"]] = factor(data[["mutual_neighbours"]])

bin = read.csv("model_input_revise2_slsvsdl2.csv")
bin = bin[which(bin$shortest_path != Inf),]
bin = bin[which(bin$degree != Inf),]
bin = bin[which(bin$bp_count != Inf),]
bin = bin[which(bin$cc_count != Inf),]
bin = bin[which(bin$mf_count != Inf),]
bin = bin[which(bin$betweenness != Inf),]
bin = bin[which(bin$closeness != Inf),]
bin = bin[which(bin$coreness != Inf),]
bin = bin[which(bin$constraint != Inf),]
bin = bin[which(bin$eccentricity != Inf),]
bin = bin[which(bin$eigen_centrality != Inf),]
bin = bin[which(bin$hub_score != Inf),]
bin = bin[which(bin$neighborhood_n1 != Inf),]
bin = bin[which(bin$neighborhood_n2 != Inf),]
bin = bin[which(bin$neighborhood_n3 != Inf),]
bin = bin[which(bin$neighborhood_n4 != Inf),]
bin = bin[which(bin$neighborhood_n5 != Inf),]
bin = bin[which(bin$neighborhood_n6 != Inf),]
bin = bin[which(bin$cohesion != Inf),]
bin = bin[which(bin$adhesion != Inf),]
bin = bin[which(bin$mutual_neighbours != Inf),]
bin = na.omit(bin)
data = bin
anyNA(data)
data[["pair"]] = factor(data[["pair"]])
data[["gi"]] = factor(data[["gi"]])
dim(data)
data = data %>% mutate_if(is.character, as.factor)

bin2 = read.csv("model_input_revise2_slsvsdl2.csv")
is.na(data) = sapply(data, is.infinite)
data = as.matrix(data)
