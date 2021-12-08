setwd("/home/nikola/new_def_SL_SV/featues/")
data = read.csv("model_input_revise2_slsvsdl2.csv")
library(dplyr)
library(naivebayes)
data = na.omit(data)
#View(data)
#use entire data
data2 = data
#data2 = data[, c(1:6, 23)]
data2$gi = as.factor(data2$gi)
data2$pair = as.factor(data2$pair)
index = sample(2, nrow(data2), replace = T, prob = c(0.6, 0.4))
train_nb = data2[index == 1,]
test_nb = data2[index == 2,]
library(e1071)
library(caret)
#n = gi ~ degree+shortest_path+bp_count+cc_count+mf_count
#mod = naiveBayes(n, data = train_nb)
model_nb = naiveBayes(gi ~ degree+shortest_path+bp_count+cc_count+mf_count+betweenness+closeness+coreness+constraint+eccentricity+eigen_centrality+hub_score+neighborhood_n1+neighborhood_n2+neighborhood_n3+neighborhood_n4+neighborhood_n5+neighborhood_n6+cohesion+adhesion+mutual_neighbours, data = train_nb)
p = predict(model_nb, test_nb)

confusionMatrix(p, test_nb$gi, positive = "1")
table(predict(model_nb), train_dt$gi)


s = sample(nrow(data2), nrow(data2)*.8)
train_nb = data2[s == 1,]
test_nb = data2[s == -1,]
model = naiveBayes(gi ~., data = train_nb)
model2 = naiveBayes(gi ~., data = train_nb, laplace = 1)
pred = predict(model, test_nb)
pred2 = predict(model2, test_nb [,-1])
table(p)
table(test_nb$gi)
