setwd("/home/nikola/new_def_SL_SV/featues/")
data = read.csv("model_input_revise2_slsvsdl2.csv")
data2 = data[, c(1:22)]
normalise = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#data2 = data2[, c(1:6, 23)]
dim(data2)
#data.subset = as.data.frame(lapply(data2[, 1:22], normalise))

set.seed(123)
data2 = na.omit(data2)
data3 = data[, c(1:23)]
data3 = na.omit(data3)
ind = sample(1:nrow(data2), size = nrow(data2) * 0.7, replace = F)
train.knear = as.data.frame(data3[ind,])
test.knear = as.data.frame(data3[-ind,])

train.klabels = as.data.frame(data3[ind, 23])
test.klabels = as.data.frame(data3[-ind, 23])
train.knear = na.omit(train.knear)
test.knear = na.omit(test.knear)
test.klabels = na.omit(test.klabels)
train.klabels = na.omit(train.klabels)

library(class)
NROW(train.klabels)
sqrt(87157)
knn.295 = knn(train = train.knear, test = test.knear, cl = train.klabels, k = 295)
knn(train = train.knear, test = test.knear, cl = train.klabels, k = 295)


#another

a= 0.8
data3 = data[, c(1:6, 23)]
train.index = sample.int(nrow(data3), nrow(data)*a)
str(train.index)
data.train = data3[train.index,]
data.test = data3[-train.index,]
library(class)
knn(data.train[, c("degree", "shortest_path", "bp_count", "cc_count", "mf_count")], 
    data.test[, c("degree", "shortest_path", "bp_count", "cc_count", "mf_count")],
    data.train$gi)

#another try
data3 = data
data3 = na.omit(data3)
row_labels = data3[,23]
data3$gi = as.factor(data3$gi)
data3$pair = as.factor(data3$pair)
#data3$gi = na.omit(data3$gi)
#data3$pair = na.omit(data3$pair)
#data3[, 1:22] = scale(data3[,1:22])
set.seed(123)
size = floor(0.7 * nrow(data3))
size = sample(1:nrow(data3),0.7*nrow(data3))
train1 = sample(seq_len(nrow(data3)), size = size)
train1 = na.omit(train1)
train2 = as.data.frame(data3[train1, 23])
dim(train2)
train2 = na.omit(train2)
test2 = row_labels[-train1]
test2 = na.omit(test2)
data_train = data3[train1,1:22]
dim(data_train)
data_train = na.omit(data_train)
data_test = data3[-train1, 1:22]
data_test = na.omit(data_test)
library(class)
train = data_train
test = data_test
cl = train2
cl = as.vector(cl)
prediction = knn(train, test, cl, k = round(sqrt(nrow(data_train))))
na.omit(data_train)
class(train2)
train2 = as.vector(train2)
dim(train)
dim(cl)
dim(data_train)
class(cl)
dim(train2)

#trial
library(gapminder)
library(class)
library(dplyr)
condata = head(gapminder, 200)
condata <- condata[,-c(1,3)]
condata_preprocess <- preProcess(condata[,c(2:4)],method = c("range"))
library(class)
library(dplyr)
library(gapminder)
## Continent K-Nearest Neighbors 

condata <- head(gapminder,200)

## Clean data

condata <- condata[,-c(1,3)]

condata
summary(condata)

## Normalize data

condata_preprocess <- preProcess(condata[,c(2:4)],method = c("range"))
condata_norm <- predict(condata_preprocess,condata[,c(2:4)])

summary(condata_norm)

## Split data into training and test

condata_ran <- sample(1:nrow(condata),0.7*nrow(condata))

condata_train <- condata_norm[condata_ran,]
condata_test <- condata_norm[-condata_ran,]

nrow(condata_train)
nrow(condata_test)
view(condata_train)

## Create training and test labels

condata_train_labels <- condata[condata_ran,1]
class(condata_train_labels)
condata_test_labels <- condata[-condata_ran,1]

condata_train_labels
condata_test_labels
nrow(condata_train_labels)
view(cbind.data.frame(condata_train_labels,condata_train))

## Build KNN

condata_knn <- knn(condata_train,condata_test,cl = condata_train_labels$continent,k = 15)
condata_knn = as.data.frame(condata_knn)

#my data trial
install.packages("IDPmisc")
library(IDPmisc)
NaRV.omit(data)
data
data = na.omit(data)
data = data[is.finite(data$shortest_path), ]
data$pair = as.factor(data$pair)
data$gi = as.factor(data$gi)
#data = filter(data$shortest_path == "Inf")
data_pre = preProcess(data[,c(1:22)],method = c("range"))
data_norm <- predict(data_pre,data[,c(1:22)])
data_ran <- sample(1:nrow(data),0.7*nrow(data))
train_data <- data_norm[data_ran,]
train_data = na.omit(train_data)
test_data <- data_norm[-data_ran,]
nrow(train_data)
nrow(test_data)
data_train_labels <- as.data.frame(data[data_ran,23])
class(data_train_labels)
colnames(data_train_labels) = "gi"
View(data_train_labels)
#data_train_labels = data[, -23]
dim(data_train_labels)
#data_test_labels <- data[-data_ran,23]
nrow(data_train_labels)
#View(cbind.data.frame(data_train_labels,train_data))
cl = as.data.frame(data_train_labels[, 1])
cl = na.omit(cl)
colnames(cl) = "gi"
#cl = data_train_labels
#cl = data_train_labels[1:85446, 23]
dim(cl)
dim(data_train_labels)
dim(train_data)
data_knn <-
  knn(train_data,
      test_data,
      cl$gi,
      k = 290)
dim(cl)
dim(train_data)
dim(data_train_labels)
sqrt(84116)
