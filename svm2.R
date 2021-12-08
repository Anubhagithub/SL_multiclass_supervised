setwd("/home/nikola/new_def_SL_SV/featues/")
library(caret)
bin = read.csv("model_input_revise2_slsvsdl2.csv")
bin_fltr = bin[which(bin$shortest_path != Inf),]
bin_fltr2 = na.omit(bin_fltr)
#memory data
data = data2
data = sample_frac(data, 0.10)
set.seed(3033)
library(dplyr)
#data = bin_fltr2
library(e1071)
trainsvm = createDataPartition(y = data$gi, p = 0.7, list = FALSE)
tr_svm = data[trainsvm,]
te_svm = data[-trainsvm,]
anyNA(data)
data = na.omit(data)
tr_svm[["gi"]] = factor(tr_svm[["gi"]])
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_model = train(gi ~., data = tr_svm, method = "svmLinear", trControl = trctrl, 
preProcess = c("center", "scale"), tuneLength = 10)
mymodel = svm(gi ~ ., data = data)

#memory managment
setwd("/home/nikola/new_def_SL_SV/featues/")
index <- 0
chunkSize <- 120166
higgs_colnames <- c("pair", "degree", "shortest_path", "bp_count", "cc_count", "mf_count", "betweenness", "closeness", "coreness", "constraint", "eccentricity", "eigen_centrality", "hub_score", "neighborhood_n1", "neighborhood_n2", "neighborhood_n3", "neighborhood_n4", "neighborhood_n5", "neighborhood_n6", "cohesion", "adhesion", "mutual_neighbours", "gi")
transactFile <-"model_input_revise2_slsvsdl2.csv"
con <- file(description=transactFile,open="r")
data <- read.table(con, nrows=chunkSize, header=T, fill=TRUE, sep=",")
data = data[which(data$shortest_path != Inf),]
anyNA(data)
data = na.omit(data)
data = unique(data)
data2 = data
chunkSize <- 116047
dataChunk <- read.table(con, nrows=chunkSize, header=T, fill=TRUE, sep=",", col.names=higgs_colnames)
anyNA(dataChunk)
counter <- 0
total_lepton_pT <- 0
repeat {
  index <- index + 1
  print(paste('Processing rows:', index * chunkSize))
  
  total_lepton_pT <- total_lepton_pT + sum(dataChunk$lepton_pT)
  counter <- counter + nrow(dataChunk)
  
  if (nrow(dataChunk) != chunkSize){
    print('Processed all files!')
    break}
  
  dataChunk <- read.table(con, nrows=chunkSize, skip=0, header=FALSE, fill = TRUE, sep=",", col.names=higgs_colnames)
  
  if (index > 3) break
  
}
close(con)
dim(dataChunk)
print(paste0('lepton_pT mean: ',  total_lepton_pT / counter))


if(.Platform$OS.type == "ubuntu") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
#RAM management
install.packages("disk.frame")
library(disk.frame)
setup_disk.frame()
diskf = disk.frame::csv_to_disk.frame("/home/nikola/new_def_SL_SV/featues/model_input_revise2_slsvsdl2.csv")
diskf2 = as.data.frame(diskf)
Sys.getenv("PATH")
df = disk.frame("/home/nikola/.local/bin/diskf")
head(df)

anyNA(diskf2)
diskf2 = diskf2[which(diskf2$shortest_path != Inf),]
diskf2 = na.omit(diskf2)
data = diskf2
anyNA(data)
#data = sample_frac(data, 0.10)
set.seed(3033)
library(dplyr)
library(caret)
library(e1071)
trainsvm = createDataPartition(y = data$gi, p = 0.7, list = FALSE)
tr_svm = data[trainsvm,]
te_svm = data[-trainsvm,]
anyNA(data)
data = na.omit(data)
tr_svm[["gi"]] = factor(tr_svm[["gi"]])
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
svm_model = train(gi ~., data = tr_svm, method = "svmLinear", trControl = trctrl, 
                  preProcess = c("center", "scale"), tuneLength = 10)
library(dplyr)