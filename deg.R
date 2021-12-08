setwd("/home/nikola/new_def_SL_SV/featues/")
degSL = read.csv("SL_degree.csv", sep = ",")
head(degSL)
degSV = read.csv("SV_degree.csv", sep = ",")
degSL$new = paste(degSL$symbol1, degSL$symbol2, sep = "_")
degSL$new2 = paste(degSL$symbol2, degSL$symbol1, sep = "_")
degSV$new = paste(degSV$symbol1, degSV$symbol2, sep = "_")
degSV$new2 = paste(degSV$symbol2, degSV$symbol1, sep = "_")
cmn = as.data.frame(intersect(degSL$new, degSV$new2))
cmn2 = as.data.frame(intersect(degSV$new, degSL$new2))
colnames(cmn) = "name"
colnames(cmn2) = "name"
cmn3 = as.data.frame(intersect(cmn, cmn2))
