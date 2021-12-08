BiocManager::install("WGCNA")
library(WGCNA)

?correlationPreservation
vec = fet[, c(5,6)]
vec = unique(vec)
vec = na.omit(vec)
vec_t = t(vec$symbol2)
class(vec_t)
vec2 = as.list(vec)
correlationPreservation(vec2, excludeGrey = TRUE, greyLabel = "grey")
library(limma)
library(edgeR)
ls("package:edgeR")
BiocManager::install("dcanr")
library(dcanr)
cor_pairs = cor.pairs(x, cor.method = c("spearman"))
x <- matrix(rnorm(200), 100, 2)
mat = as.data.frame(xtabs( ~ symbol1+symbol2, data=vec))
mat_cor = mat[which(mat$Freq == 0),]
BiocManager::install("wTO")
library(wTO)
ls("package:wTO")
?CorrelationOverlap
class(vec)
vec = as.matrix(vec)
View(mat)
