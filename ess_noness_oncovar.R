setwd("/home/nikola/new_def_SL_SV/ess_non_ess/Onco_genes_OncoVar_TCGA/")
emp = read.csv("TCGA.ACC.onco.genes.OncoVar.tsv", sep = "\t")
emp2 = as.data.frame(emp$Gene_symbol)
colnames(emp2) = "gene"
emp3 = read.csv("TCGA.BRCA.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.CESC.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.ESCA.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.KICH.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.KIRC.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.KIRP.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.LIHC.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.LUAD.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.LUSC.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.OV.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.PAAD.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.SKCM.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.STAD.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
emp3 = read.csv("TCGA.THCA.onco.genes.OncoVar.tsv", sep = "\t")
emp4 = as.data.frame(emp3$Gene_symbol)
colnames(emp4) = "gene"
emp2 = rbind(emp2, emp4)
uni_emp2 = as.data.frame(unique(emp2$gene))
colnames(uni_emp2) = "gene"
setwd("/home/nikola/new_def_SL_SV/ess_non_ess/")
cgi_SL = read.csv("All_synthetic_lethality_gene_pairs_cgidb.csv")
exp_cgi = cgi_SL[which(cgi_SL$source != 'our prediction'),]
exp_tissue_cgi = cgi_SL[which(cgi_SL$cancer.or.cellline == 'ACC'),]
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'BRCA'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'CESC'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'ESCA'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'KICH'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'KIRC'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'KIRP'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'LIHC'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'LUAD'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'LUSC'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'OV'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'PAAD'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'STAD'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'THCA'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)
exp_tissue_cgi2 = cgi_SL[which(cgi_SL$cancer.or.cellline == 'SKCM'),]
exp_tissue_cgi = rbind(exp_tissue_cgi, exp_tissue_cgi2)

exp_tissue_cgi$pair = paste(exp_tissue_cgi$symbol1, exp_tissue_cgi$symbol2, sep = "_")
uni_sym1 = as.data.frame(unique(exp_tissue_cgi$symbol1))
uni_sym2 = as.data.frame(unique(exp_tissue_cgi$symbol2))
colnames(uni_sym1) = "gene"
colnames(uni_sym2) = "gene"
common = as.data.frame(intersect(uni_emp2$gene, uni_sym1$gene))
common2 = as.data.frame(intersect(uni_emp2$gene, uni_sym2$gene))
