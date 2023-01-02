MSC_miRNA <- read.delim("test.txt", header = TRUE)
View(MSC_miRNA)
MSC_miRNA1 <- MSC_miRNA[,-1]
rownames(MSC_miRNA1) <- MSC_miRNA[,1]
head(MSC_miRNA1)
View(MSC_miRNA1)
dim(MSC_miRNA1)

library(pheatmap)
pheatmap(MSC_miRNA1, scale = "row", cluster_row = TRUE, cluster_col = TRUE, cellwidth = 10, cellheight = 5, fontsize = 5, color = hcl.colors(50, "BluYl"))
