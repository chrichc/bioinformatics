p <- read.delim("/Users/username/Desktop/total_miRNA/pca.txt", header = TRUE, stringsAsFactors = FALSE)
head(p)
str(p)

matrixp<- as.data.frame(t(p), stringsAsFactors=FALSE)
str(matrixp)

names(matrixp) <- matrixp[1,]
row.names(matrixp) <- NULL
matrixp1 <- matrixp[-1,]
str(matrixp1)

for (i in 1:dim(matrixp1)[2]) {
  if (all(!is.na(as.numeric(matrixp1[,i])))) {
    matrixp1[,i] <- as.numeric(matrixp1[,i])
  }
}

str(matrixp1)
dim(matrixp1)
head(matrixp1)
View(matrixp1)

p_add <- cbind(matrixp1, c("BCC", "BCC", "BCC", "pBCC", "pBCC", "pBCC"))
colnames(p_add)[39] <- "group"
View(p_add)


head(p_add)
install.packages("ggfortify")
library(ggfortify)
head(p_add[1:38])

dfn <- p_add[1:38]
pca_resn <- prcomp(dfn, scale. = TRUE)
plot(pca_resn, type="line")
View(pca_resn$rotation)
summary(pca_resn)
biplot(pca_resn)
print(pca_resn)
autoplot(pca_resn)
autoplot(pca_resn, data = p_add, colour = 'group', size = 7) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face  = 'bold'))

ggsave("PCA_sample_021522.png")






