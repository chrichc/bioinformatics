WJ_miRNA_expression<- read.delim("WJ_miRNA_expression.txt", header = TRUE)
View(WJ_miRNA_expression)

data <- data.frame(name = WJ_miRNA_expression$Accession[2:100], value = WJ_miRNA_expression$Average_expression[2:100],
                   sd = WJ_miRNA_expression$SD[2:100])
View(data)

library(ggplot2)
ggplot(data) +
  geom_bar(aes(x = reorder(name, -value), y = value), stat = "identity", fill = "skyblue", alpha=0.7, xlab = FALSE) +
  geom_errorbar(aes(x = name, ymin = value-sd, ymax = value+sd), width = 0.5, colour = "orange", alpha = 0.9, size = 0.5) +
  labs(x = "", y = "Expression values")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 3))

ggsave("WJ_miRNA_expression_unclean.png")


WJ_miRNA_expression_m<- read.delim("WJ_miRNA_expression_modified.txt", header = TRUE)
View(WJ_miRNA_expression_m)

data_m <- data.frame(name = WJ_miRNA_expression_m$Accession[1:50], value = WJ_miRNA_expression_m$Average_expression[1:50],
                   sd = WJ_miRNA_expression_m$SD[1:50])
View(data_m)

library(ggplot2)
ggplot(data_m) +
  geom_bar(aes(x = reorder(name, -value), y = value), stat = "identity", fill = "skyblue", alpha=0.7, xlab = FALSE) +
  geom_errorbar(aes(x = name, ymin = value-sd, ymax = value+sd), width = 0.5, colour = "orange", alpha = 0.9, size = 0.5) +
  labs(x = "", y = "Expression values")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 5))

ggsave("WJ_miRNA_expression_clean_50.png")
