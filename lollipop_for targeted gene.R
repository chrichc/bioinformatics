library(ggplot2)
library(ggtext)
library(tidyverse)
install.packages("patchwork")
library(patchwork)
install.packages("rcartocolor")
library(rcartocolor)

mir125b <- read.delim("/Users/christinechang/Desktop/mir125b_gene.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir125b)

plot <- ggplot(mir125b, aes(rank, score, color = category)) +
  geom_segment(
    aes(x = rank, xend = rank, y = 0, yend = score),
    size = 1.2
  ) +
  geom_rect(
    aes(xmin = 1, xmax = 150, ymin = 0, ymax = 100),
    fill = "grey97", color = "grey97"
  ) +
  geom_hline(aes(yintercept = (1*24*60 + 100)), color = "grey88") +
  geom_hline(aes(yintercept = (7*24*60 + 100)), color = "grey85") +
  geom_hline(aes(yintercept = (30*24*60 + 100)), color = "grey82") +
  geom_hline(aes(yintercept = (365*24*60 + 100)), color = "grey79") +
  geom_point(aes(size = score)) +
  scale_y_log10(expand = c(0,0)) +
  rcartocolor::scale_color_carto_d(palette = "Prism", guide = "none") +
  scale_size(
    range = c(1,8),
    limits = c(100, max(mir125b$score)),
    guide = "none"
  ) +
  coord_polar()

plot


#second type of circular plot

library(tidyverse)

mir125b_1 <- read.delim("/Users/christinechang/Desktop/mir125b_gene.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir125b)

data125b <- data.frame(
  individual=mir125b_1$label,
  group=mir125b_1$category,
  value=mir125b_1$score
)
view(data125b)

empty_bar <- 4
to_add <- data.frame(matrix(NA, empty_bar*nlevels(data125b$group), ncol(data125b)))
colnames(to_add) <- colnames(data125b)
to_add$group <- rep(levels(data125b$group), each=empty_bar)
data125b <- rbind(data125b, to_add)
data125b <- data125b %>% arrange(group)
data125b$id <- seq(1, nrow(data125b))

view(data125b)

label_data <- data125b
number_of_bar <- nrow(label_data)
angle <- 90-360*(label_data$id-0.5)　/number_of_bar
label_data$hjust <- ifelse (angle < -90, 1, 0)
label_data$angle <- ifelse (angle < -90, angle+180, angle)

view(label_data)

group.colors <- c("HIF1a unrelated" = "skyblue", "HIF1a related" = "red")

p125b <- ggplot(data125b, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  scale_fill_manual(values = group.colors) +
  geom_text(data = label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE)

p125b

ggsave("mir125b.png")




mir100 <- read.delim("/Users/christinechang/Desktop/mir100_gene.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir100)

data100 <- data.frame(
  individual=mir100$label,
  group=mir100$category,
  value=mir100$score
)
view(data100)

empty_bar <- 1

to_add <- matrix(NA, empty_bar, ncol(data100))
colnames(to_add) <- colnames(data100)
data100 <- rbind(data100, to_add)
data100$id <- seq(1, nrow(data100))

view(data100)

label_data100 <- data100
number_of_bar <- nrow(label_data100)
angle <- 90-360*(label_data100$id-0.5)　/number_of_bar
label_data100$hjust <- ifelse (angle < -90, 1, 0)
label_data100$angle <- ifelse (angle < -90, angle+180, angle)

view(label_data100)

p100 <- ggplot(data100, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", fill = "skyblue", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  geom_text(data = label_data100, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data100$angle, inherit.aes = FALSE)

p100

ggsave("mir100.png")





mir100a <- read.delim("/Users/christinechang/Desktop/mir100_all.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir100a)

data100a <- data.frame(
  individual=mir100a$label,
  group=mir100a$category,
  value=mir100a$score
)
view(data100a)

empty_bar <- 1

to_add <- matrix(NA, empty_bar, ncol(data100a))
colnames(to_add) <- colnames(data100a)
data100a <- rbind(data100a, to_add)
data100a$id <- seq(1, nrow(data100a))

view(data100a)

label_data100a <- data100a
number_of_bar <- nrow(label_data100a)
angle <- 90-360*(label_data100a$id-0.5)　/number_of_bar
label_data100a$hjust <- ifelse (angle < -90, 1, 0)
label_data100a$angle <- ifelse (angle < -90, angle+180, angle)

view(label_data100a)

p100a <- ggplot(data100a, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", fill = "skyblue", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  geom_text(data = label_data100a, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data100a$angle, inherit.aes = FALSE)

p100a

ggsave("mir100a.png")




mir21 <- read.delim("/Users/christinechang/Desktop/mir21_gene.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir21)

data21 <- data.frame(
  individual=mir21$label,
  group=mir21$category,
  value=mir21$score
)
#view(data21)

empty_bar <- 1

to_add <- matrix(NA, empty_bar, ncol(data21))
colnames(to_add) <- colnames(data21)
data21 <- rbind(data21, to_add)
data21$id <- seq(1, nrow(data21))

#view(data21)

label_data21 <- data21
number_of_bar <- nrow(label_data21)
angle <- 90-360*(label_data21$id-0.5)　/number_of_bar
label_data21$hjust <- ifelse (angle < -90, 1, 0)
label_data21$angle <- ifelse (angle < -90, angle+180, angle)

#view(label_data21)

p21 <- ggplot(data21, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", fill = "skyblue", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  geom_text(data = label_data21, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data21$angle, inherit.aes = FALSE)

p21

ggsave("mir21.png")




mir21a <- read.delim("/Users/christinechang/Desktop/mir21_all.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir21a)

data21a <- data.frame(
  individual=mir21a$label,
  group=mir21a$category,
  value=mir21a$score
)
#view(data21a)

empty_bar <- 1

to_add <- matrix(NA, empty_bar, ncol(data21a))
colnames(to_add) <- colnames(data21a)
data21a <- rbind(data21a, to_add)
data21a$id <- seq(1, nrow(data21a))

#view(data21a)

label_data21a <- data21a
number_of_bar <- nrow(label_data21a)
angle <- 90-360*(label_data21a$id-0.5)　/number_of_bar
label_data21a$hjust <- ifelse (angle < -90, 1, 0)
label_data21a$angle <- ifelse (angle < -90, angle+180, angle)

#view(label_data21a)

p21a <- ggplot(data21a, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", fill = "skyblue", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  geom_text(data = label_data21a, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data21a$angle, inherit.aes = FALSE)

p21a

ggsave("mir21a.png")









mir191 <- read.delim("/Users/christinechang/Desktop/mir191_gene.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir191)

data191 <- data.frame(
  individual=mir191$label,
  group=mir191$category,
  value=mir191$score
)
#view(data191)

empty_bar <- 1

to_add <- matrix(NA, empty_bar, ncol(data191))
colnames(to_add) <- colnames(data191)
data191 <- rbind(data191, to_add)
data191$id <- seq(1, nrow(data191))

#view(data191)

label_data191 <- data191
number_of_bar <- nrow(label_data191)
angle <- 90-360*(label_data191$id-0.5)　/number_of_bar
label_data191$hjust <- ifelse (angle < -90, 1, 0)
label_data191$angle <- ifelse (angle < -90, angle+180, angle)

#view(label_data191)

p191 <- ggplot(data191, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", fill = "skyblue", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  geom_text(data = label_data191, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data191$angle, inherit.aes = FALSE)

p191

ggsave("mir191.png")



mir191a <- read.delim("/Users/christinechang/Desktop/mir191_all.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir191a)

data191a <- data.frame(
  individual=mir191a$label,
  group=mir191a$category,
  value=mir191a$score
)
#view(data191a)

empty_bar <- 1

to_add <- matrix(NA, empty_bar, ncol(data191a))
colnames(to_add) <- colnames(data191a)
data191a <- rbind(data191a, to_add)
data191a$id <- seq(1, nrow(data191a))

#view(data191a)

label_data191a <- data191a
number_of_bar <- nrow(label_data191a)
angle <- 90-360*(label_data191a$id-0.5)　/number_of_bar
label_data191a$hjust <- ifelse (angle < -90, 1, 0)
label_data191a$angle <- ifelse (angle < -90, angle+180, angle)

#view(label_data191)

p191a <- ggplot(data191a, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", fill = "skyblue", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  geom_text(data = label_data191a, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data191a$angle, inherit.aes = FALSE)

p191a

ggsave("mir191a.png")






mir30 <- read.delim("/Users/christinechang/Desktop/mir30_gene_all.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir30)

data30 <- data.frame(
  individual=mir30$label,
  group=mir30$category,
  value=mir30$score
)
#view(data30)

empty_bar <- 1

to_add <- matrix(NA, empty_bar, ncol(data30))
colnames(to_add) <- colnames(data30)
data30 <- rbind(data30, to_add)
data30$id <- seq(1, nrow(data30))

#view(data30)

label_data30 <- data30
number_of_bar <- nrow(label_data30)
angle <- 90-360*(label_data30$id-0.5)　/number_of_bar
label_data30$hjust <- ifelse (angle < -90, 1, 0)
label_data30$angle <- ifelse (angle < -90, angle+180, angle)

#view(label_data30)

p30 <- ggplot(data30, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", fill = "skyblue", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  geom_text(data = label_data30, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data30$angle, inherit.aes = FALSE)

p30

ggsave("mir30.png")






mir30a <- read.delim("/Users/christinechang/Desktop/mir30_all.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir30a)

data30a <- data.frame(
  individual=mir30a$label,
  group=mir30a$category,
  value=mir30a$score
)
#view(data30a)

empty_bar <- 1

to_add <- matrix(NA, empty_bar, ncol(data30a))
colnames(to_add) <- colnames(data30a)
data30a <- rbind(data30a, to_add)
data30a$id <- seq(1, nrow(data30a))

#view(data30)

label_data30a <- data30a
number_of_bar <- nrow(label_data30a)
angle <- 90-360*(label_data30a$id-0.5)　/number_of_bar
label_data30a$hjust <- ifelse (angle < -90, 1, 0)
label_data30a$angle <- ifelse (angle < -90, angle+180, angle)

#view(label_data30)

p30a <- ggplot(data30a, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", fill = "skyblue", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  geom_text(data = label_data30a, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=0.5, angle= label_data30a$angle, inherit.aes = FALSE)

p30a

ggsave("mir30a.png")







mir17 <- read.delim("/Users/christinechang/Desktop/mir17_gene_all.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir17)

data17 <- data.frame(
  individual=mir17$label,
  group=mir17$category,
  value=mir17$score
)
#view(data17)

empty_bar <- 1

to_add <- matrix(NA, empty_bar, ncol(data17))
colnames(to_add) <- colnames(data17)
data17 <- rbind(data17, to_add)
data17$id <- seq(1, nrow(data17))

#view(data17)

label_data17 <- data17
number_of_bar <- nrow(label_data17)
angle <- 90-360*(label_data17$id-0.5)　/number_of_bar
label_data17$hjust <- ifelse (angle < -90, 1, 0)
label_data17$angle <- ifelse (angle < -90, angle+180, angle)

#view(label_data30)

p17 <- ggplot(data17, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", fill = "skyblue", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  geom_text(data = label_data17, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.5, angle= label_data17$angle, inherit.aes = FALSE)

p17

ggsave("mir17.png")




mir17a <- read.delim("/Users/christinechang/Desktop/mir17_all.txt", header = TRUE, stringsAsFactors = FALSE)
head(mir17a)

data17a <- data.frame(
  individual=mir17a$label,
  group=mir17a$category,
  value=mir17a$score
)
#view(data17a)

empty_bar <- 1

to_add <- matrix(NA, empty_bar, ncol(data17a))
colnames(to_add) <- colnames(data17a)
data17a <- rbind(data17a, to_add)
data17a$id <- seq(1, nrow(data17a))

#view(data17)

label_data17a <- data17a
number_of_bar <- nrow(label_data17a)
angle <- 90-360*(label_data17a$id-0.5)　/number_of_bar
label_data17a$hjust <- ifelse (angle < -90, 1, 0)
label_data17a$angle <- ifelse (angle < -90, angle+180, angle)

#view(label_data17a)

p17a <- ggplot(data17a, aes(x=as.factor(id), y=value, fill=group)) +
  geom_bar(stat="identity", fill = "skyblue", alpha=0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4),"cm")
  ) +
  coord_polar() +
  geom_text(data = label_data17a, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=1, angle= label_data17a$angle, inherit.aes = FALSE)

p17a

ggsave("mir17a.png")




