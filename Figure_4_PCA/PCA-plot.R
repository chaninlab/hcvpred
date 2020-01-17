library(dplyr)
library(ggplot2)
library(cowplot)
library(reshape2)
library(ggrepel)

df = read.csv("score.csv", header = TRUE)
df$set <- factor(df$set,levels = c("train", "test"))  ### Train and test are in Pink and  Cyan colour

p = ggplot(df, aes(x=df$Dim.1, y=df$Dim.2, colour=set, label=X))+
  geom_point() + #geom_text_repel(aes(label=X)) +       # to remove the label
  theme_bw() + xlab("PC1") + ylab("PC2") +
  theme(aspect.ratio=1,
        axis.title.x = element_text(size = 15, face = "bold", colour = "black"),
        axis.title.y = element_text(size = 15, face = "bold", colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, colour = "black"),
        legend.position="none")
print(p)

ggsave("PCA_Pubchem.pdf", width = 20, height = 20, units = "cm")
