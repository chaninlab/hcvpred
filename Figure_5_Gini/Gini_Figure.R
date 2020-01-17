library(reshape2)
library(ggplot2)
library(cowplot)

data= read.csv("Gini_PubchemFingerprinter.csv", header = TRUE)

data_melt <- melt(data[1:20,], id.vars = "feature")
data_melt$feature <- factor(data_melt1$feature)
set.seed(10)
plot <- ggplot(data_melt, aes(x = reorder(feature, value, FUN = mean), y = value)) +
  geom_boxplot(fill = "#F8766D", colour = "black", alpha = 0.5) +
  ggtitle("Pubchem") +
  theme_bw() + xlab("") + ylab("Gini index") + coord_flip() + theme(
    plot.title = element_text(hjust = 0.5, face="bold", size = 18),
    axis.text.y = element_text(size = 14, colour = "black"),
    axis.text.x = element_text(size = 14, colour = "black"),
    panel.border = element_rect(linetype = "solid", colour = "black", fill = NA, size = 1),
    axis.title.x = element_text(size = 16, face = "bold", colour = "black")
  )
print(plot)
