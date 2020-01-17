library(ggplot2)
library(cowplot)
data = read.csv("lipinski_results.csv", header = TRUE)

data$Activity <- factor(data$Activity, levels = c("Inactive", "Active"))

p <- ggplot(data, aes(MW, ALogP))
p <- p + geom_point(aes(colour = factor(Activity)), size = 3, alpha = 0.3) + xlim(0,800) + ylim(-3.5, 7.5) + theme_bw()
  geom_point(shape = 1,size = 0,colour = "lavenderblush4")
p <- p + theme(aspect.ratio=1, legend.position = ("none"),
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
               panel.border = element_rect(linetype = "solid",
                                           colour = "black", fill = NA, size = 1),
               axis.text.x = element_text(colour = "black", size = 15),
               axis.text.y = element_text(colour = "black", size = 15),
               plot.margin = unit(c(1, 1, 1, 1), "cm"),
               axis.title.x = element_text(colour = "black", size = 18, face = "bold", margin = margin(t=15)),
               axis.title.y = element_text(colour = "black", size = 18, face = "bold", margin = margin(r=15))
)
ggsave("chemical_space.pdf", width = 5, height = 5)
print(p)
