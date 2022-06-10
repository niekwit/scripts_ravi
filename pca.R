
library(ggplot2)
library(ggrepel)

setwd("/home/niek/Documents/analyses/RNA-Seq/data-ravi/DESeq2/Hypoxia_vs_Normoxia")
pcaData <- load("PCA-data")

shapes <- c(15,16,17,15,16,17)

p<-ggplot(pcaData, aes(PC1, PC2, color=condition,shape=sample),
          show.legend = TRUE) +
  theme_bw(base_size = 20) +
  theme(legend.position = "right") +
  geom_point(size=10,
             shape = shapes) +
  scale_color_manual(values=rep(c("red", "navy"),3)) +
  xlab(paste0("PC1: ",51.9,"% variance")) +
  ylab(paste0("PC2: ",24.1,"% variance")) +
  coord_fixed(ratio=1.5)+




  geom_label_repel(size=5,
                   aes(x = PC1,
                       y = PC2,
                       label = sample), 
                   data = pcaData,
                   nudge_x =2.5,
                   nudge_y =1.5)

ggsave("PCA-plot.pdf", p)
