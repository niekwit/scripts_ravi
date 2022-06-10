library(ggplot2)
library(reshape2)
library(scales)
library(viridisLite)

setwd("/home/niek/Documents/analyses/RNA-Seq/data-ravi/DESeq2/Hypoxia_vs_Normoxia")


genes <- c("HK2","ALDOC","EGLN3","SLC2A1","SLC2A3","VEGFA","ANGPTL4")

df <- read.csv("DESeq-output.csv")

df <- df[df$SYMBOL %in% genes, ]
df$minus.log.pvalue <- -log10(df$padj)
df$condition <- "Hypoxia vs. normoxia"


p <- ggplot(df, aes(x = `condition`,
                    y = `SYMBOL`,
                    fill = `log2FoldChange`,
                    size = `minus.log.pvalue`)) +
  geom_point(shape = 21,
             colour = "black") +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.width=unit(1.25,"cm"),
        legend.title=element_text(size=16)) +
  scale_fill_viridis_c(limits = c(0,7),
                       breaks = c(0,1,2,3,4,5,6,7),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black",
                                              barheight = 13.5)) +
  scale_size_continuous(range=c(7,20),
                        limits = c(1,80),
                        breaks = c(5,10,30,50,70)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_discrete(position="top") +
  labs(size = "-log(Adj. P value)") +
  scale_y_discrete(limits=rev) 

ggsave("bubble_plot.pdf", p,
       width = 5,
       height = 7.5)
