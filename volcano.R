

setwd("/home/niek/Documents/analyses/RNA-Seq/data-ravi/DESeq2/Hypoxia_vs_Normoxia")


#library(GO.db)
#library(org.Hs.eg.db) 
library(dplyr)
library(ggplot2)
library(ggrepel)

genes <- c("HK2","ALDOC","EGLN3","SLC2A1","SLC2A3","VEGFA","ANGPTL4") #enter GO you are interested in

df <- read.csv("DESeq-output.csv")






## GO:0006006 = glucose metabolic process
## GO:0001525 = angiogenesis
## GO:0008137 = NADH dehydrogenase (ubiquinone) activity


#Cut off values for log2(FC) and log10(p value)
FCcutoff <- 0.5
Pcutoff <- 3

df$minus.log.pvalue <- -log10(df$padj)



p <- ggplot(df, aes(x = `log2FoldChange`, 
                     y = `minus.log.pvalue`)) +
        theme_bw() +
        theme(text=element_text(size=20)) +
        geom_point(df, 
                   alpha = 0.4,
                   mapping = aes(x = `log2FoldChange`,
                                 y = `minus.log.pvalue`,
                                 color = `minus.log.pvalue` < Pcutoff |
                                         `log2FoldChange` < FCcutoff &
                                         `log2FoldChange` > -FCcutoff)) + 
        scale_color_manual(values = c("TRUE" = "black", 
                                      "FALSE" = "red")) +
        xlab("log2(fold change)") +
        ylab("-log10(adj. p value)") +
        geom_hline(yintercept=Pcutoff, 
                   linetype="dashed", 
                   color = "red") +
        geom_vline(xintercept=c(-FCcutoff,FCcutoff), 
                   linetype="dashed", 
                   color = "red") +
        guides(color = "none",
               fill = "none") +
        xlim(-10, 10) +
        geom_label_repel(size=5,
                         aes(x = `log2FoldChange`,
                             y = `minus.log.pvalue`,
                             label = SYMBOL), 
                         data = df[df$SYMBOL %in% genes, ],
                         nudge_x = 1,
                         nudge_y = 1)


ggsave(plot = p,
       filename = "Volcano_plot.pdf",
       width = 8,
       height = 10,
       useDingbats = FALSE) #prevents artefacts in Illustrator     

