library(ggplot2)
library(ggpubr)
library(dplyr)
setwd("/Users/yangjing/Desktop/bubble plot data/")

# measure data ------------------------------------------------------------

df <- read.csv("bubbleplot_df.csv")
df <- df %>%
  mutate(p = case_when(
    pvalue < 0.001 ~ "< 0.001",
    pvalue < 0.01 ~ "< 0.01",
    pvalue < 0.05 ~ "< 0.05",
    TRUE ~ "> 0.05"
  )) %>% as.data.frame()


# plot -------------------------------------------------------------

df$group <- factor(df$group, levels = c("5ppb","10ppb","50ppb","100ppb","200ppb"))

df_plot <- 
  ggplot()+
  geom_point(data = df,aes(group,chemical, size = abs(fold_change), fill = p),
             color = "grey85", shape = 21)+
  scale_fill_manual(values = c("navy","blue","deepskyblue","grey85"))+
  geom_point(data = df[which(df$Inc_Dec == "Inc"),],
             aes(group,chemical, size = abs(fold_change), color = p),
             shape = 16)+
  scale_color_manual(values = c("magenta","deeppink","palevioletred1","grey85"))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"),
        axis.text = element_text(color="black", size=15),
        axis.title=element_text(size=15),
        title = element_text(color="black", size=15),
        legend.title=element_text(size=15), 
        legend.text=element_text(size=12))+
  xlab("Chemical concentration")+
  ylab("Chemical")+
  ggtitle("Chemical content")+
  guides(size = guide_legend(title = "Fold change", order = 1),
         fill = guide_legend(title = "Fold change decrease", order = 2),
         color = guide_legend(title = "Fold change increase", order = 3))

df_plot