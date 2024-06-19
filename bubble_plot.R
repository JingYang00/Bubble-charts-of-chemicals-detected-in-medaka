library(ggplot2)
library(ggpubr)
library(dplyr)
setwd("/Users/yangjing/Desktop/cityu project/Picture/bubble plot/")
df <- read.csv("bubbleplot_df.csv")

# measure data ------------------------------------------------------------

df <- df %>%
  mutate(p = case_when(
    pvalue < 0.001 ~ "< 0.001",
    pvalue < 0.01 ~ "< 0.01",
    pvalue < 0.05 ~ "< 0.05",
    TRUE ~ "> 0.05"
  )) %>% as.data.frame()

larvae_df <- filter(df, sex == "larvae")
male_df <- filter(df, sex == "male")
female_df <- filter(df, sex == "female")


# larvae plot -------------------------------------------------------------

larvae_df$group <- factor(larvae_df$group, levels = c("5ppb","10ppb","50ppb","100ppb","200ppb"))

larvae_plot <- 
  ggplot()+
  geom_point(data = larvae_df,aes(group,chemical, size = abs(fold_change), fill = p),
             color = "grey85", shape = 21)+
  scale_fill_manual(values = c("blue","deepskyblue","grey85"))+
  geom_point(data = larvae_df[which(larvae_df$Pos_Neg == "Pos"),],
             aes(group,chemical, size = abs(fold_change), color = p),
             shape = 16)+
  scale_color_manual(values = c("deeppink","palevioletred1","grey85"))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"),
        axis.text = element_text(color="black", size=15),
        axis.title=element_text(size=15),
        title = element_text(color="black", size=15),
        legend.position = "none")+
  xlab("Chemical concentration")+
  ylab("Chemical")+
  ggtitle("Larvae chemical content")+
  guides(size = guide_legend(title = "Fold change", order = 1),
         fill = guide_legend(title = "Fold change decrease", order = 2),
         col = guide_legend(title = "Fold change increase", order = 3))

larvae_plot

# female plot -------------------------------------------------------------

female_df <- filter(female_df, !chemical == "Kyn")
female_df <- filter(female_df, !chemical == "5-HIAA")
female_df <- filter(female_df, !chemical == "5-HTP")

female_plot <- 
  ggplot()+
  geom_point(data = female_df,aes(group,chemical, size = abs(fold_change), fill = p),
             color = "grey85", shape = 21)+
  scale_fill_manual(values = c("navy","blue","deepskyblue","grey85"))+
  geom_point(data = female_df[which(larvae_df$Pos_Neg == "Pos"),],
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
  ggtitle("Female chemical content")+
  guides(size = guide_legend(title = "Fold change", order = 1),
         fill = guide_legend(title = "Fold change decrease", order = 2),
         col = guide_legend(title = "Fold change increase", order = 3))
female_plot

# male plot ---------------------------------------------------------------

male_df <- filter(male_df, !chemical == "Kyn")
male_df <- filter(male_df, !chemical == "5-HIAA")
male_df <- filter(male_df, !chemical == "5-HTP")

male_plot <- 
  ggplot()+
  geom_point(data = male_df,aes(group,chemical, size = abs(fold_change), fill = p),
             color = "grey85", shape = 21)+
  scale_fill_manual(values = c("blue","deepskyblue","grey85"))+
  geom_point(data = male_df[which(larvae_df$Pos_Neg == "Pos"),],
             aes(group,chemical, size = abs(fold_change), color = p),
             shape = 16)+
  scale_color_manual(values = c("deeppink","palevioletred1","grey85"))+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(fill=NA,color="black", size=1.5, linetype="solid"),
        axis.text = element_text(color="black", size=15),
        axis.title=element_text(size=15),
        title = element_text(color="black", size=15),
        legend.position = "none")+
  xlab("Chemical concentration")+
  ylab("Chemical")+
  ggtitle("Male chemical content")+
  guides(size = guide_legend(title = "Fold change", order = 1),
         fill = guide_legend(title = "Fold change decrease", order = 2),
         col = guide_legend(title = "Fold change increase", order = 3))

male_plot

ggarrange(larvae_plot,male_plot,female_plot, nrow = 3,common.legend = T, legend = "right")

