library(vegan)
library(dplyr)
setwd("/Users/yangjing/Desktop/cityu project/Picture/bubble plot/")
df <- read.csv("f0_f3.csv") %>% as.data.frame()

# 假设df是你的原始数据框
filenames <- c('DA', 'epi', 'GABA', 'Glu', 'Gln', 'His', 'KYNA', 'Kyn', 
               'norepi', 'Trp', 'Tyr', 'HIAA5', 'HTP5')

# 使用循环为每个Filename创建一个过滤后的数据框
for (name in filenames) {
  assign(name, df %>% filter(Filename == name))
}

# 假设你有一个列表，包含所有数据框
data_frames <- list(DA, epi, GABA, Glu, Gln, His, KYNA, Kyn, norepi, Trp, Tyr, HIAA5, HTP5)

# 定义一个函数来执行t.test并提取p值
run_t_test <- function(df, suffix) {
  # 提取所需的列
  col0 <- df[[paste0("F0_sol_", suffix)]]
  colpt0 <- df[[paste0("F0_", suffix)]]
  
  col1 <- df[[paste0("F1_sol_", suffix)]]
  colp1 <- df[[paste0("F1_P_", suffix)]]
  colt1 <- df[[paste0("F1_T_", suffix)]]
  
  col2 <- df[[paste0("F2_sol_", suffix)]]
  colp2 <- df[[paste0("F2_P_", suffix)]]
  colt2 <- df[[paste0("F2_T_", suffix)]]
  
  col3 <- df[[paste0("F3_sol_", suffix)]]
  colp3 <- df[[paste0("F3_P_", suffix)]]
  colt3 <- df[[paste0("F3_T_", suffix)]]
  
  # 运行t.test并提取p值
  p_values <- c(
    t.test(col0, colpt0, var.equal = TRUE)$p.value,
    t.test(col1, colp1, var.equal = TRUE)$p.value,
    t.test(col1, colt1, var.equal = TRUE)$p.value,
    t.test(col2, colp2, var.equal = TRUE)$p.value,
    t.test(col2, colt2, var.equal = TRUE)$p.value,
    t.test(col3, colp3, var.equal = TRUE)$p.value,
    t.test(col3, colt3, var.equal = TRUE)$p.value
  )
  
  return(p_values)
}

# 使用lapply运行函数并提取p值
p_values_m <- lapply(data_frames, run_t_test, suffix = "M")
p_values_f <- lapply(data_frames, run_t_test, suffix = "F")

# 定义列名
mcol_names <- c(
  "F0_sol_M vs F0_M",
  "F1_sol_M vs F1_P_M",
  "F1_sol_M vs F1_T_M",
  "F2_sol_M vs F2_P_M",
  "F2_sol_M vs F2_T_M",
  "F3_sol_M vs F3_P_M",
  "F3_sol_M vs F3_T_M"
)

fcol_names <- c(
  "F0_sol_F vs F0_F",
  "F1_sol_F vs F1_P_F",
  "F1_sol_F vs F1_T_F",
  "F2_sol_F vs F2_P_F",
  "F2_sol_F vs F2_T_F",
  "F3_sol_F vs F3_P_F",
  "F3_sol_F vs F3_T_F"
)

allrow_names <- c("DA", "epi", "GABA", "Glu", "Gln", "His", "KYNA", "Kyn", 
                  "norepi", "Trp", "Tyr", "HIAA5", "HTP5")

# 将p值转换为数据框
p_values_m_df <- do.call(rbind, p_values_m)
colnames(p_values_m_df) <- mcol_names
rownames(p_values_m_df) <- allrow_names

p_values_f_df <- do.call(rbind, p_values_f)
colnames(p_values_f_df) <- fcol_names
rownames(p_values_f_df) <- allrow_names

#改变dataframe格式
p_values_m_df <- p_values_m_df %>% as.data.frame() %>% 
  rownames_to_column(., var = "Chem") %>% as_tibble() %>% 
  pivot_longer(!`Chem`, names_to = "Sample", values_to = "pvalue") 

p_values_f_df <- p_values_f_df %>% as.data.frame() %>% 
  rownames_to_column(., var = "Chem") %>% as_tibble() %>% 
  pivot_longer(!`Chem`, names_to = "Sample", values_to = "pvalue")

# 使用case_when添加不同数量的星号
p_values_m_df <- p_values_m_df %>%
  mutate(significance = case_when(
    pvalue < 0.001 ~ "***",
    pvalue < 0.01 ~ "**",
    pvalue < 0.05 ~ "*",
    TRUE ~ ""
  )) %>% as.data.frame()

p_values_f_df <- p_values_f_df %>%
  mutate(significance = case_when(
    pvalue < 0.001 ~ "***",
    pvalue < 0.01 ~ "**",
    pvalue < 0.05 ~ "*",
    TRUE ~ ""
  )) %>% as.data.frame()

# 将M和F的结果合并到一个数据框中
combined_results <- cbind(p_values_m_df, p_values_f_df)

# 导出到CSV文件
write.csv(combined_results, "t.test_results2.csv")
