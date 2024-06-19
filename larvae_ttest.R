library(vegan)
library(dplyr)
setwd("/Users/yangjing/Desktop/cityu project/Picture/bubble plot/")
df <- read.csv("larvae.csv") %>% as.data.frame()

# 假设df是你的原始数据框
filenames <- c('DA', 'epi', 'GABA', 'Glu', 'Gln', 'His', 'KYNA', 'Kyn', 
               'norepi', 'Trp', 'Tyr', 'HIAA5', 'HTP5', 'IDOPA')

# 使用循环为每个Filename创建一个过滤后的数据框
for (name in filenames) {
  assign(name, df %>% filter(Filename == name))
}

# 创建数据框列表
data_frames <- list(DA, epi, GABA, Glu, Gln, His, KYNA, Kyn, 
                    norepi, Trp, Tyr, HIAA5, HTP5, IDOPA)  # 请确保这里列出了所有的数据框

# 定义列名
ppb_columns <- c("ppb5", "ppb10", "ppb50", "ppb100", "ppb200")

# 初始化结果列表
results <- list()

# 遍历每个数据框
for (i in seq_along(data_frames)) {
  df <- data_frames[[i]]
  con <- df$control  # 提取控制组数据
  
  # 遍历每个列名进行计算
  for (ppb in ppb_columns) {
    # 从DA中提取当前循环中的变量
    current_ppb <- df[[ppb]]
    
    # 执行t检验，假设方差相等
    t_test_result <- t.test(con, current_ppb, var.equal = TRUE)
    print(t_test_result)  # 打印t检验结果
  
    # 保存结果
    results[[paste("chem", i, ppb, "t_test", sep = "_")]] <- t_test_result
  }
}

# 初始化一个空的数据框来存储结果
final_results <- data.frame()

# 遍历结果列表，提取关键信息
for (name in names(results)) {
  test_result <- results[[name]]
  p_value <- test_result$p.value
  # 创建一个临时数据框存储结果
  temp_df <- data.frame(
    t.test_Group=name,
    pvalue=p_value
  )
  # 将临时数据框与最终结果绑定
  final_results <- rbind(final_results, temp_df)
}

# 使用case_when添加不同数量的星号
final_results <- final_results %>%
  mutate(significance = case_when(
    pvalue < 0.001 ~ "***",
    pvalue < 0.01 ~ "**",
    pvalue < 0.05 ~ "*",
    TRUE ~ ""
  )) %>% as.data.frame()

# 查看结果
final_results

# 重复每个元素5次
repeated_filenames <- rep(filenames, each = 5)
final_results <- mutate(final_results, Chem = repeated_filenames)


# 导出结果
write.csv(final_results, "t.test_results1.csv")
