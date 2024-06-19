library(vegan)
library(dplyr)
setwd("/Users/yangjing/Desktop/bubble plot/")
df <- read.csv("adult.csv") %>% as.data.frame()


# measure data ------------------------------------------------------------

# Use a loop to create a filtered data frame for each Filename
filenames <- c('DA', 'epi', 'GABA', 'Glu', 'Gln', 'His', 'KYNA', 'Kyn', 
               'norepi', 'Trp', 'Tyr', 'HIAA5', 'HTP5')

for (name in filenames) {
  assign(name, df %>% filter(Filename == name))
}

# Use a list to contain all the data frames
data_frames <- list(DA, epi, GABA, Glu, Gln, His, KYNA, Kyn, norepi, Trp, Tyr, HIAA5, HTP5)


# t.test ------------------------------------------------------------------

run_t_test <- function(df, suffix) {
  
  # Extract the required columns
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
  
  # Run t.test and extract p-value
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

# Use lapply to run the function and extract the p-value
p_values_m <- lapply(data_frames, run_t_test, suffix = "M")
p_values_f <- lapply(data_frames, run_t_test, suffix = "F")


# measure results ---------------------------------------------------------

# Define column names
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

# Convert results to data frame
p_values_m_df <- do.call(rbind, p_values_m)
colnames(p_values_m_df) <- mcol_names
rownames(p_values_m_df) <- allrow_names

p_values_f_df <- do.call(rbind, p_values_f)
colnames(p_values_f_df) <- fcol_names
rownames(p_values_f_df) <- allrow_names

# Change the dataframe format
p_values_m_df <- p_values_m_df %>% as.data.frame() %>% 
  rownames_to_column(., var = "Chem") %>% as_tibble() %>% 
  pivot_longer(!`Chem`, names_to = "Sample", values_to = "pvalue") 

p_values_f_df <- p_values_f_df %>% as.data.frame() %>% 
  rownames_to_column(., var = "Chem") %>% as_tibble() %>% 
  pivot_longer(!`Chem`, names_to = "Sample", values_to = "pvalue")

# Add * using case_when
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


# combine data frame and get results --------------------------------------

combined_results <- cbind(p_values_m_df, p_values_f_df)
write.csv(combined_results, "t.test_results2.csv")
