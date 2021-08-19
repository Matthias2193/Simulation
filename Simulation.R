source("Helpers.R")
library(tidyverse)

#Univariate Normal Distribution
result_df1 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 10), 0.5, 10)
result_df2 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 10), 0.5, 5)
result_df3 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 10), 1, 10)
result_df4 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 10), 1, 5)
result_df5 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 20), 0.5, 10, add_x2 = T)
result_df6 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 20), 0.5, 5, add_x2 = T)
result_df7 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 20), 1, 10, add_x2 = T)
result_df8 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 20), 1, 5, add_x2 = T)

#Univariate Lognormal Distribution
result_df9 <- uni_sim(5000, c(500,500), c(2, 2.5), c(0.5, 0.5), 0.5, 10, lognormal = T)
result_df10 <- uni_sim(5000, c(500,500), c(2, 2.5), c(0.5, 0.5), 0.5, 5, lognormal = T)
result_df11 <- uni_sim(5000, c(500,500), c(2, 2.5), c(0.5, 0.5), 1, 10, lognormal = T)
result_df12 <- uni_sim(5000, c(500,500), c(2, 2.5), c(0.5, 0.5), 1, 5, lognormal = T)

#Bivariate
bi_result_df1 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 10, 0.5)
bi_result_df2 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 5, 0.5)
bi_result_df3 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 10, -0.5)
bi_result_df4 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 5, -0.5)


combined_result_df <- rbind(result_df1, result_df2, result_df3, result_df4, result_df5, 
                  result_df6, result_df7, result_df8, result_df9, result_df10, 
                  result_df11, result_df12, bi_result_df1, bi_result_df2,
                  bi_result_df3, bi_result_df4)


grouped_results_tbl <- combined_result_df %>% 
  group_by(mean_0, mean_1, sd_0, sd_1, heap_perc, round_value, cor_value) %>% 
  summarise(mean_original_1 = mean(original_1),
            mean_rounded_1 = mean(rounded_1),
            mean_bias_1 = mean(bias_1),
            mean_original_2 = mean(original_2),
            mean_rounded_2 = mean(rounded_2),
            mean_bias_2 = mean(bias_2))
  
