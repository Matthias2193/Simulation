list.of.packages <- c("ggplot2", "fitdistrplus", "MASS", "foreach", "doParallel", "gridExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(fitdistrplus)
library(ggplot2)
library(MASS)
library(foreach)
library(doParallel)

source("Helpers.R")

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


final_df <- rbind(result_df1, result_df2, result_df3, result_df4, result_df5, 
                  result_df8, result_df7, result_df8, result_df9, result_df10, 
                  result_df11, result_df12, bi_result_df1, bi_result_df2,
                  bi_result_df3, bi_result_df4)


plot(density(final_df[final_df$round_value == 5,]$bias_1))
lines(density(final_df[final_df$round_value == 10,]$bias_1))


plot(density(final_df[final_df$round_value == 5 & !is.na(final_df$bias_2),]$bias_2))
lines(density(final_df[final_df$round_value == 10 & !is.na(final_df$bias_2),]$bias_2))
 




#Old Tests
# bi_result_df3 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 10, 0.5)
# bi_result_df4 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 5, 0.5)
# bi_result_df5 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 10, 0.5)
# bi_result_df6 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 5, 0.5)
# bi_result_df7 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 10, 0.5)
# bi_result_df8 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 5, 0.5)


# bi_result_df11 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 10, 0.25)
# bi_result_df12 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 5, 0.25)
# bi_result_df13 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 10, 0.25)
# bi_result_df14 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 5, 0.25)
# bi_result_df15 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 10, 0.25)
# bi_result_df16 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 5, 0.25)
# bi_result_df17 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 10, 0.25)
# bi_result_df18 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 5, 0.25)


# bi_result_df21 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 10, 0.75)
# bi_result_df22 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 5, 0.75)
# bi_result_df23 <- parallel_bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 10, 0.75)
# bi_result_df24 <- parallel_bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 5, 0.75)
# bi_result_df25 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 10, 0.75)
# bi_result_df26 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 5, 0.75)
# bi_result_df27 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 10, 0.75)
# bi_result_df28 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 5, 0.75)
