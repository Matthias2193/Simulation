list.of.packages <- c("ggplot2", "fitdistrplus", "MASS", "foreach", "doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(fitdistrplus)
library(ggplot2)
library(MASS)
library(foreach)
library(doParallel)

source("Helpers.R")



# mu_x <- 5
# sd_x <- 5
# 
# mu <- log(mu_x^2/(sqrt(mu_x^2+sd_x^2)))
# sd <- log(1+(sd_x^2/mu_x^2))
# test_dist <- rlnorm(10000,mu,sd)
# plot(density(test_dist))




#Univariate
start_time <- Sys.time()
result_df1 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 10), 0.5, 10)
result_df2 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 10), 0.5, 5)
result_df3 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 20), 0.5, 10)
result_df4 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 20), 0.5, 5)
result_df5 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 10), 1, 10)
result_df6 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 10), 1, 5)
result_df7 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 20), 1, 10)
result_df8 <- uni_sim(5000, c(500,500), c(90, 100), c(10, 20), 1, 5)
end_time <- Sys.time()
difftime(end_time,start_time)
plot_results(bi_result_df5,T)

temp_df <- result_df8

mean(temp_df$original)
mean(temp_df$rounded)
mean(temp_df$bias)
save(bi_result_df22, file="bi_result_df22.Rda")



#Bivariate
bi_result_df1 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 10, 0.5)
bi_result_df2 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 5, 0.5)
bi_result_df3 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 10, 0.5)
bi_result_df4 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 5, 0.5)
bi_result_df5 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 10, 0.5)
bi_result_df6 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 5, 0.5)
bi_result_df7 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 10, 0.5)
bi_result_df8 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 5, 0.5)

bi_result_df11 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 10, 0.25)
bi_result_df12 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 5, 0.25)
# bi_result_df13 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 10, 0.25)
# bi_result_df14 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 5, 0.25)
# bi_result_df15 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 10, 0.25)
# bi_result_df16 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 5, 0.25)
# bi_result_df17 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 10, 0.25)
# bi_result_df18 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 5, 0.25)

start_time <- Sys.time()
bi_result_df21 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 10, 0.75)
end_time <- Sys.time()
regular_time <- difftime(end_time,start_time)

start_time <- Sys.time()
bi_result_df22 <- parallel_bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 0.5, 5, 0.75)
end_time <- Sys.time()
parallel_time <- difftime(end_time,start_time)

# bi_result_df23 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 10, 0.75)
# bi_result_df24 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 0.5, 5, 0.75)
# bi_result_df25 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 10, 0.75)
# bi_result_df26 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 10), 1, 5, 0.75)
# bi_result_df27 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 10, 0.75)
# bi_result_df28 <- bi_sim(5000, c(500,500), c(90, 90, 100), c(10, 10, 20), 1, 5, 0.75)



temp_df <- bi_result_df22

mean(temp_df$original_1)
mean(temp_df$rounded_1)
mean(temp_df$bias_1)
mean(temp_df$original_2)
mean(temp_df$rounded_2)
mean(temp_df$bias_2)
