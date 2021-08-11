# In this script multiple different plots are created
#Requires results in "final_df" as produced by the "Simulation.R" scipt

library(ggplot2)
library(gridExtra)

plot_data <- final_df
plot_data$round_value <- as.factor(plot_data$round_value)
ggplot(plot_data,aes(x=bias_1, fill=round_value)) + geom_density(alpha=0.25) + ggtitle("Bias Density by Rounding Vlaue") +
  ylab("Density") + labs(fill='Rounding Value')
ggplot(plot_data,aes(x=round_value, y=bias_1, fill=round_value)) + geom_boxplot(show.legend = FALSE) +
  xlab("Rounding Value") + ylab("Bias")

no_log <- final_df[final_df$sd_0 != 0.5,]
no_log$round_value <- as.factor(no_log$round_value)
ggplot(no_log,aes(x=bias_1, fill=round_value)) + geom_density(alpha=0.25) + ggtitle("Bias Density by Rounding Value Without Lognormal") +
  ylab("Density") + labs(fill='Rounding Value')
ggplot(no_log,aes(x=round_value, y=bias_1, fill=round_value)) + geom_boxplot(show.legend = FALSE) +
  xlab("Rounding Value") + ylab("Bias")

log_df <- final_df[final_df$sd_0 == 0.5,]
log_df$round_value <- as.factor(log_df$round_value)
ggplot(log_df,aes(x=bias_1, fill=round_value)) + geom_density(alpha=0.25) + 
  ggtitle("Bias Density by Rounding Value") +  ylab("Density") + xlab("Bias X1") +
  labs(fill='Round Value')
ggplot(log_df,aes(x=round_value, y=bias_1, fill=round_value)) + geom_boxplot(show.legend = FALSE) +
  xlab("Rounding Value") + ylab("Bias")


temp_df <- final_df[is.na(final_df$cor_value) & final_df$sd_1 != 20,]
temp_df$log <- ifelse(temp_df$sd_0 == 0.5, "Lognormal", "Normal")
temp_df$log <- as.factor(temp_df$log)
ggplot(temp_df[temp_df$round_value == 5,],aes(x=bias_1, fill=log)) + geom_density(alpha=0.25) + 
  ggtitle("Bias Density by Distribution with Rounding Value = 5") +  ylab("Density") + 
  labs(fill='Distribution')
ggplot(temp_df[temp_df$round_value == 10,],aes(x=bias_1, fill=log)) + geom_density(alpha=0.25) + 
  ggtitle("Bias Density by Distribution with Rounding Value = 10") + ylab("Density") + 
  labs(fill='Distribution')
p1 <- ggplot(temp_df[temp_df$round_value == 5,],aes(x=log, y=bias_1, fill=log)) + geom_boxplot(show.legend = FALSE) +
  xlab("Distribution") + ylab("Bias") + ggtitle("Rounding Value 5")
p2 <- ggplot(temp_df[temp_df$round_value == 10,],aes(x=log, y=bias_1, fill=log)) + geom_boxplot(show.legend = FALSE) +
  xlab("Distribution") + ylab("Bias") + ggtitle("Rounding Value 10")
grid.arrange(p1, p2, ncol=2)
ggplot(temp_df,aes(x=log, y=bias_1, fill=log)) + geom_boxplot(show.legend = FALSE) +
  xlab("Distribution") + ylab("Bias")


cor_df <- final_df[!is.na(final_df$cor_value),]
cor_df$cor_value <- as.factor(cor_df$cor_value)
ggplot(cor_df,aes(x=bias_1, fill=cor_value)) + geom_density(alpha=0.25) + ggtitle("Bias Density by Correlation Value") +
  ylab("Density") + labs(fill='Correlation Value') + xlab("Bias X1")
ggplot(cor_df,aes(x=bias_2, fill=cor_value)) + geom_density(alpha=0.25) + ggtitle("Bias Density by Correlation Value") +
  ylab("Density") + labs(fill='Correlation Value') + xlab("Bias X2")
p1 <- ggplot(cor_df,aes(x=cor_value, y=bias_1, fill=cor_value)) + geom_boxplot(show.legend = FALSE) + 
  xlab("Correlation Value") + ylab("Bias X1")
p2 <- ggplot(cor_df,aes(x=cor_value, y=bias_2, fill=cor_value)) + geom_boxplot(show.legend = FALSE) + 
  xlab("Correlation Value") + ylab("Bias X2")
grid.arrange(p1, p2, ncol=2)

temp_df <- final_df[is.na(final_df$cor_value) & final_df$sd_0 == 10,]
temp_df$sd_1 <- as.factor(temp_df$sd_1)
ggplot(temp_df,aes(x=bias_1, fill=sd_1)) + geom_density(alpha=0.25) + ggtitle("Bias Density by Standard Deviation of X when Y = 1") +
  ylab("Density") + xlab("Bias")
ggplot(temp_df,aes(x=sd_1, y=bias_1, fill=sd_1)) + geom_boxplot(show.legend = FALSE) + xlab("Standard Deviation of X given Y = 1") +
  ylab("Bias") 


temp_df <- final_df
temp_df$heap_perc <- as.factor(temp_df$heap_perc)
ggplot(temp_df,aes(x=bias_1, fill=heap_perc)) + geom_density(alpha=0.25) + ggtitle("Bias Density by Heaping Percentage") +
  ylab("Density") + xlab("Bias") + labs(fill="Heaping Percentage")
ggplot(temp_df,aes(x=heap_perc, y=bias_1, fill=heap_perc)) + geom_boxplot(show.legend = FALSE) + xlab("Heaping Percentage") +
  ylab("Bias") 



log_rand <- c(rlnorm(500, mean = 2, sd = 0.5), rlnorm(500, mean = 2.5, sd = 0.5))
log_heaped <- log_rand
round_mult <- 10
for(s in sample(seq(1,1000), 500)){
  if(log_heaped[s] >= 5/2){
    if(log_heaped[s] < 7.5){
      log_heaped[s] <- round(log_heaped[s]/5)*5
    } else{
      log_heaped[s] <- round(log_heaped[s]/round_mult)*round_mult
    }
  }
}
log_heaped2 <- log_rand
for(s in sample(seq(1,1000), 1000)){
  if(log_heaped2[s] >= 5/2){
    if(log_heaped2[s] < 7.5){
      log_heaped2[s] <- round(log_heaped2[s]/5)*5
    } else{
      log_heaped2[s] <- round(log_heaped2[s]/round_mult)*round_mult
    }
  }
}
plot_df <- data.frame(x = c(log_rand, log_heaped, log_heaped2),
                      heap_perc = c(rep(0,1000),rep(50,1000), rep(100, 1000))) 


p1 <- ggplot(plot_df[plot_df$heap_perc == 0,], aes(x=x)) +  
  geom_histogram(alpha=0.8, fill = "white", color = "red") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(0, 70, by = 5))

p2 <- ggplot(plot_df[plot_df$heap_perc == 50,], aes(x=x)) +  
  geom_histogram(alpha=0.8, fill =  "white", color = "blue") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(0, 70, by = 5))

p3 <- ggplot(plot_df[plot_df$heap_perc == 100,], aes(x=x)) +  
  geom_histogram(alpha=0.8, fill = "white", color = "green") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(0, 70, by = 5))
 
grid.arrange(p1, p2, p3, ncol=3)


log_heaped <- log_rand
round_mult <- 5
for(s in sample(seq(1,1000), 500)){
  if(log_heaped[s] >= 5/2){
    if(log_heaped[s] < 7.5){
      log_heaped[s] <- round(log_heaped[s]/5)*5
    } else{
      log_heaped[s] <- round(log_heaped[s]/round_mult)*round_mult
    }
  }
}
log_heaped2 <- log_rand
for(s in sample(seq(1,1000), 1000)){
  if(log_heaped2[s] >= 5/2){
    if(log_heaped2[s] < 7.5){
      log_heaped2[s] <- round(log_heaped2[s]/5)*5
    } else{
      log_heaped2[s] <- round(log_heaped2[s]/round_mult)*round_mult
    }
  }
}
plot_df <- data.frame(x = c(log_rand, log_heaped, log_heaped2),
                      heap_perc = c(rep(0,1000),rep(50,1000), rep(100, 1000)))


p1 <- ggplot(plot_df[plot_df$heap_perc == 0,], aes(x=x)) +  
  geom_histogram(alpha=0, fill = "white", color = "red") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(0, 70, by = 5))

p2 <- ggplot(plot_df[plot_df$heap_perc == 50,], aes(x=x)) +  
  geom_histogram(alpha=0, fill =  "white", color = "blue") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(0, 70, by = 5))

p3 <- ggplot(plot_df[plot_df$heap_perc == 100,], aes(x=x)) +  
  geom_histogram(alpha=0, fill = "white", color = "green") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(0, 70, by = 5))

grid.arrange(p1, p2, p3, ncol=3)

norm_rand <- c(rnorm(500, mean = 90, sd = 10), rnorm(500, mean = 100, sd = 10))
norm_heaped <- norm_rand
round_mult <- 10
for(s in sample(seq(1,1000),500)){
  norm_heaped[s] <- round(norm_heaped[s]/round_mult)*round_mult
}
norm_heaped2 <- norm_rand
for(s in seq(1,1000)){
  norm_heaped2[s] <- round(norm_heaped2[s]/round_mult)*round_mult
}
plot_df <- data.frame(x = c(norm_rand, norm_heaped, norm_heaped2),
                      heap_perc = c(rep(0,1000),rep(50,1000), rep(100, 1000)))


p1 <- ggplot(plot_df[plot_df$heap_perc == 0,], aes(x=x)) +  
  geom_histogram(alpha=0, fill = "white", color = "red") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(0, 70, by = 5))

p2 <- ggplot(plot_df[plot_df$heap_perc == 50,], aes(x=x)) +  
  geom_histogram(alpha=0, fill =  "white", color = "blue") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(0, 70, by = 5))

p3 <- ggplot(plot_df[plot_df$heap_perc == 100,], aes(x=x)) +  
  geom_histogram(alpha=0, fill = "white", color = "green") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(0, 70, by = 5))


grid.arrange(p1, p2, p3, ncol=3)

norm_heaped <- norm_rand
round_mult <- 5
for(s in sample(seq(1,1000),500)){
  norm_heaped[s] <- round(norm_heaped[s]/round_mult)*round_mult
}
norm_heaped2 <- norm_rand
for(s in seq(1,1000)){
  norm_heaped2[s] <- round(norm_heaped2[s]/round_mult)*round_mult
}
plot_df <- data.frame(x = c(norm_rand, norm_heaped, norm_heaped2),
                      heap_perc = c(rep(0,1000),rep(50,1000), rep(100, 1000)))


p1 <- ggplot(plot_df[plot_df$heap_perc == 0,], aes(x=x)) +  
  geom_histogram(alpha=0.8, fill = "white", color = "red") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(60, 135, by = 5))

p2 <- ggplot(plot_df[plot_df$heap_perc == 50,], aes(x=x)) +  
  geom_histogram(alpha=0.8, fill =  "white", color = "blue") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(60, 135, by = 5))

p3 <- ggplot(plot_df[plot_df$heap_perc == 100,], aes(x=x)) +  
  geom_histogram(alpha=0.8, fill = "white", color = "green") + 
  ggtitle("") + ylab("Count") + scale_x_continuous(breaks = seq(60, 135, by = 5))


grid.arrange(p1, p2, p3, ncol=3)
