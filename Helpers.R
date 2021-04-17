list.of.packages <- c("ggplot2", "fitdistrplus", "MASS", "foreach", "doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(fitdistrplus)
library(ggplot2)
library(MASS)
library(foreach)
library(doParallel)

uni_sim_helper <- function(n_0, n_1, m_0, m_1, sd_0, sd_1, heap_perc, round_mult, x){
  set.seed(x)
  x <- c(rnorm(n_1, mean = m_1, sd = sd_1), rnorm(n_0, mean = m_0, sd = sd_0))
  #x <- c(rlnorm(n_1, meanlog = m_1, sdlog = sd_1), rlnorm(n_0, meanlog = m_0, sdlog = sd_0))
  y <- c(rep(1,n_1),rep(0,n_0))
  
  #Create data frame
  data <- data.frame(x,y)
  
  #Logistic regression for "true" values
  model <- glm(y ~., family = binomial, data = data)
  
  #Save the coefficient
  original <- model$coefficients[["x"]]
  
  #Randomly round 50% of the x values to the nearest 10s
  for(s in sample(seq(1,(n_1 + n_0)),heap_perc * (n_1+n_0))){
    data[s,]["x"] <- round(data[s,]["x"]/round_mult)*round_mult
  }
  
  #Logistic regression for the rounded values
  model <- glm(y ~., family = binomial, data = data)
  
  #Save the coefficient
  rounded <- model$coefficients[["x"]]
  
  bias <- (original[length(original)]-rounded[length(rounded)]) * 100/original[length(original)]
  
  return(c(original, rounded, bias))
  
}

bi_sim_helper <- function(n_0, n_1, m_0, m_1, m_2, sd_0, sd_1, sd_2, heap_perc, round_mult, cor_value, x){
  set.seed(x)
  Sig <- (matrix(cor_value, nrow=2, ncol=2) + diag(2)*(1-cor_value)) * sd_2^2
  data <- data.frame(mvrnorm(n = n_1, mu = c(m_2, m_2), Sigma = Sig))
  Sig <- (matrix(cor_value, nrow=2, ncol=2) + diag(2)*(1-cor_value)) * sd_1^2
  data <- rbind(data,data.frame(mvrnorm(n = n_0, mu = c(m_0, m_0), Sigma = Sig)))
  y <- c(rep(1,n_1),rep(0,n_0))
  
  #Create data frame
  data <- cbind(data,y)
  
  #Logistic regression for "true" values
  model <- glm(y ~., family = binomial, data = data)
  
  #Save the coefficient
  original_1 <- model$coefficients[["X1"]]
  original_2 <- model$coefficients[["X2"]]
  #Randomly round 50% of the x values to the nearest 10s
  for(s in sample(seq(1,(n_1 + n_0)),heap_perc * (n_1+n_0))){
    data[s,]["X1"] <- round(data[s,]["X1"]/round_mult)*round_mult
  }
  
  #Logistic regression for the rounded values
  model <- glm(y ~., family = binomial, data = data)
  
  #Save the coefficient
  rounded_1 <- model$coefficients[["X1"]]
  rounded_2 <- model$coefficients[["X2"]]
  
  bias_1 <- (original_1[length(original_1)]-rounded_1[length(rounded_1)]) * 100/original_1[length(original_1)]
  bias_2 <- (original_2[length(original_2)]-rounded_2[length(rounded_2)]) * 100/original_2[length(original_2)]
  
  return(c(original_1, rounded_1, bias_1, original_2, rounded_2, bias_2))
  
}

parallel_uni_sim <- function(n_rep, n_sample, mu, sd, heap_perc, round_mult, remain_cores = 1){
  n_0 <- n_sample[1]
  n_1 <- n_sample[2]
  
  m_0 <- mu[1]
  m_1 <- mu[2]
  
  sd_0 <- sd[1]
  sd_1 <- sd[2]
  numCores <- detectCores()
  cl <- makePSOCKcluster(numCores - remain_cores)
  registerDoParallel(cl)
  results <- foreach(x = 1:n_rep) %dopar%{
    source("Helpers.R")
    return(uni_sim_helper(n_0, n_1, m_0, m_1, sd_0, sd_1, heap_perc, round_mult, x))
  }
  stopCluster(cl)
  results_df <- as.data.frame(t(matrix(unlist(results), nrow=length(unlist(results[1])))))
  colnames(results_df) <- c("original", "rounded", "bias")
  results_df$differences <- (results_df$original - results_df$rounded)
  return(results_df)
}


parallel_bi_sim <- function(n_rep, n_sample, mu, sd, heap_perc, round_mult, cor_value, remain_cores = 1){
  n_0 <- n_sample[1]
  n_1 <- n_sample[2]
  
  m_0 <- mu[1]
  m_1 <- mu[2]
  m_2 <- mu[3]
  
  sd_0 <- sd[1]
  sd_1 <- sd[2]
  sd_2 <- sd[3]
  numCores <- detectCores()
  cl <- makePSOCKcluster(numCores - remain_cores)
  registerDoParallel(cl)
  results <- foreach(x = 1:n_rep) %dopar%{
    source("Helpers.R")
    return(bi_sim_helper(n_0, n_1, m_0, m_1, m_2, sd_0, sd_1, sd_2, heap_perc, round_mult, cor_value, x))
  }
  stopCluster(cl)
  results_df <- as.data.frame(t(matrix(unlist(results), nrow=length(unlist(results[1])))))
  colnames(results_df) <- c("original_1", "rounded_1", "bias_1", "original_2", "rounded_2", "bias_2")
  results_df$differences_1 <- (results_df$original_1 - results_df$rounded_1)
  results_df$differences_2 <- (results_df$original_2 - results_df$rounded_2)
  return(results_df)
}

uni_sim <- function(n_rep, n_sample, mu, sd, heap_perc, round_mult){
  original <- c()
  rounded <- c()
  bias <- c()
  
  n_0 <- n_sample[1]
  n_1 <- n_sample[2]
  
  m_0 <- mu[1]
  m_1 <- mu[2]
  
  sd_0 <- sd[1]
  sd_1 <- sd[2]
  
  for(r in seq(1, n_rep)){
    #Sample x
    x <- c(rnorm(n_1, mean = m_1, sd = sd_1), rnorm(n_0, mean = m_0, sd = sd_0))
    #x <- c(rlnorm(n_1, meanlog = m_1, sdlog = sd_1), rlnorm(n_0, meanlog = m_0, sdlog = sd_0))
    y <- c(rep(1,n_1),rep(0,n_0))
    
    #Create data frame
    data <- data.frame(x,y)
    
    #Logistic regression for "true" values
    model <- glm(y ~., family = binomial, data = data)
    
    #Save the coefficient
    original <- c(original,model$coefficients[["x"]])
    
    #Randomly round 50% of the x values to the nearest 10s
    for(s in sample(seq(1,(n_1 + n_0)),heap_perc * (n_1+n_0))){
      data[s,]["x"] <- round(data[s,]["x"]/round_mult)*round_mult
    }
    
    #Logistic regression for the rounded values
    model <- glm(y ~., family = binomial, data = data)
    
    #Save the coefficient
    rounded <- c(rounded,model$coefficients[["x"]])
    
    bias <- c(bias,(original[length(original)]-rounded[length(rounded)]) * 100/original[length(original)])
    print(r)
  }
  
  differences <- original - rounded
  
  return(data.frame(cbind(original, rounded, bias, differences)))
}



bi_sim <- function(n_rep, n_sample, mu, sd, heap_perc, round_mult, cor_value){
  original_1 <- c()
  original_2 <- c()
  rounded_1 <- c()
  rounded_2 <- c()
  bias_1 <- c()
  bias_2 <- c()
  
  n_0 <- n_sample[1]
  n_1 <- n_sample[2]
  
  m_0 <- mu[1]
  m_1 <- mu[2]
  m_2 <- mu[3]
  
  sd_0 <- sd[1]
  sd_1 <- sd[2]
  sd_2 <- sd[3]
  
  for(r in seq(1, n_rep)){
    #Sample x
    Sig <- (matrix(cor_value, nrow=2, ncol=2) + diag(2)*(1-cor_value)) * sd_2^2
    data <- data.frame(mvrnorm(n = n_1, mu = c(m_2, m_2), Sigma = Sig))
    Sig <- (matrix(cor_value, nrow=2, ncol=2) + diag(2)*(1-cor_value)) * sd_1^2
    data <- rbind(data,data.frame(mvrnorm(n = n_0, mu = c(m_0, m_0), Sigma = Sig)))
    y <- c(rep(1,n_1),rep(0,n_0))
    
    #Create data frame
    data <- cbind(data,y)
    
    #Logistic regression for "true" values
    model <- glm(y ~., family = binomial, data = data)
    
    #Save the coefficient
    original_1 <- c(original_1,model$coefficients[["X1"]])
    original_2 <- c(original_2,model$coefficients[["X2"]])
    #Randomly round 50% of the x values to the nearest 10s
    for(s in sample(seq(1,(n_1 + n_0)),heap_perc * (n_1+n_0))){
      data[s,]["X1"] <- round(data[s,]["X1"]/round_mult)*round_mult
    }
    
    #Logistic regression for the rounded values
    model <- glm(y ~., family = binomial, data = data)
    
    #Save the coefficient
    rounded_1 <- c(rounded_1,model$coefficients[["X1"]])
    rounded_2 <- c(rounded_2,model$coefficients[["X2"]])
    
    bias_1 <- c(bias_1,(original_1[length(original_1)]-rounded_1[length(rounded_1)]) * 100/original_1[length(original_1)])
    bias_2 <- c(bias_2,(original_2[length(original_2)]-rounded_2[length(rounded_2)]) * 100/original_2[length(original_2)])
    print(r)  
  }
  
  differences_1 <- original_1 - rounded_1
  differences_2 <- original_2 - rounded_2
  
  return(data.frame(cbind(original_1, rounded_1, bias_1, differences_1, original_2, rounded_2, bias_2, differences_2)))
}

plot_results <- function(result_df, bi = F){
  if(!bi){
    plot_data <- data.frame("coefficient" = c(result_df$original,result_df$rounded),y = c(rep("original",nrow(result_df)),rep("rounded",nrow(result_df))))
    plot_data$y <- as.factor(plot_data$y)
    p1 <- ggplot(plot_data,aes(x=coefficient, fill=y)) + geom_density(alpha=0.25)
    show(p1)
    
    t.test(result_df$original, result_df$rounded)
    
    
    p2 <- ggplot(result_df,aes(x=differences)) + geom_density(color="darkblue", fill="lightblue")
    show(p2)
    
    p3 <- ggplot(result_df,aes(x=bias)) + geom_density(color="darkblue", fill="lightblue")
    show(p3)
    
    fit.norm <- fitdist(result_df$differences, "norm")
    plot(fit.norm)
    
    summary(fit.norm)
  } else{
    
    plot_data <- data.frame("coefficient" = c(result_df$original_1,result_df$rounded_1),y = c(rep("original",nrow(result_df)),rep("rounded",nrow(result_df))))
    plot_data$y <- as.factor(plot_data$y)
    p1 <- ggplot(plot_data,aes(x=coefficient, fill=y)) + geom_density(alpha=0.25) + ggtitle("X1")
    show(p1)
    
    plot_data <- data.frame("coefficient" = c(result_df$original_2,result_df$rounded_2),y = c(rep("original",nrow(result_df)),rep("rounded",nrow(result_df))))
    plot_data$y <- as.factor(plot_data$y)
    p1 <- ggplot(plot_data,aes(x=coefficient, fill=y)) + geom_density(alpha=0.25) + ggtitle("X2")
    show(p1)
    
    
    p2 <- ggplot(result_df,aes(x=differences_1)) + geom_density(color="darkblue", fill="lightblue")
    show(p2)
    
    p2 <- ggplot(result_df,aes(x=differences_2)) + geom_density(color="darkblue", fill="lightblue")
    show(p2)
    
    p3 <- ggplot(result_df,aes(x=bias_1)) + geom_density(color="darkblue", fill="lightblue")
    show(p3)
    
    p3 <- ggplot(result_df,aes(x=bias_2)) + geom_density(color="darkblue", fill="lightblue")
    show(p3)
    
    fit.norm <- fitdist(result_df$differences_1, "norm")
    plot(fit.norm)
    
    fit.norm <- fitdist(result_df$differences_2, "norm")
    plot(fit.norm)
    
    summary(fit.norm)
  }
  
}
