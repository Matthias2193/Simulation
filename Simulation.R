# Beispiel 1:  Wann hatten sie Ihre Herzoperation?    richtige Antwort:  1988;  Angabe 1990  ("runde Zahl")
# Beispiel 2:  Wieviel Zigaretten rauchen Sie durchschnittlich am Tag?    richtige Antwort: 21;   Antwort: 20
# 
# Das ganze nennt sich "heaping"
# 
# Die Frage ist nun, wie sich ein solches heaping auf das Ergebnis auswirkt, wenn man z.B. den Effekt des Alters bei einer OP oder der Rauchmenge auf eine sp?tere Erkrankung untersuchen m?chte. Das kann man mit einer Simulation quantifizieren. Man gibt sich ein beestimmtes Setting vor, z.B. eine Fall-Kontroll-Studie oder eine Kohortenstudie und simuliert Daten, die einen vorgegebenen Efffekt erzeugen.
# 
# Beispiel:  
#   man simuliert
# 100  Erkrankte, Faktor X normalverteilt mit, sagen wir, Erwartungswert 100 und Standardabweichung 10 
# 100  Gesunden, Faktor X normalverteilt mit Erwartungswert 90 und Standardabweichung 10        
# (damit hat man einen Effekt von X auf die Krankheit, den man mit einer logistischen Regression sch?tzt)
# 
# dann ver?ndert man die Daten, indem man z.B. sagt: 50% der Probanden geben den Wert gerundet auf die n?chste durch 10 teilbare Zahl an und berechnet den effekt der ver?nderten Variablen X
# 
# Das ganze macht man N mal (N zb. 10.000) und berechnet f?r die 10000 Simulationsl?ufe die  Verteilung der Differenz der Sch?tzer.

list.of.packages <- c("ggplot2", "fitdistrplus", "MASS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(fitdistrplus)
library(ggplot2)
library(MASS)


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
    Sig <- (matrix(cor_value, nrow=2, ncol=2) + diag(2)*(1-cor_value)) * sd_1^2
    data <- data.frame(mvrnorm(n = n_1, mu = c(m_1, m_1), Sigma = Sig))
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

#Univariate

result_df1 <- uni_sim(10000, c(100,100), c(75, 100), c(10, 10), 0.5, 10)
result_df2 <- uni_sim(10000, c(100,100), c(75, 100), c(10, 10), 0.5, 5)
result_df3 <- uni_sim(10000, c(500,500), c(75, 100), c(10, 10), 0.5, 10)
result_df4 <- uni_sim(10000, c(500,500), c(75, 100), c(10, 10), 0.5, 5)
result_df5 <- uni_sim(10000, c(100,100), c(75, 100), c(10, 10), 1, 10)
result_df6 <- uni_sim(10000, c(100,100), c(75, 100), c(10, 10), 1, 5)
result_df7 <- uni_sim(10000, c(500,500), c(75, 100), c(10, 10), 1, 10)
result_df8 <- uni_sim(10000, c(500,500), c(75, 100), c(10, 10), 1, 5)
plot_results(test_df)


#Bivariate
bi_result_df1 <- uni_sim(10000, c(100,100), c(75, 100, 100), c(10, 10, 10), 0.5, 10, 0.5)
bi_result_df1 <- uni_sim(10000, c(100,100), c(75, 100, 100), c(10, 10, 10), 0.5, 5, 0.5)
bi_result_df1 <- uni_sim(10000, c(500,500), c(75, 100, 100), c(10, 10, 10), 0.5, 10, 0.5)
bi_result_df1 <- uni_sim(10000, c(500,500), c(75, 100, 100), c(10, 10, 10), 0.5, 5, 0.5)
bi_result_df1 <- uni_sim(10000, c(100,100), c(75, 100, 100), c(10, 10, 10), 1, 10, 0.5)
bi_result_df1 <- uni_sim(10000, c(100,100), c(75, 100, 100), c(10, 10, 10), 1, 5, 0.5)
bi_result_df1 <- uni_sim(10000, c(500,500), c(75, 100, 100), c(10, 10, 10), 1, 10, 0.5)
bi_result_df1 <- uni_sim(10000, c(500,500), c(75, 100, 100), c(10, 10, 10), 1, 5, 0.5)

