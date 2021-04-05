# Beispiel 1:  Wann hatten sie Ihre Herzoperation?    richtige Antwort:  1988;  Angabe 1990  ("runde Zahl")
# Beispiel 2:  Wieviel Zigaretten rauchen Sie durchschnittlich am Tag?    richtige Antwort: 21;   Antwort: 20
# 
# Das ganze nennt sich "heaping"
# 
# Die Frage ist nun, wie sich ein solches heaping auf das Ergebnis auswirkt, wenn man z.B. den Effekt des Alters bei einer OP oder der Rauchmenge auf eine spätere Erkrankung untersuchen möchte. Das kann man mit einer Simulation quantifizieren. Man gibt sich ein beestimmtes Setting vor, z.B. eine Fall-Kontroll-Studie oder eine Kohortenstudie und simuliert Daten, die einen vorgegebenen Efffekt erzeugen.
# 
# Beispiel:  
#   man simuliert
# 100  Erkrankte, Faktor X normalverteilt mit, sagen wir, Erwartungswert 100 und Standardabweichung 10 
# 100  Gesunden, Faktor X normalverteilt mit Erwartungswert 90 und Standardabweichung 10        
# (damit hat man einen Effekt von X auf die Krankheit, den man mit einer logistischen Regression schätzt)
# 
# dann verändert man die Daten, indem man z.B. sagt: 50% der Probanden geben den Wert gerundet auf die nächste durch 10 teilbare Zahl an und berechnet den effekt der veränderten Variablen X
# 
# Das ganze macht man N mal (N zb. 10.000) und berechnet für die 10000 Simulationsläufe die  Verteilung der Differenz der Schätzer.

list.of.packages <- c("ggplot2", "fitdistrplus")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(fitdistrplus)
library(ggplot2)

original <- c()
rounded <- c()

for(r in seq(1,1000)){
  #Sample x
  x <- c(rnorm(100, mean = 100, sd = 10), rnorm(100, mean = 75, sd = 10))
  rlnorm(100, meanlog = log(100), sdlog = log(10))
  y <- c(rep(1,100),rep(0,100))
  
  #Create data frame
  data <- data.frame(x,y)
  
  #Logistic regression for "true" values
  model <- glm(y ~., family = binomial, data = data)
  
  #Save the coefficient
  original <- c(original,model$coefficients[["x"]])
  
  #Randomly round 50% of the x values to the nearest 10s
  for(s in sample(seq(1,200),100)){
    data[s,]["x"] <- round(data[s,]["x"], digits = -1)
  }
  
  #Logistic regression for the rounded values
  model <- glm(y ~., family = binomial, data = data)
  
  #Save the coefficient
  rounded <- c(rounded,model$coefficients[["x"]])
  
  bias <- (original[-1]-rounded[-1])/original[-1]
}

differences <- original - rounded




plot_data <- data.frame("coefficient" = c(original,rounded),y = c(rep("original",1000),rep("rounded",1000)))
plot_data$y <- as.factor(plot_data$y)
ggplot(plot_data,aes(x=coefficient, fill=y)) + geom_density(alpha=0.25)
unheaped_data$y <- as.factor(unheaped_data$y)
ggplot(unheaped_data,aes(x=x, fill=y)) + geom_histogram(alpha=0.25, bins = 100)
t.test(original, rounded)


ggplot(data.frame(differences),aes(x=differences)) + geom_density(color="darkblue", fill="lightblue")


fit.norm <- fitdist(differences, "norm")
plot(fit.norm)

summary(fit.norm)


mean(differences)/mean(original) *100
