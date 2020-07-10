
### Florence prediction code

setwd("~/Copia13.01.20/Concursos/Florence Nightgale/bbdd")
load("proc_data.rdata")
names(data)
data$Month_date <-as.Date(as.yearmon(data$month))
data$total_deaths <- data$zymotic_diseases_deaths +data$wounds_injuries_deaths + data$all_other_causes_deaths
data$period <- 1:nrow(data)

## In this section the shiny app provides an interactive section for dinamyc prediction of the mortality rate of a specific month using previous
## information. As statistician at 1850, would you be able to provide an accurate prediction of the future mortality rate? 

## 0) radioButton per selecionar el método de predicció. De moment tenim "Linear prediction", "Exponential smoothing".

#######################
## 1) Linear prediction
#######################

# Exaplicació: Linear regression to predict the next mortality rate taking into consideration the x last observed mortality rates

## Quines entrades necessita aquesta predicció:
# 1) Boto que et pregunti si vols fer la predicció abans de que s'implantesis les mesures de sanitat o després: 
#     "Prediction before health measures or prediction after health measures"
# 2) Un sliderInput que et permeti seleccionar a quina data vols fer la predicció. Les opcions de la sliderInput han d'estar condicionats 
# a si es vol fer a predició pre o post mesures. Si és pre, les opcions que han de sortir és del Juny del 54 al febrer del 55. Si es post,
# ha de ser del May del 55 al Març 56.
## 3) numericInput, "how many previous months do you want to use for prediction"?

enter1 <- "Dec 1854"  # Aquí s'indica el mes on es vol fer la predicció
enter2 <- 7 # Quants mesos previs volen considerar per fer la predicció?

time_pred <- which(data$month==enter1 )
start <- which(data$month==enter1 ) - enter2

x <- data$period[start:(time_pred-1)]
y <- data$zymotic_diseases_MR1000[start:(time_pred-1)]
reg <- lm(y ~ x)
new <- data.frame(x =time_pred )
pred.linear <- predict(lm(y ~ x), new, interval = "prediction");pred.linear

windows(20,14)
plot(x,y,xlim=c(1,time_pred),ylim=c(0,max(data$zymotic_diseases_MR1000)),pch=19,cex=1.8,xaxt="n",las=1, ylab="Mortality rate")
axis(1, at=c(1:time_pred),labels=data$month[1:time_pred], las=2)
points(data$period[time_pred], data$zymotic_diseases_MR1000[time_pred],col="darkorange",pch=15,cex=1.8)
points(data$period[time_pred],ifelse(pred.linear[1]>0,pred.linear[1],0),col="darkred",pch=19,cex=1.8)
abline(reg)
legend("topleft",c("Observed value","Predicted value") ,bty = "n",text.font=2, col=c("darkorange","darkred"),cex=1.4,pch=c(15,19))


#######################
## 2) Exponential smoothing 
#######################

library(tidyverse)
# install.packages("fpp2")
library(fpp2) 

# Explicació: Exponential smoothing to predict the next mortality rate taking into consideration the x last observed mortality rates (R function hold)

## Aquesta predicció ha de tenir els mateixos elemnents d'entrada que l'anterior.

enter1 <- "Jan 1855"  # Aquí s'indica el mes on es vol fer la predicció
enter2 <- 7 # Quants mesos previs volen considerar per fer la predicció?

time_pred <- which(data$month==enter1 )
start <- which(data$month==enter1 ) - enter2

x <- data$period[start:(time_pred-1)]
y <- data$zymotic_diseases_MR1000[start:(time_pred-1)]

holt.res <- holt(y, h = 1)
plot(x,y,xlim=c(1,time_pred),ylim=c(0,max(data$zymotic_diseases_MR1000)),pch=19,cex=1.4,xaxt="n",xlab=" ")
axis(1, at=c(1:time_pred),labels=data$month[1:time_pred], las=2)
points(data$period[time_pred], data$zymotic_diseases_MR1000[time_pred],col="darkorange",pch=15,cex=1.8)
points(data$period[time_pred],holt.res$mean[1],col="darkred",pch=19,cex=1.8)
legend("topleft",c("Observed value","Predicted value") ,bty = "n",text.font=2, col=c("darkorange","darkred"),cex=1.4,pch=c(15,19))

