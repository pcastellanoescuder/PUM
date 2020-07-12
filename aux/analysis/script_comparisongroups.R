#############################################

# pum! - Analisis pre-/post-sanitary measures

#############################################


load("C:/Users/mbofi/Dropbox/C5/Scripts/GitKraken/PUM/data/proc_data.RData")

# Table estimated probabilities of death by year
# prob_year = death_year/samplesize1

data$prob_zymotic_death = data$zymotic_diseases_deaths/data$average_size_of_army
data$prob_injuries_death = data$wounds_injuries_deaths/data$average_size_of_army
data$prob_other_death = data$all_other_causes_deaths/data$average_size_of_army


# inputs: 
# i) comparison based on diff probabilities, risk ratio, odds ratio
# ii) probabilities in each group (e.g., death rates before sanitary measures at april 54 bs death rates before sanitary measures at april 55) <-check dates
# iii) army sizes in each group
# iv) cause of death

diffg_p <- function(phat_group1,phat_group0,samplesize0,samplesize1, alpha=0.025){
  
  z.alpha <- qnorm(1-alpha,0,1)   
  
  riskdiff = phat_group1-phat_group0
  
  test <- (phat_group1-phat_group0)/sqrt((phat_group0*(1-phat_group0)/samplesize0+phat_group1*(1-phat_group1)/samplesize1))
  
  reject <- (test < - z.alpha)
  
  return(list(prob_postmeasures=phat_group1,prob_premeasures=phat_group0,difference=riskdiff,test=test, reject=reject))
}

# test risk ratio
diffg_rr <- function(phat_group1,phat_group0,samplesize0,samplesize1, alpha=0.025){
  
  z.alpha <- qnorm(1-alpha,0,1)  
  
  riskratio = phat_group1/phat_group0
  
  test <- log(phat_group1/phat_group0)*(( (1-phat_group1)/phat_group1/samplesize1 + (1-phat_group0)/phat_group0/samplesize0) )^(-1/2)
  
  reject <- (test < - z.alpha)
  
  return(list(prob_postmeasures=phat_group1,prob_premeasures=phat_group0,riskratio=riskratio,test=test)) 
}

# test odds ratio
diffg_or <- function(phat_group1,phat_group0,samplesize0,samplesize1, alpha=0.025){
  
  z.alpha <- qnorm(1-alpha,0,1)  
  
  oddsratio = (phat_group1/(1-phat_group1))/(phat_group0/(1-phat_group0))
  
  test <- log((phat_group1/(1-phat_group1))/(phat_group0/(1-phat_group0)))*((1/(phat_group0*(1-phat_group0)/samplesize0)+ 1/(phat_group1*(1-phat_group1)/samplesize1)))^(-1/2)
  
  reject <- (test < - z.alpha)
  
  return(list(prob_postmeasures=phat_group1,prob_premeasures=phat_group0,oddsratio=oddsratio,test=test))
  
}

