#############################################

# pum! - Analisis pre-/post-sanitary measures

#############################################


# Table estimated probabilities of death by year

prob_year = death_year/samplesize1


# inputs: 
# i) comparison based on diff probabilities, risk ratio, odds ratio
# ii) rates in each group (e.g., death rates before sanitary measures at april 54 bs death rates before sanitary measures at april 55)
# iii) army sizes in each group
# iv) cause

# test diff p

# deaths_group1, deaths_group2
# return: probabilities, risk diff, test, frase conclusions

alpha=0.025 
z.alpha <- qnorm(1-alpha,0,1)   

diffg_p <- function(phat_group1,phat_group0,samplesize0,samplesize1){
  
  phat_group1 = death_year1/samplesize1
  phat_group0 = death_year0/samplesize0
  
  riskdiff = phat_group1-phat_group0
  
  test <- (phat_group1-phat_group0)/sqrt((phat_group0*(1-phat_group0)/samplesize0+phat_group1*(1-phat_group1)/samplesize1))
  
  reject <- (test < - z.alpha)
  
  return(list(prob_postmeasures=phat_group1,prob_premeasures=phat_group0,difference=riskdiff,test=test, reject=reject))
}

# test risk ratio
diffg_rr <- function(phat_group1,phat_group0,samplesize0,samplesize1){
  
  phat_group1 = death_year1/samplesize1
  phat_group0 = death_year0/samplesize0
  
  riskratio = phat_group1/phat_group0
  
  test <- log(phat_group1/phat_group0)*(( (1-phat_group1)/phat_group1/samplesize1 + (1-phat_group0)/phat_group0/samplesize0) )^(-1/2)
  
  reject <- (test < - z.alpha)
  
  return(list(prob_postmeasures=phat_group1,prob_premeasures=phat_group0,riskratio=riskratio,test=test)) 
}

# test odds ratio
diffg_or <- function(phat_group1,phat_group0,samplesize0,samplesize1){
  
  phat_group1 = death_year1/samplesize1
  phat_group0 = death_year0/samplesize0
  
  oddsratio = (phat_group1/(1-phat_group1))/(phat_group0/(1-phat_group0))
  
  test <- log((phat_group1/(1-phat_group1))/(phat_group0/(1-phat_group0)))*((1/(phat_group0*(1-phat_group0)/samplesize0)+ 1/(phat_group1*(1-phat_group1)/samplesize1)))^(-1/2)
  
  reject <- (test < - z.alpha)
  
  return(list(prob_postmeasures=phat_group1,prob_premeasures=phat_group0,oddsratio=oddsratio,test=test))
  
}



