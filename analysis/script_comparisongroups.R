#############################################

#PUM - Analisis pre-/post-sanitary measures

#############################################

# inputs: 
# i) comparison based on diff probabilities, risk ratio, odds ratio
# ii) rates in each group (e.g., death rates before sanitary measures at april 54 bs death rates before sanitary measures at april 55)
# iii) army sizes in each group

# test diff p
diffg_p <- function(phat_group1,phat_group0,samplesize0,samplesize1){
  test <- (phat_group1-phat_group0)/sqrt((phat_group0*(1-phat_group0)/samplesize0+phat_group1*(1-phat_group1)/samplesize1))
  return(test)
}

# test risk ratio
diffg_rr <- function(phat_group1,phat_group0,samplesize0,samplesize1){
  test <- log(phat_group1/phat_group0)*(( (1-phat_group1)/phat_group1/samplesize1 + (1-phat_group0)/phat_group0/samplesize0) )^(-1/2)
  return(test)
}

# test odds ratio
diffg_or <- function(phat_group1,phat_group0,samplesize0,samplesize1){
  test <- log((phat_group1/(1-phat_group1))/(phat_group0/(1-phat_group0)))*((1/(phat_group0*(1-phat_group0)/samplesize0)+ 1/(phat_group1*(1-phat_group1)/samplesize1)))^(-1/2)
  return(test)
}