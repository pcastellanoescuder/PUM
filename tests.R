
diffg_p <- function(phat_group1, phat_group0, samplesize0, samplesize1, alpha = 0.025){
  
  z.alpha <- qnorm(1-alpha,0,1)   
  
  riskdiff = phat_group1-phat_group0
  
  test <- (phat_group1-phat_group0)/sqrt((phat_group0*(1-phat_group0)/samplesize0+phat_group1*(1-phat_group1)/samplesize1))
  
  reject <- (test < - z.alpha)
  
  return(list(prob1 = phat_group1, prob0 = phat_group0, difference = riskdiff, test = test, reject = reject))
} 

# test risk ratio
diffg_rr <- function(phat_group1,phat_group0,samplesize0,samplesize1, alpha=0.025){
  
  z.alpha <- qnorm(1-alpha,0,1)  
  
  riskratio = phat_group1/phat_group0
  
  test <- log(phat_group1/phat_group0)*(( (1-phat_group1)/phat_group1/samplesize1 + (1-phat_group0)/phat_group0/samplesize0) )^(-1/2)
  
  reject <- (test < - z.alpha)
  
  return(list(prob1 = phat_group1, prob0 = phat_group0, riskratio = riskratio, test = test, reject = reject)) 
}

