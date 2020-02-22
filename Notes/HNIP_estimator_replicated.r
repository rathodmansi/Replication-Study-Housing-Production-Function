################################################################
### Code for paper:
### "A New Approach to Estimating the Production Function for Housing"
### by Dennis Epple, Brett Gordon, and Holger Sieg, forthcoming AER.
###
### This code implements the nonlinear errors-in-variables estimator 
### found in Hausman, Newey, Ichimura, and Powell (Journal of Econometrics, 1991).
###
### Written by Brett Gordon, June 2008
### Comments should be sent to brg2114@columbia.edu.
################################################################

library(MASS)

####################################################################
# K - Order of the polynomial to use
# P.cont - Dimension of the instruments (includes constant if added)
# q - single continuous instrumental variable
# Q.dummy - matrix of instrumental dummy variables
# x - observable data
# y - observable outcomes
HNP.estimator = function(K,P.cont,q,Q.dummy,x,y){
  
  N = length(x)
  
  ### Check if matrix of dummy variables
  if(dim(Q.dummy)[1] == 1){
    INST.DUMMY = FALSE
  }else{
    INST.DUMMY = TRUE
  }
  
  P = P.cont + ifelse(INST.DUMMY, dim(Q.dummy)[2], 0)
  
  #### (1) Estimate alpha coefficients (HNIP, p. 283, eq. 3.3)
  # Create vector of instruments
  
  if(INST.HAS.CONSTANT){
    qvec = matrix(1, nrow=N, ncol=P.cont)
    for(i in 1:K){ qvec[,i+1] = q^{i} }
    if(INST.DUMMY){
      qvec = cbind(qvec, Q.dummy)
    }
    alpha.stage.lm <- lm(x ~ qvec[,2:P])
  }else{
    qvec = matrix(1, nrow=N, ncol=P.cont)
    for(i in 1:K){ qvec[,i] = q^{i} }
    if(INST.DUMMY){
      qvec = cbind(qvec, Q.dummy)
    }
    alpha.stage.lm <- lm(x ~ qvec - 1)
  }
  
  #summary(alpha.stage.lm)
  
  alpha = alpha.stage.lm$coefficients     		
  
  #### (2) Form the w's, the fitted values of the instruments from above
  
  w = alpha.stage.lm$fitted.values
  
  #### (3) Estimate the gamma coefficients (HNIP, p. 284, eq. 3.5)
  
  W = matrix(0, nrow=N, ncol=K)
  for(i in 1:K){ W[,i] = w^i }
  
  if(BASE.EQ.CONSTANT){
    gamma.stage.lm <- lm(y ~ W) 
  }else{
    gamma.stage.lm <- lm(y ~ W-1)  ### No constant b/c model says that 0 = r(0)
  }
  
  #summary(gamma.stage.lm)
  
  gamma = (gamma.stage.lm$coefficients)
  
  #### (4) Estimate the delta coefficients (HNIP, p. 285, eq. 3.8)  
  
  xy = x*y
  
  WW = matrix(0, nrow=N, ncol=K+1)
  for(i in 1:(K+1)){ WW[,i] = w^i}
  
  if(BASE.EQ.CONSTANT){
    delta.stage.lm <- lm(xy ~ WW)  
  }else{
    delta.stage.lm <- lm(xy ~ WW - 1)   	  # DO NOT allow for a constant
  }
  #summary(delta.stage.lm)
  delta = delta.stage.lm$coefficients   ## Keep all the coefficients, including constant
  
  ####################################################################
  #### Estimate the covariance matrix of the reduced-form parameters
  ####################################################################
  
  dim.gamma = length(gamma)
  dim.delta = length(delta)
  
  a = alpha.stage.lm$residuals  ## Residuals from top of page. 288 in HNIP
  Q = calcExpectedOuterProduct(qvec)
  Q.inv = solve(Q)
  sum.A = matrix(0, nrow=P, ncol=P)
  for(n in 1:N){
    sum.A = sum.A + (a[n]*a[n])*(qvec[n,]%*%t(qvec[n,]))
  }
  A = sum.A/N
  
  ### Asymptotic covariance matrix for gamma (p. 288, eq. 3.15)
  #V.gamma = Q.inv %*% A %*% Q.inv  
  #sqrt(diag(V.gamma))
  
  ### s_i and t_i in the paper are same as W and WW in the code
  ### Dimenion here does not need to include constant - will be added later if needed
  Delta.S = matrix(0, N, K)   ### Rows should be {1, i*w^(i-1),...}
  Delta.T = matrix(0, N, K+1)
  for(i in 1:K){
    Delta.S[,i] = i*(w^(i-1))
    Delta.T[,i] = Delta.S[,i]
  }
  Delta.T[,K+1] = (K+1)*(w^(K))
  
  if(BASE.EQ.CONSTANT){
    W = cbind(rep(1, N), W)  ## Add 1 for the constant
    WW = cbind(rep(1, N), WW)
    Delta.S = cbind(rep(0,N), Delta.S)  ## Add 0 for the constant (after taking derivative)
    Delta.T = cbind(rep(0,N), Delta.T)	
  }
  S = calcExpectedOuterProduct(W)
  T = calcExpectedOuterProduct(WW)
  
  sum.F = matrix(0, nrow=dim.gamma, ncol=P)
  for(n in 1:N){
    sum.F = sum.F + as.matrix( as.vector(t(Delta.S[n,])%*%gamma) * (W[n,] %*% t(qvec[n,])))
  }
  F = sum.F/N
  
  sum.G = matrix(0, nrow=dim.delta, ncol=P)
  for(n in 1:N){
    sum.G = sum.G + as.matrix( as.vector(t(Delta.T[n,])%*%delta) * (WW[n,] %*% t(qvec[n,])))
  }
  G = sum.G/N
  
  ## Get residuals from first stage regressions
  e = gamma.stage.lm$residuals
  u = delta.stage.lm$residuals
  
  ## Create other matrices for asymptotic variance
  sum.M0 = matrix(0, nrow=dim.gamma, ncol=dim.gamma)
  sum.M1 = matrix(0, nrow=dim.delta, ncol=dim.delta)
  sum.M2= matrix(0, nrow=dim.delta, ncol=dim.gamma)
  
  for(n in 1:N){
    seFQqa = ( (W[n,]*e[n])  - (F%*%Q.inv %*% (qvec[n,]*a[n])) )
    tuGQqa = ( (WW[n,]*u[n]) - (G%*%Q.inv %*% (qvec[n,]*a[n])) )
    
    sum.M0 = sum.M0 + crossprod(t(seFQqa))
    sum.M1 = sum.M1 + crossprod(t(tuGQqa))
    sum.M2 = sum.M2 + as.matrix(tuGQqa  %*% t(seFQqa))
  }
  M0 = sum.M0/N
  M1 = sum.M1/N
  M2 = sum.M2/N
  
  MM = rbind(cbind(M0, t(M2)), cbind(M2, M1))	### Center matrix of eq 3.16, page 289
  ST = rbind(cbind(S, matrix(0,nrow=dim.gamma,ncol=dim.delta)),   ##side matrix
             cbind(matrix(0,nrow=dim.delta,ncol=dim.gamma), T))
  
  ST.inv = solve(ST)
  V = ST.inv %*% MM %*% ST.inv
  dim.V = dim(V)[1]  ## Should be the same as dim.gamma + dim.delta
  
  ####################################################################
  #### Obtain efficient estimates of the overidentified parameters 
  ####################################################################
  
  pi1.index = c(length(gamma), length(gamma)-1)
  pi2.index = c(dim.V, dim.V - 1)
  if(BASE.EQ.CONSTANT){
    ## WITH CONSTANT, b/c want to ignore delta_0
    pi3.index = c((dim.gamma - 2):1, (dim.V-2):(dim.gamma+2))  
  }else{
    ## NO CONSTANT, b/c no delta_0 to ignore, so delta[len(gamma)+1] = delta_1
    pi3.index = c((dim.gamma - 2):1, (dim.V-2):(dim.gamma+1))
  }
  
  V11 = as.matrix(V[pi1.index,pi1.index]); V12 = as.matrix(V[pi1.index,pi2.index]); V13 = as.matrix(V[pi1.index,pi3.index]);
  V21 = as.matrix(V[pi2.index,pi1.index]); V22 = as.matrix(V[pi2.index,pi2.index]); V23 = as.matrix(V[pi2.index,pi3.index]);
  V31 = as.matrix(V[pi3.index,pi1.index]); V32 = as.matrix(V[pi3.index,pi2.index]); V33 = as.matrix(V[pi3.index,pi3.index]);
  
  pi1.hat = c(gamma[dim.gamma], gamma[dim.gamma-1])
  pi2.hat = c(delta[dim.delta], delta[dim.delta-1])
  
  if(BASE.EQ.CONSTANT){
    pi3.hat = c(gamma[(dim.gamma - 2):1], delta[(dim.delta-2):2])
  }else{
    pi3.hat = c(gamma[(dim.gamma - 2):1], delta[(dim.delta-2):1])
  }
  
  ### Efficient estimators from eq. 3.17 on page 290
  V.11.22.12.21.inv = solve(V11 + V22 - V12 - V21)
  pi2.tilde = as.vector(pi2.hat - ((V21 - V22) %*% V.11.22.12.21.inv %*% (pi1.hat - pi2.hat)))
  pi3.tilde = as.vector(pi3.hat - ((V31 - V32) %*% V.11.22.12.21.inv %*% (pi1.hat - pi2.hat)))
  
  ### Asymptotic covariance matrix for pi2.tilde and pi3.tilde, eq 3.18 (not needed)
  #Omega22 = V22 - as.matrix( (V21 - V22) %*% V.11.22.12.21.inv %*% (V21 - V22) )
  #Omega23 = V23 - as.matrix( (V21 - V22) %*% V.11.22.12.21.inv %*% (V21 - V22) )
  #Omega32 = V32 - as.matrix( (V31 - V32) %*% V.11.22.12.21.inv %*% (V31 - V32) )
  #Omega33 = V33 - as.matrix( (V31 - V32) %*% V.11.22.12.21.inv %*% (V31 - V32) )
  
  ### Update our estimates of the reduced-form parameters before doing the recursion
  dim.pi3.tilde = length(pi3.tilde)
  gamma.tilde = c(pi3.tilde[(dim.gamma-2):1], pi2.tilde[2], pi2.tilde[1])
  delta.tilde = c(pi3.tilde[dim.pi3.tilde:(dim.gamma-2+1)], pi2.tilde[2], pi2.tilde[1])
  
  ### Use the updated pi2.tilde and pi3.tilde estimates to do the recursion
  if(BASE.EQ.CONSTANT){			##Add the 0 here so that the indices line up correctly
    fin.beta = doRecursion.constant(K, c(0,delta.tilde), gamma.tilde)
  }else{
    fin.beta = doRecursion.no.constant(K, delta.tilde, gamma.tilde)
  }
  
  ### Chi-sq statistic for test of overidentifying restrictions (df = 2)
  chi.stat = N*((pi1.hat - pi2.hat) %*% V.11.22.12.21.inv %*% (pi1.hat - pi2.hat))
  
  return(c(fin.beta, chi.stat))
  
} ### End of HNP.estimator function
#############################################################################################


#############################################################################################
### Use the recursion formulae on HNIP, eq. 3.11 - 3.14 to get sturctural parameters
### given some estimates of the last two beta values (from the second-stage above)
####################################################################
doRecursion.constant = function(K, delta.hat, gamma.hat){
  evj = rep(0, K+1)
  evj[1] = 1  ## j = 0
  evj[2] = 0  ## j = 1
  
  beta.hat = rep(0, K+1)
  beta.hat[K+1] = gamma.hat[length(gamma.hat)]
  beta.hat[K] = gamma.hat[length(gamma.hat)-1]
  
  ### the term '+1', with two spaces, is to handle the intercept terms at j=0, that must be indexed at 1 in R
  for(j in 2:K){
    
    ## (1) Get the evj's
    
    denom = choose(K,K-j+1)*beta.hat[K  +1]
    
    lb = K - j + 1
    ub = K - 1
    numer = delta.hat[K-j+1 +1] - gamma.hat[K-j  +1]
    for(l in lb:ub){
      numer = numer - choose(l,K-j+1)*beta.hat[l  +1]*evj[l-K+j  +1] ## add one b/c of intercept
    }
    
    evj[j+1] = numer/denom
    
    ## (2) Get the beta.hat's
    
    beta.hat[K-j  +1] = gamma.hat[K-j  +1]
    lb = K - j + 1
    ub = K
    for(l in lb:ub){
      beta.hat[K-j  +1] = beta.hat[K-j  +1] - choose(l,K-j)*beta.hat[l  +1]*evj[l-K+j +1]
    }
  }
  return(beta.hat)
}
#############################################################################################

#############################################################################################
### Use the recursion formulae on HNIP, eq. 3.11 - 3.14 to get sturctural parameters
### given some estimates of the last two beta values (from the second-stage above)
###
### NOTE:   NO CONSTANT
####################################################################
doRecursion.no.constant = function(K, delta.hat, gamma.hat){
  evj = rep(0, K)
  evj[1] = 0  ## j = 1
  
  beta.hat = rep(0, K)
  beta.hat[K] = gamma.hat[length(gamma.hat)]
  beta.hat[K-1] = gamma.hat[length(gamma.hat)-1]
  
  if(K == 2){
    return(beta.hat)
  }
  
  ### the term '+1', with two spaces, is to handle the intercept terms at j=0, that must be indexed at 1 in R
  for(j in 2:(K-1)){
    
    ## (1) Get the evj's
    
    denom = choose(K,K-j+1)*beta.hat[K]
    
    lb = K - j + 1
    ub = K - 1
    numer = delta.hat[K-j+1] - gamma.hat[K-j]
    for(l in lb:ub){
      numer = numer - choose(l,K-j+1)*beta.hat[l]*evj[l-K+j]
    }
    
    evj[j] = numer/denom
    
    ## (2) Get the beta.hat's
    
    beta.hat[K-j] = gamma.hat[K-j]
    lb = K - j + 1
    ub = K
    for(l in lb:ub){
      beta.hat[K-j] = beta.hat[K-j] - choose(l,K-j)*beta.hat[l]*evj[l-K+j]
    }
  }
  return(beta.hat)
}
#############################################################################################

#################################################################
### Helper function to compute expected outer product
### e.g.  E[s_i s_i'] where s_i is a (K x 1) vector, and there are i in 1:N observations
### Returns a matrix of dimension (K x K)
calcExpectedOuterProduct = function(X){
  if(is.vector(X)){
    return(sum(X^2)/length(X))
  }else{
    N = dim(X)[1]
    K = dim(X)[2]
    sum.X = matrix(0, nrow=K, ncol=K)
    for(n in 1:N){
      sum.X = sum.X + as.matrix(X[n,] %*% t(X[n,]))
    }
    return(sum.X/N)
  }
}