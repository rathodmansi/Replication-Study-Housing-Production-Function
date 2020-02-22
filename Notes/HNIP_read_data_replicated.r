################################################################
### Code for paper:
### "A New Approach to Estimating the Production Function for Housing"
### by Dennis Epple, Brett Gordon, and Holger Sieg, forthcoming AER.
###
### This code uses the implementation of the 
### estimator in Hausman, Newey, Ichimura, and Powell (Journal of Econometrics, 1991)
### and applies it to the data set in the paper on Pittsburgh housing.
###
### Written by Brett Gordon, June 2008
### Comments should be sent to brg2114@columbia.edu.
################################################################


source("HNIP_estimator_REPLICATED.R")

INST.HAS.CONSTANT = TRUE
BASE.EQ.CONSTANT = FALSE

#### (1) Read in the raw data

# Just pland, v, and lotarea
data = read.table("data.txt", header=T, sep=",")

N = dim(data)[1]

#### (2) Clean up the data by removing outliers. Consistent with file 'supply.R'
v = data$v
pland = data$pland
lotarea = data$lotarea
muni = data$muni
tcog = data$tcog

## Maybe don't do this, to be consistent with results in main paper already
sorted = sort(v,index.return=TRUE, method="quick")
pland = pland[sorted$ix]
lotarea = lotarea[sorted$ix]
muni = muni[sorted$ix]
tcog = tcog[sorted$ix]  ## the INSTRUMENT: Travel time to city center, in minutes
v = sorted$x

v.full = v
pland.full = pland

goodv = (((v.full < 144.4682981)*(v > 0.923818)) == 1)  ### 185 and 0.30
v = v.full[goodv]
pland = pland.full[goodv]
lotarea = lotarea[goodv]
muni = muni[goodv]
tcog = tcog[goodv]
N = length(v)  ## New data size after stripping away some observations

### For real data
#q = tcog
#x = v
#y = pland

K = 3  #### Order of the polynomial to use
p.cont = K+1  ### dimensions of the instruments
p.dummy = length(unique(muni)) - 1   ##
P = p.cont + p.dummy  #### Dimension of the instruments

### Create design matrix of dummy variables based on municipalities
Q.d = factor(muni)
Q.df = model.matrix(~ Q.d - 1)

### Estimate with the dummies as IVs
beta.hat.3 = HNP.estimator(K,p.cont,tcog,(as.matrix(Q.df))[,2:(p.dummy+1)],v,pland)

### Estimate without the dummy variables as IVs
# beta.hat = HNP.estimator(K,p.cont,tcog, matrix(1,1,1),v,pland)

#############################################
### Bootstrap for standard errors
#############################################
B = 1000
beta.hat = matrix(NA, nrow=B, ncol=K + 1)  ## add one for chi.stat

for(b in 1:B){
  
  index.b = sample(N, replace=T)
  v.b = v[index.b]
  pland.b = pland[index.b]
  muni.b = muni[index.b]
  tcog.b = tcog[index.b]
  
  p.dummy = length(unique(muni.b)) - 1   ##
  P = p.cont + p.dummy  #### Dimension of the instruments
  
  Q.d.b = factor(muni.b)
  Q.df.b = model.matrix(~ Q.d.b - 1)
  
  beta.hat[b,] = HNP.estimator(K,p.cont,tcog.b,(as.matrix(Q.df.b))[,2:(p.dummy+1)],v.b,pland.b)
  # beta.hat[b,] = HNP.estimator(K,p.cont,tcog.b, matrix(1,1,1),v.b,pland.b)
  print(beta.hat)
  print(b)
  if(b %% 50 == 0){
    print(paste("b = ", b)) ## print iterations
  }
}

est = sapply(1:K, function(i){ return( round(c(mean(beta.hat[,i]), sqrt(var(beta.hat[,i])) ), digits=8) ) })
par(mfrow=c(1,3))
plot(density(beta.hat[,1]), main="Coefficient for V")
plot(density(beta.hat[,2]), main="Coefficient for V^2")
plot(density(beta.hat[,3]), main="Coefficient for V^3")

round(est, digits=6)









