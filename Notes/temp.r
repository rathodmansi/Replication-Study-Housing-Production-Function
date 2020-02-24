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
