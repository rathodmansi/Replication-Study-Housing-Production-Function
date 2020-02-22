

# Define the squared error cost function	
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y)) 
}


gradientDescentLinearModel <- function(x, y, alpha, num_iters){
   
   alpha <- alpha # Specify the learning rate
   num_iters <- num_iters # Specify the number of iterations
   cost_history <- rep(0,num_iters) # will be used to store the value of cost function after
   # every iteration 
   theta_history <- list(num_iters) # will be used to store the value of theta after every 
   # iteration 
   theta <-  c(0,0) # Initial values of theta
   X <- cbind(1,x) # Add a column vector with all values  to be 1 to x so that hypothesis 
   # function has an intercept 
   for (i in 1:num_iters) {
      theta[1] <- theta[1] - alpha * (1/length(y)) * sum(((X%*%theta)- y))
      theta[2] <- theta[2] - alpha * (1/length(y)) * sum(((X%*%theta)- y)*X[,2])
      cost_history[i] <- cost(X, y, theta)
      theta_history[[i]] <- theta
   } 
   # Plots the training dataset
   plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
   # Plots various lines during the course of convergence
   for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
      abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
   }
   abline(coef=theta, col='blue')
   return(theta)
}

calculatePredictionValues <- function(x, y, slope, intercept){
   y <- slope * x + intercept
   print(y)
}


#gradientDescentLinearModel(log(dataSubset$v), log(dataSubset$pland), 0.1, 1000)
