createExcel <- function(logLinear, linear, quadratic, cubic, gglm, ggllm, gdlm, n){
  
  table<- data.frame(matrix(ncol=8,nrow = 6))
  namesOfColumn <- c("Coefficients","Log_Linear","Linear","Quadratic","Cubic",
                         "Gaussian_Generalized_Linear_Model",
                         "Gaussian_Generalized_Log_Linear_Model",
                         "Gradient_Descent_Log_Linear_Model")
  colnames(table) <- namesOfColumn
  table$Coefficients <- c("v", "v^2", "v^3", "constant", "log(v)", "N")
  table[4,2] <- logLinear[1]
  table[5,2] <- logLinear[2]
  table[1,3] <- linear[1]
  table[1,4] <- quadratic[1]
  table[2,4] <- quadratic[2]
  table[1,5] <- cubic[1]
  table[2,5] <- cubic[2]
  table[3,5] <- cubic[3]
  table[1,6] <- gglm[1]
  table[4,7] <- ggllm[1]
  table[5,7] <- ggllm[2]
  table[4,8] <- gdlm[1]
  table[5,8] <- gdlm[2]
  table[6,2:8] <- as.numeric(n) 
  View(table)
  
}