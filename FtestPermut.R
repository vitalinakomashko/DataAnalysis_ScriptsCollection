###This function calculates p values of the F test for the permuted data
###Input:
###data - data matrix for the columns of which F test is to be calculated
###sampleGroup - factor with two levels indicating columns between which the comparison is to be performed
###iter - number of iterations
###Output: a matrix with p values for each row and columns equal to the number of iterations

FtestPermute = function(data,sampleGroup,iter){
  #outputMatrix = matrix(nrow=nrow(data),ncol=iter)
  outputMatrix = list()
  for (i in 1:iter){
    samplesPermuted = sample(sampleGroup,size=length(sampleGroup))
    FtestI = apply(data, 1, function(x) var.test(x~samplesPermuted)$p.value)
    #outputMatrix[,i] = FtestI
    outputMatrix[[length(outputMatrix)+1]] = FtestI
  }
  #rownames(outputMatrix) = rownames(data)
  return(outputMatrix)
}



  
