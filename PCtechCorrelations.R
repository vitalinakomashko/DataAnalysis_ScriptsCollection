#This function calculates correlation between the principal components of the data and the technical variables supplied as a data frame

#Writing this function I learned that after svd I had a matrix of the prinicipal components, not a list
#Also, lapply works with the list or a vector so I needed to convert the matrix of the principal
#components to a data frame.

pcTechCor = function(pcs,techVar){
  output = lapply(as.data.frame(pcs),testCor,techVar)
  outputFrame = data.frame(matrix(unlist(output),nrow=length(output),byrow=T),row.names=names(output))
  colnames(outputFrame) = colnames(techVar)
  return(outputFrame)
}

#x is a vector, y is a data frame like techVar
#I am going to make a huge assumption that all technical variables are categorical and I will be comparing
#principal components and the technical variable, so I will use Kruskal - Wallis test
testCor = function(x,y){
  return(apply(y,2,function(v1,v2) kruskal.test(v1,as.factor(v2))$p.value, v1=x))
}
