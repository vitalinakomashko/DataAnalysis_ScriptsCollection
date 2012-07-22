###This function bundles together a few steps that I perform with every matrix during normalization:
###It plots data distribution, saved it to a file
###Performes SVD, plots relative variance explained to a file
###Identifies the outliers of the PC1 and a scatterplot between those two arrays and saves it to a file
###Input: data matrix and cancerName - character string giving the cancer name (for plots and file naming purposes)
###normalization - character string used to indicate if there is any normalization has be done with the data. Examples: "noNorm", "batchRemoved". Used for plots and file naming
###Function returns SVD (3 matrices) and 3 plots


basicSVD = function(data,cancerName,normalization){
  require(corpcor)
  ##Plot the data distribution
  png(paste(cancerName,"_Mvalue_",normalization,"_dataDistribution.png",sep=""))
  plot(density(data), main=paste(cancerName,"M value",normalization,"\n data distribution",sep=" "))
  dev.off()

  ##Perform SVD
  u = fast.svd(sweep(data,1,rowMeans(data)))
  ##Plot relative variance
  png(paste(cancerName,"_Mvalue_",normalization,"_RelativeVariance.png",sep=""))
  plot(u$d^2/sum(u$d^2),main=paste(cancerName,"M value",normalization,"\n relative variance",sep=" "))
  dev.off()

  ##Identify the outliers of the PC1
  out1 = order(u$v[,1])[1]
  out2 = order(-u$v[,1])[1]
  png(paste(cancerName,"_Mvalue_",normalization,"_PC1outliers.png",sep=""))
  plot(data[,c(out1,out2)],xlab=colnames(data)[out1],ylab=colnames(data)[out2],main=paste(cancerName,"M value", normalization,"PC1 outliers",sep=" "))
  dev.off()

  return(u)
}
