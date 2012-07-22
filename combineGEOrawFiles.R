##This function combines GEO DNA methylation files into M or beta value
##for each CpG for each patient

#Date last modified: May 7, 2012
#metric - do we you want to have beta or M value as the result
#The function outputs a matrix with M or beta values and a matrix with
#P values

GEOcombine = function(metric){
  patients = list()
  dataMatrix = list()
  pvalMatrix = list()
  n = 0
  files = list.files(pattern=".txt$") #$because I also have .txt.gz files
  for (i in seq(along=files)){
    y = read.table(files[i],header=T,row.names=1,sep="\t")
    inds = strsplit(files[i],"[.]")[[1]][1]
    print(inds) #printing the patient id
    patients[[length(patients) + 1]] = inds
    probeNames = rownames(y)
    #print(length(probeNames))
    if(metric=="beta"){
      beta = y[,1]
      dataMatrix[[length(dataMatrix)+1]] = beta
    }
    else{
      if (min(y[,2],y[,3])<0.01){
        y[,2][y[,2]<0.01] = 0.01
        y[,3][y[,3]<0.01] = 0.01
      }
      mval = log2((y[,2]+1)/(y[,3]+1))
      dataMatrix[[length(dataMatrix)+1]] = mval
    }
    pValData = y[,4]
    pvalMatrix[[length(pvalMatrix)+1]] = pValData
    n = n+1
    print(n)
  }
  dataMatrixFinal = matrix(unlist(dataMatrix), ncol=n)
  rownames(dataMatrixFinal) = probeNames
  colnames(dataMatrixFinal) = unlist(patients)
  pValDataFinal = matrix(unlist(pvalMatrix), ncol=n)
  rownames(pValDataFinal) = probeNames
  colnames(pValDataFinal) = unlist(patients)
  return(list(dataMatrixFinal, pValDataFinal))
}