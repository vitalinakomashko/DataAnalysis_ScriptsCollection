##This script collects p values for each CpG for each array based on the Level 1 TCGA human methylation 27k arrays

methPvalCollect = function(){
  patients = list() #initialize a list to collect patient IDs
  pValMat = list() #initialize a lits t
  n = 0 #initialize a counter for the number of arrays processed, will be printed
  files = list.files(pattern=".txt")
  for (i in seq(along=files)) {
    y = read.table(files[i],skip=2,row.names=1)
    inds = strsplit(files[i],"\\.")[[1]][6]
    print(inds)
    patients[[length(patients)+1]] = inds
    pValCol = y[,11]
    pValMat[[length(pValMat)+1]] = pValCol
    n=n+1
  }
  pValMatFinal =  matrix(unlist(pValMat), ncol=n, byrow=FALSE)
  rownames(pValMatFinal) = row.names(y)
  colnames(pValMatFinal) = unlist(patients)
  return(pValMatFinal)
}
