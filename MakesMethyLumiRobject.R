#This R script is for processing HumanIlluminaMethylation27k arrays
#The result is like M value (according to the code in Bioconductor package lumi)
methArrayProcess = function(){
  patients = list() #initialize a list to collect patient IDs
  exprs = list() #initialize a lits t
  n = 0 #initialize a counter for the number of arrays processed, will be printed 
  files = list.files(pattern=".txt") 
  for (i in seq(along=files)) {
    ##read a file, skip two rows, don't care about the headers
    y = read.table(files[i],skip=2,row.names=1)
    ##get the patient ID from the file name
    inds = strsplit(files[i],"\\.")[[1]][6]
    print(inds)
    patients[[length(patients)+1]] = inds	
    mm = min(c(y[,1],y[,4]))
    if (mm < 0.01){
      y[,1][y[,1]<0.01] = 0.01
      y[,4][y[,4]<0.01] = 0.01
    } 
    M = log2((y[,1]+1)/(y[,4]+1))
    n = n+1
    print(n)
    exprs[[length(exprs)+1]] = M
  }
  exprsM =  matrix(unlist(exprs), ncol=n, byrow=FALSE)
  rownames(exprsM) = row.names(y)
  colnames(exprsM) = unlist(patients)
  return(exprsM)
}
