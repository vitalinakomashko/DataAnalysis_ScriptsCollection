###This function takes a dataset and a number (n) and selects n the most
###variable genes based on their standard deviation
###Returns reduced dataset
###Use: to reduce dataset size for clustering

###Author: Vitalina Komashko
###Created: June 19, 2012
###Last updated:


mostVarData = function(data,n){
  sdProbes = apply(data,1,sd)
  sdProbesSorted = sdProbes[order(sdProbes,decreasing=T)]
  nSDs = sdProbesSorted[1:n]
  i = match(names(nSDs),rownames(data))
  reducedData = data[i,]
  return(reducedData)
}
