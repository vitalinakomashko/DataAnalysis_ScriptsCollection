###This script was designed to select the most variable CpGs per gene in a dataset
###Need to have a dataset with DNA methylation data, need to have row names as CpG IDs
###Also need to have a table with CpGs and corresponding genes. Could be gene names, symbols, ENTREZ ID, whatever
###CpG = column for the CpG ID
###gene = column for the gene ID
selectVarCPG<-function(meth,annot,CpG,gene){
  #get the subset of the data that matches CpGs from the annotation table
  i = match(annot[,CpG],rownames(meth))
  methCpGmatched = meth[i,]
  #calculate standard deviation for each CpG:
  CPGsd = apply(methCpGmatched,1,sd)
  #Now need to get the list of CpGs that have the max sd for each gene:
  maxSDlist = tapply(CPGsd,annot[,gene],function(x) return(list(which(x==max(x)))))
  #get the CpGs ids merged with the gene name, the format is genename.cpgid
  geneCPG = names(unlist(maxSDlist))
  #now split the names and get the CpGs:
  maxCPG = sapply(strsplit(geneCPG,"[.]"),"[",2)
  if (any(is.na(maxCPG))==TRUE){
    maxCPG = maxCPG[!is.na(maxCPG)]
  }
  #also get the gene names
  maxGene = sapply(strsplit(geneCPG,"[.]"),"[",1)
  if (any(is.na(maxGene))==TRUE){
    maxGene = maxGene[!is.na(maxGene)]
  }
  return(list(maxCPG,maxGene))
}