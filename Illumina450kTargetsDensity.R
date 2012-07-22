###This function plots density plots according to the columns of the targets file
###Need the following libraries: minfi, IlluminaHumanMethylation450kmanifest
###Input
###data - matrix of methylation values, I usually use M values
###pd - is the object created from the targets file where rownames of pd match column names of the data matrix. Since the
###target file contains a lot of different information indicate the number of columns for which you want to plot the data density
###CancerType - character vector indicating cancer type, used for file naming and annotation of plots
###DataType - character vector indicating the data type, i.e. "Mvalue","Beta","raw". This is used for the annotation of plots.


vitadensity=function(z,w,cancer,values){
  ##z - methylation data
  ##w - column in the new pd file
  ##cancer - a character vector defining cancer type for the plots
  ##values - type of the data considered, i.e. "Mvalue","Beta","raw". This is used for the annotation of plots
  filename = paste(cancer,"density",values,name(w),".png",sep="")
  png(filename)
  densityPlot(z,sampGroups=w$name(w),main=paste(cancer,values),xlab=values)
  dev.off()
}



diagnIlluminaPlots = function(data,pd,CancerType,DataType){
  newpd=pd #new pd has only the selected column
  sapply(colnames(newpd),2,function(x,y) vitadensity(data,newpd[,x],CancerType,DataType))
}


