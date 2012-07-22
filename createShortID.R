##This function takes a long barcode for TCGA patients and converts it to a short one
##

##input - a vector of long TCGA IDs from DNA methylation, gene expression or other
##Split - character by which the barcode should be split
##newSep - character that will be joining the fields for the new barcode
#for now it can truncate the barcodes only to the first 3 fields

##Written by Vitalina Komashko
##Date: April 24, 2012

createShortTCGA = function(input,Split,newSep){
  barcodeFields = strsplit(input,split=Split)
  barcodeMatrix = t(sapply(barcodeFields,"[",1:3))
  shortBarcodes = apply(barcodeMatrix,1,function(x) paste(x[1],x[2],x[3],sep=newSep))
  return(shortBarcodes)
}


