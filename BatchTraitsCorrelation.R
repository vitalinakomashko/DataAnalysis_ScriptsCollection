###To run this script place source it from a directory (/blah/blah/CancerName) where you unpack TCGA clinical data archive. It should have the files such as clinical_patient_public.txt and clinical_aliquot_public.txt. Also, it will create files with the cancer name and it gets the name from the directory to which the clinical files are unpacked. For example, if you have /home/vkomask/TCGA/GBM/ it will use the name of the directory (GBM) as the cancer name. If you want it to work nicely, make sure to have your directory sstructure as /blah/blah/CancerName. 

### This function calculates correlations between the batch defined as the sixth field in the patient barcode and all clinical traits available from the Clinical file clinical_patient_public_CancerType.txt. The input is the clinical_patient_public file and the batch. Batches need to be matched to the patients. The output of this script is a list that give the types of the data for each trait, such as logical (all NAs), factor ("categorical") or integer (continuous), number of NAs for that column, type of statistical test performed and the associated p values. If the number of NA's for a particulat trait is equal or less than 50% then a statistical test for association of the batch with that biological trait is performed. If it a categorical trait, then chi-square test is performed, if it is a continuous trait, then kruskal.test is performed. If there are too many NA's statistical test will be "too many NAs" and for the associated p-values the result will be "not calculated".


batchTrait <- function(clinical, batch, CancerID){
  outputList = lapply(clinical,comparison, batch)
  outputFrame = data.frame(matrix(unlist(outputList),nrow=length(outputList),byrow=T),row.names=names(outputList))
  colnames(outputFrame) = c("DataType","NumberOfNAs","Test","Pvalue")
  write.table(outputFrame,paste("BatchClinicalInfoCorrelations",CancerID,".txt",sep=""),sep="\t")
}

comparison <- function(x,y){
  numberOfNAs = length(which(is.na(x)))
  columnType = class(x)
  if (class(x)=="factor" & length(levels(x))>=2 & length(which(is.na(x)))<=length(x)/2){
    testType = chisq.test(x,y)$method
    pValue = chisq.test(x,y)$p.value
  }
  else if (class(x)=="factor" & length(levels(x))<2) {
    testType = "less than 2 levels"
    pValue = "not calculated"
  }
  else if (class(x)=="integer" & length(which(is.na(x))) <= length(x)/2){
    testType = kruskal.test(x,as.factor(y))$method
    pValue = kruskal.test(x,as.factor(y))$p.value
  }
  else {
    testType = "too many NA's"
    pValue = "not calculated"
  }
  return(list(columnType,numberOfNAs,testType,pValue))
}


###This part of the script deals with reading and formatting the files in the way the it is useful for running the batchTrait function. It is a very routine work and I get tired of doing it every single time

### Here is what it does:

### 1. Read the clinical file, report how many clinical traits we are working with
clinFileAccess = grep("clinical_patient_public_*",dir())
clinical = read.table(dir()[clinFileAccess],header=T,row.names=1,sep="\t",na.strings="null")
cat("number of clinical traits is", ncol(clinical),"\n")

### 2. Read clinical aliquot file
aliqFileName =  grep("clinical_aliquot_public_*",dir())
aliquot = read.table(dir()[aliqFileName],header=T,sep="\t")

### 3. Subset methylation samples according to this pattern: TCGA-..-....-0..-..D-....-05, bcr_aliquot_barcode
barcodeMatch = grep("TCGA-..-....-0..-..D-....-05",aliquot[,2])
aliquotMeth = aliquot[barcodeMatch,]

### 4. Split the methylation barcodes by "-"
fields = strsplit(as.character(aliquotMeth[,2]),split="-")

### 5. Take fields 1,2,3 - make a short patient barcode
one = sapply(fields,"[",1)
center = sapply(fields,"[",2) #second field in the patient ID is the center
three = sapply(fields,"[",3)
shortID = paste(one,center,three,sep="-")

### 6. Take 6 - get the batch=plate ID; report how many batches are observed
batchID = sapply(fields,"[",6)
cat("number of batches based on DNA methylation data is",length(table(batchID)),"\n")

### 7. Cbind short ids, batch and the methylation samples in a single data frame
aliquotMethBatch = cbind(shortID, batchID,aliquotMeth)

### 8. Match clinical data frame and a new data frame according to the short id
clinicalMatch = match(rownames(clinical),aliquotMethBatch[,1])
meth = aliquotMethBatch[clinicalMatch[!is.na(clinicalMatch)],]
clinicalMethMatch = match(meth[,1],rownames(clinical))
clinicalMatched = clinical[clinicalMethMatch[!is.na(clinicalMethMatch)],]

### 9. Write it to a file "DNAmetBatchMatchedToClinicalCancerCode.txt"
directory = unlist(strsplit(getwd(),split="/"))
cancerCode = directory[length(directory)]
write.table(meth,paste("DNAmethBatchMatchedToClinical",cancerCode,".txt",sep=""),sep="\t",row.names=F)

###Finally, calculate correlations between the clinical variables and the batch
batchTrait(clinicalMatched,meth[,2],cancerCode)

###remove intermediate variables to clean up the workspace. Note: I should really put these steps in a function which will take care of the cleaning process
rm("one","three","clinicalMatch","clinicalMethMatch","directory","cancerCode","aliquotMethBatch","clinical","clinFileAccess","aliqFileName","aliquot","aliquotMeth","barcodeMatch","fields","shortID")
