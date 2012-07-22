###survivalBuild: builds a survival object, plots a Kaplan Meier Curve and saves the data frame for the survival data with the censoring indicator
###clinical = usually clinical_patient_public.txt read as a data frame to an R session
###death = column number with the days to death
###fup = column number with the days to the last follow up
###vital = column number with the vital status
###cancerName = character string giving the cancer type, e.g. "GBM,"BRCA"
###batch = vector of batch matched to the clinical based on patient ID

survivalBuild = function(clinical, death, fup, vital, cancerName, batch){
  require(survival)
  infoTable = as.data.frame(cbind(clinical[,death],clinical[,fup],clinical[,vital]))
  colnames(infoTable) = c("DaysDeath","DaysFollowup","Vital")
  cat("Number of patients is", nrow(infoTable),"...","\n")
  rownames(infoTable) = rownames(clinical)
  ##check if there are any rows that don't have information for the days to death and days to the last follow up
  if (!is.null(nrow(infoTable[is.na(infoTable$DaysDeath) & is.na(infoTable$DaysFollowup),]))){
    mask = is.na(infoTable$DaysDeath) & is.na(infoTable$DaysFollowup)
    infoTable = infoTable[!mask,]
    cat("Number of patients for whom no days to death and days to the last follow are available is",length(which(mask!=0)),"\n")
    cat("Survival object will be constructed for",nrow(infoTable),"patients","\n")
    batch = batch[!mask]
    batch = droplevels(batch)
  }
  ##Create a sensoring indicator
  status = rep(1,nrow(infoTable))
  ##Assign values of the sensoring indicator to 0 for whose elements which don't have days to death in the same position in the infoTable
  status[which(is.na(infoTable$DaysDeath))] = 0
  ##Now assign days to the last follow up as days to death for those patients for whom days to death are NA
  infoTable[is.na(infoTable$DaysDeath),1] = infoTable[which(is.na(infoTable$DaysDeath)),2]
  survivalTable = cbind(infoTable,status)
  write.table(survivalTable,paste("SurvivalTable",cancerName,".txt",sep=""),sep="\t")
  ##Create a survival object
  surv = Surv(as.numeric(survivalTable$DaysDeath),as.numeric(survivalTable$status))
  png(paste("KaplanMeierCurve",cancerName,".png",sep=""))
  plot(survfit(surv~1),xlab="Days to death",main=paste("Kaplan-Meier curve for TCGA ",cancerName,sep=""))
  dev.off()
  ##Correlate batch and survival and plot survival by batch. Return statistics for coxph model
  cat("Calculate correlation of the batch vs survival..The result will be written to a file\n")
  survBatch(surv,batch,cancerName)
  return(survivalTable)
}

###Correlate batch with survival and make a plot of survival for the patients per batch
survBatch = function(survivalObject,batchVector,cancerType){
  sink(file=paste("SurvivalBatchSummaryStatistics",cancerType,".txt",sep=""))
  print(summary(coxph(survivalObject~batchVector)))
  sink(file=NULL)
  png(paste("SurvivalByBatch",cancerType,".png",sep=""))
  plot(survfit(survivalObject~batchVector),xlab="days to death",main=paste("Survival by batch for TCGA",cancerType,sep=" "),col=1:length(levels(as.factor(batchVector))), lty=1:length(levels(as.factor(batchVector))))
  dev.off()
}
