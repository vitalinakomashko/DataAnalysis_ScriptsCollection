level3combined = function(){
    patients = list()
    betaMatrix = list()
    n = 0
    files = list.files(pattern=".txt")
    for (i in seq(along=files)){
        y = read.table(files[i],header=T,row.names=1,sep="\t")
        fields = strsplit(files[i],"\\__")[[1]][3]
        print(fields) #printing the patient id
        patients[[length(patients) + 1]] = inds
        probeNames = y[,1]
        beta = y[,2]
        n = n+1
        print(n)
        betaMatrix[[length(betaMatrix)+1]] = beta
    }
    betaMatrixFinal = matrix(unlist(betaMatrix), ncol=n, byrow=TRUE)
    rownames(betaMatrixFinal) = probeNames
    colnames(betaMatrixFinal) = unlist(patients)
    return(betaMatrixFinal)
}
