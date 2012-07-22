###This function takes two dataframes: 
###1) with CpGs (some array IDs) and annotations for those CpGs
##2) with the data which CpGs from the first frame are a subset
###The goal: find the most variable CpGs for an annotation entity (annotation entity could be
###gene symbol, ENTREZ gene id, etc).
###information to provide:
###first data frame, column with CpG ids, column with annotation entity
###second data frame

###Output:
###List of the CpGs IDs that were found to be the most variable per each annotation entity (based on sd)
