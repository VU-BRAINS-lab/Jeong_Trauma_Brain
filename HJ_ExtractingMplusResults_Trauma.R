setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(MplusAutomation)

# read mplus output file
modelResults <- readModels("HJ_Trauma_DesikanROIs_AverageCT_NoKSADS.out") 

# read standardized results
standardizedResults <- modelResults[["parameters"]][["stdyx.standardized"]]

# read the index of regression results (can find in "paramHeader" column ending in ".ON")
reg_i=grep(".ON", standardizedResults$paramHeader)

# read the index of results for trauma factor (can find in "param" column)
trauma_i=grep("TRAUMA", standardizedResults$param)

# find the index of regression results for trauma
reg_trauma=intersect(reg_i, trauma_i)

# read p-values
trauma_p=standardizedResults$pval[reg_trauma]

# perform fdr
trauma_p_fdr <- p.adjust(trauma_p, method="fdr")

# convert to data frame
trauma_p_fdr <- as.data.frame(trauma_p_fdr)

# print fdr-corrected p-values to three decimal places
trauma_p_fdr_round <- round(trauma_p_fdr,3)

# create dataframe to store data
trauma <- data.frame(
  Factor = "Trauma",
  est = standardizedResults$est[reg_trauma],
  se = standardizedResults$se[reg_trauma],
  est_se = standardizedResults$est_se[reg_trauma],
  pval = standardizedResults$pval[reg_trauma],
  FDR_pval = trauma_p_fdr_round,
  stringsAsFactors = FALSE
)

# save as csv
write.table(trauma, "FILE NAME.csv", quote=FALSE, sep=",", row.names=FALSE)
