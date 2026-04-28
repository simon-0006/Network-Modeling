#---------------------------------- SETUP --------------------------------------

if (!require(sna)) install.packages("sna")
if (!require(here)) install.packages("here")

covariatesList <- readRDS(here("covariatesAllClasses.rds"))

resultsList <- list()

#---------------------------------- MR-QAP -------------------------------------

for(classID in names(covariatesList)) {
  cov <- covariatesList[[classID]]

  fit <- netlogit(
    cov$w2,
    list(
      cov$w1,
      cov$literacyRec,
      cov$sameGender,
      cov$hiseiSend
    ),
    nullhyp = "qapspp",
    reps = 5000
  )
  
  s <- summary(fit)
  print(s)
  coefVec <- fit$coefficients
  pVec <- fit$pgreqabs
  
  varNames <- c("Intercept", "w1", "literacyRec", "sameGender", "hiseiSend")
  
  classResults <- data.frame(
    classID = classID,
    variable = varNames,
    beta = coefVec,
    oddsRatio = exp(coefVec),
    oddsRatio = exp(coefVec),
    pValue = pVec,
    stringsAsFactors = FALSE
  )
  
  resultsList[[classID]] <- classResults
}

#---------------------------------- SAVE RESULTS --------------------------------

allResults <- do.call(rbind, resultsList)

saveRDS(allResults, here("mrqapResults.rds"))
write.csv(allResults, here("mrqapResults.csv"), row.names = FALSE)

cat("\nAnalysis complete for", length(resultsList), "classes\n")

print(allResults)
