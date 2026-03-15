#---------------------------------- SETUP --------------------------------------

if (!require(sna)) install.packages("sna")
if (!require(here)) install.packages("here")

networkMatricesClean <- readRDS(here("networkMatricesClean.rds"))
attrClean <- readRDS(here("attrClean.rds"))

classNames <- names(networkMatricesClean)[grepl("_W2$", names(networkMatricesClean))] # Find the second wave classes

covariatesList <- list()
finalNetworks <- list()

for(className in classNames){
  
#------------------------------- FILTERING -------------------------------------
  
  cat("Processing", className, "\n")
  
  classID <- sub("_W2$", "", className)
  w1Name <- paste0(classID, "_W1") # build the name of the first Wave
  
  networkW1Rows <- rownames(networkMatricesClean[[w1Name]])
  networkW2Rows <- rownames(networkMatricesClean[[className]])
  attrRows <- rownames(attrClean) # get the student data from the tables
  
  overlapWaves <- intersect(networkW1Rows, networkW2Rows)
  finalRows <- intersect(overlapWaves, attrRows) # filter students with all the necessary data
  
  cat("  ", length(finalRows), "students →", length(finalRows)^2, "dyads\n")
  
  w1Final <- networkMatricesClean[[w1Name]][finalRows, finalRows]
  w2Final <- networkMatricesClean[[className]][finalRows, finalRows]
  attrFinal <- attrClean[finalRows, ] # get the matrix subsets
  
  
  nNodes <- length(finalRows)
  
#---------------------------------- VARIABLES ----------------------------------

  literacyRec <- matrix(attrFinal[, "literacy_end"], nNodes, nNodes, byrow = FALSE) # Literacy
  
  sameGender <- outer(attrFinal[, "gender"], attrFinal[, "gender"], "==") * 1 # homophily
  
  hiseiSend <- matrix(attrFinal[, "HISEI"], nNodes, nNodes, byrow = TRUE) # HISEI
  
  mode(w1Final) <- "numeric"
  mode(w2Final) <- "numeric"
  mode(literacyRec) <- "numeric"
  mode(sameGender) <- "numeric"
  mode(hiseiSend) <- "numeric"

#----------------------------------- SAVING ------------------------------------
  
  covariatesList[[classID]] <- list(
    w2 = w2Final,
    w1 = w1Final,
    literacyRec = literacyRec,
    sameGender = sameGender,
    hiseiSend = hiseiSend
  )
  
  finalNetworks[[classID]] <- list(
    w1 = w1Final,
    w2 = w2Final
  )
  
}

saveRDS(covariatesList, here("covariatesAllClasses.rds"))
saveRDS(finalNetworks, here("finalNetworksAligned.rds"))

cat("Saved", length(covariatesList), "classes\n")