suppressPackageStartupMessages({
  library(RSiena)
  library(sna)
  library(network)
})

set.seed(161)
data_path <- "Glasgow"

f1 <- as.matrix(read.csv(file.path(data_path, "f1.csv"), header = FALSE))
f2 <- as.matrix(read.csv(file.path(data_path, "f2.csv"), header = FALSE))
f3 <- as.matrix(read.csv(file.path(data_path, "f3.csv"), header = FALSE))
dimnames(f1) <- dimnames(f2) <- dimnames(f3) <- NULL

demographic <- read.csv(file.path(data_path, "demographic.csv"))
gender <- demographic$gender
age    <- demographic$age

alcohol <- as.matrix(read.csv(file.path(data_path, "alcohol.csv")))
dimnames(alcohol) <- NULL
alcohol[alcohol == 0] <- NA   # codebook is 1..5

logdistance <- as.matrix(read.csv(file.path(data_path, "logdistance.csv"), header = FALSE))
dimnames(logdistance) <- NULL

n <- nrow(f1)

# ---- GeodesicDistribution helper (RSiena manual, sec. 7) ----
GeodesicDistribution <- function(i, data, sims, period, groupName, varName,
                                 levls = c(1:5, Inf), cumulative = TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  a <- sna::geodist(symmetrize(x))$gdist
  if (cumulative) {
    gdi <- sapply(levls, function(i) sum(a <= i))
  } else {
    gdi <- sapply(levls, function(i) sum(a == i))
  }
  names(gdi) <- as.character(levls)
  gdi
}

# ---- Task 3.1.1: Jaccard ----
jaccard <- function(A, B) {
  A <- ifelse(is.na(A), 0, A); B <- ifelse(is.na(B), 0, B)
  n11 <- sum(A == 1 & B == 1)
  n10 <- sum(A == 1 & B == 0)
  n01 <- sum(A == 0 & B == 1)
  n11 / (n11 + n10 + n01)
}
jac <- c(j12 = jaccard(f1, f2), j23 = jaccard(f2, f3), j13 = jaccard(f1, f3))

# ---- Task 3.1.2: network-only data + effects ----
friendship <- sienaDependent(array(c(f1, f2, f3), dim = c(n, n, 3)))
genderCov  <- coCovar(as.numeric(gender))
ageCov     <- coCovar(as.numeric(age))
alcoholCov <- coCovar(as.numeric(alcohol[, 1]))
logdistCov <- coDyadCovar(logdistance)

mydata1 <- sienaDataCreate(friendship, genderCov, ageCov, alcoholCov, logdistCov)

myeff1 <- getEffects(mydata1)
myeff1 <- includeEffects(myeff1, transTrip)
myeff1 <- includeEffects(myeff1, cycle3)
myeff1 <- includeEffects(myeff1, inPopSqrt)
myeff1 <- includeEffects(myeff1, simX, interaction1 = "alcoholCov")
myeff1 <- includeEffects(myeff1, X,    interaction1 = "logdistCov")
myeff1 <- includeEffects(myeff1, egoX, altX, sameX, interaction1 = "genderCov")

algo1 <- sienaAlgorithmCreate(projname = "saom_t31", seed = 161, n3 = 3000)

if (file.exists("model1.RData")) {
  load("model1.RData")
  cat("Loaded existing model1\n")
} else {
  cat("=== Estimating Model 1 ===\n")
  model1 <- siena07(algo1, data = mydata1, effects = myeff1,
                    returnDeps = TRUE, batch = TRUE, verbose = FALSE)
  cat("Model 1 tconv.max:", model1$tconv.max,
      " max|t|:", max(abs(model1$tconv)), "\n")
  if (model1$tconv.max > 0.25 || max(abs(model1$tconv)) > 0.10) {
    model1 <- siena07(algo1, data = mydata1, effects = myeff1,
                      prevAns = model1, returnDeps = TRUE,
                      batch = TRUE, verbose = FALSE)
    cat("Model 1 rerun tconv.max:", model1$tconv.max, "\n")
  }
  save(model1, file = "model1.RData")
}

cat("\n=== Model 1 GoF ===\n")
gof1_indeg  <- sienaGOF(model1, IndegreeDistribution,  varName = "friendship", cumulative = FALSE)
gof1_outdeg <- sienaGOF(model1, OutdegreeDistribution, varName = "friendship", cumulative = FALSE)
gof1_geo    <- sienaGOF(model1, GeodesicDistribution,  varName = "friendship", cumulative = FALSE)
gof1_tri    <- sienaGOF(model1, TriadCensus,           varName = "friendship")

save(gof1_indeg, gof1_outdeg, gof1_geo, gof1_tri, file = "gof1.RData")

# ---- Task 3.2: co-evolution ----
friendship2 <- sienaDependent(array(c(f1, f2, f3), dim = c(n, n, 3)))
drinkingbeh <- sienaDependent(alcohol, type = "behavior")
genderCov2  <- coCovar(as.numeric(gender))
ageCov2     <- coCovar(as.numeric(age))
logdistCov2 <- coDyadCovar(logdistance)

mydata2 <- sienaDataCreate(friendship2, drinkingbeh, genderCov2, ageCov2, logdistCov2)

myeff2 <- getEffects(mydata2)
myeff2 <- includeEffects(myeff2, transTrip, cycle3)
myeff2 <- includeEffects(myeff2, inPopSqrt)
myeff2 <- includeEffects(myeff2, egoX, altX, sameX, interaction1 = "genderCov2")
myeff2 <- includeEffects(myeff2, X,    interaction1 = "logdistCov2")
myeff2 <- includeEffects(myeff2, simX, interaction1 = "drinkingbeh")
myeff2 <- includeEffects(myeff2, indeg, name = "drinkingbeh", interaction1 = "friendship2")
myeff2 <- includeEffects(myeff2, avSim, name = "drinkingbeh", interaction1 = "friendship2")
myeff2 <- includeEffects(myeff2, effFrom, name = "drinkingbeh", interaction1 = "genderCov2")

algo2 <- sienaAlgorithmCreate(projname = "saom_t32", seed = 161, n3 = 3000)

if (file.exists("model2.RData")) {
  load("model2.RData")
  cat("Loaded existing model2\n")
} else {
  cat("\n=== Estimating Model 2 ===\n")
  model2 <- siena07(algo2, data = mydata2, effects = myeff2,
                    returnDeps = TRUE, batch = TRUE, verbose = FALSE)
  cat("Model 2 tconv.max:", model2$tconv.max,
      " max|t|:", max(abs(model2$tconv)), "\n")
  tries <- 0
  while ((model2$tconv.max > 0.25 || max(abs(model2$tconv)) > 0.10) && tries < 3) {
    tries <- tries + 1
    model2 <- siena07(algo2, data = mydata2, effects = myeff2,
                      prevAns = model2, returnDeps = TRUE,
                      batch = TRUE, verbose = FALSE)
    cat("Model 2 rerun", tries, "tconv.max:", model2$tconv.max, "\n")
  }
  save(model2, file = "model2.RData")
}

cat("\n=== Model 2 GoF ===\n")
gof2_indeg  <- sienaGOF(model2, IndegreeDistribution,  varName = "friendship2", cumulative = FALSE)
gof2_outdeg <- sienaGOF(model2, OutdegreeDistribution, varName = "friendship2", cumulative = FALSE)
gof2_geo    <- sienaGOF(model2, GeodesicDistribution,  varName = "friendship2", cumulative = FALSE)
gof2_beh    <- sienaGOF(model2, BehaviorDistribution,  varName = "drinkingbeh")

save(gof2_indeg, gof2_outdeg, gof2_geo, gof2_beh, file = "gof2.RData")

save(jac, model1, model2,
     gof1_indeg, gof1_outdeg, gof1_geo, gof1_tri,
     gof2_indeg, gof2_outdeg, gof2_geo, gof2_beh,
     file = "saom_results.RData")

cat("\n=== ALL DONE ===\n")

cat("\n--- Model 1 estimates ---\n")
print(data.frame(
  Effect = model1$effects$effectName,
  Estimate = round(model1$theta, 4),
  SE = round(sqrt(diag(model1$covtheta)), 4),
  t_value = round(model1$theta / sqrt(diag(model1$covtheta)), 3)
))

cat("\n--- Model 2 estimates ---\n")
print(data.frame(
  Effect = model2$effects$effectName,
  Estimate = round(model2$theta, 4),
  SE = round(sqrt(diag(model2$covtheta)), 4),
  t_value = round(model2$theta / sqrt(diag(model2$covtheta)), 3)
))
