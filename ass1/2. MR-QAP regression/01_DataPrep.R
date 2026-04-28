#------------------------------- IMPORTING -------------------------------------

if (!require(sna)) install.packages("sna")
if (!require(here)) install.packages("here")

files <- list.files(here("../Data"), pattern="\\.csv$", full.names=TRUE) # import the files

dataList <- lapply(files, function(f) {  read.csv(f, sep=";", header=TRUE, row.names=1)}) # read the CSV files with lambda expression
names(dataList) <- tools::file_path_sans_ext(basename(files))   # cut of the csv part

MatricesRaw <- lapply(dataList, as.matrix)
set.seed(161)

#-------------------------------- CLEANING -------------------------------------

isNetwork <- sapply(MatricesRaw, function(m) nrow(m) == ncol(m))
networkMatricesRaw <- MatricesRaw[isNetwork]
networkMatricesClean <- lapply(networkMatricesRaw, function(mat) {
  validRows <- complete.cases(mat)
  mat[validRows, validRows]
})   # clean all the network matrices

attrRaw <- MatricesRaw[grepl("attr", names(MatricesRaw), ignore.case=TRUE)][[1]]
validStudents <- complete.cases(attrRaw)
attrClean <- attrRaw[validStudents, ]    # clean the attributes matrix

for(i in seq_along(networkMatricesClean)){
  
  colnames(networkMatricesClean[[i]]) <- sub("^X0*", "", colnames(networkMatricesClean[[i]]))
  rownames(networkMatricesClean[[i]]) <- sub("^0*", "", rownames(networkMatricesClean[[i]]))
  
} # clean the leading X and 0

saveRDS(networkMatricesClean, here("networkMatricesClean.rds"))                 # saves as RDS
saveRDS(attrClean, here("attrClean.rds"))

#----------------------------------- 2.1 ---------------------------------------

W1Clean <- networkMatricesClean[["10_W1"]]
W2Clean <- networkMatricesClean[["10_W2"]]

nNodes <- nrow(W1Clean)
W10_3d <- array(0, dim=c(nNodes, nNodes, 2))
W10_3d[,,1] <- W1Clean  # Wave 1
W10_3d[,,2] <- W2Clean # Wave 2

nlTask1 <- netlogit(W2Clean, W1Clean, reps=5000, nullhyp="qapspp")
print(summary(nlTask1))