#------------------------------- IMPORTING -------------------------------------
set.seed(161)


if (!require(sna)) install.packages("sna")
if (!require(here)) install.packages("here")
here::i_am("main.R")

files <- list.files("Data", pattern="\\.csv$", full.names=TRUE) # import the files

dataList <- lapply(files, function(f) {  read.csv(f, sep=";", header=TRUE, row.names=1)}) # read the CSV files with lambda expression
names(dataList) <- tools::file_path_sans_ext(basename(files))   # cut of the csv part

MatricesRaw <- lapply(dataList, as.matrix)


#-------------------------------- CLEANING V2 ----------------------------------


getNet <- function(classroom) {
    w2net <- MatricesRaw[[paste(classroom, "_W2", sep="")]]
    colnames(w2net) <- rownames((w2net))
    ok_ids1 <- rownames(w2net)[complete.cases(w2net)]


    attr <- MatricesRaw[["attr"]]
    ok_ids2 <- rownames(attr)[complete.cases(attr)]

    ok_ids <- intersect(ok_ids1, ok_ids2)

    w2netClean <- as.matrix(w2net[ok_ids,ok_ids])
    w2netClean <- w2netClean / pmax(rowSums(w2netClean),1)

    return(w2netClean)
}

getAttr <- function(classroom) {
    w2net <- MatricesRaw[[paste(classroom, "_W2", sep="")]]
    colnames(w2net) <- sub("^X0*", "", colnames(w2net))
    ok_ids1 <- rownames(w2net)[complete.cases(w2net)]

    attr <- MatricesRaw[["attr"]]
    ok_ids2 <- rownames(attr)[complete.cases(attr)]

    ok_ids <- intersect(ok_ids1, ok_ids2)

    attrClean <- as.data.frame(attr[ok_ids, ])
    attrClean$HISEI <- as.numeric(attrClean$HISEI)
    attrClean$literacy_end <- as.numeric(attrClean$literacy_end)

    return(attrClean)
}

#-------------------------------------------------------------------------------


