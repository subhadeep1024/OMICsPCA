library(OMICsPCAdata)
library(OMICsPCA)

anno <- system.file("extdata/annotation/TSS_groups.bed", package = "OMICsPCAdata")
list <- system.file("extdata/annotation/GENCODE_list", package = "OMICsPCAdata")
fact <- system.file("extdata/factors/H2az", package = "OMICsPCAdata")

test_prepareDataset <- function(){

H2az <- prepareDataset(factdir = fact, annofile = anno, annolist = list)

}