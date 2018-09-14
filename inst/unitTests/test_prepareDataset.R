library(OMICsPCAdata)

anno <- system.file("extdata/annotation2/TSS_groups.bed",
                    package = "OMICsPCAdata")

list <- system.file("extdata/annotation2/TSS_list",
                    package = "OMICsPCAdata")

fact <- system.file("extdata/factors2/demofactor",
                    package = "OMICsPCAdata")

test_prepareDataset <- function(){

demo <- prepareDataset(factdir = fact, annofile = anno,
annolist = list)

}
