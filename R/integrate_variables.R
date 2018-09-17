

integrate_variables <- function(Assays, name, groups,
groupinfo = NULL, ...){

  allassays <- assays(name)

  selectedassays <- allassays[Assays]

  PCAlist <- list()

  factors = Assays

  for(Assay in Assays){
    subset <- list()
    tmp1 <- list()

    factor_value <- selectedassays[[Assay]]

    for(i in seq(from = 1, to = length(groups))){
      tmp1[[i]] <- rownames(
      groupinfo[groupinfo$group == groups[i],, drop=FALSE])

      subset <- c(subset, tmp1[[i]])
    }

    tmp0 <- factor_value[row.names(factor_value)
                         %in% subset,]

    dim = ncol(factor_value)

    PCA1 <- PCA(tmp0, ncp = dim, ...)

    PCAlist[[Assay]] <- PCA1
  }

  return(PCAlist)

}
