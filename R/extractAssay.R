extractAssay <- function(name, Assay, groups,
groupinfo = NULL, addgroupnames = TRUE){

    allassays <- assays(name)
    selectedassay <- allassays[[Assay]]
    x <- groupinfo
    x <- x[x$group %in% groups,,drop = FALSE]
    y <- name[[Assay]]
    y =  y[rownames(y) %in% rownames(x),]

    x <- x[match(rownames(y), rownames(x)),,drop = FALSE]

    if(addgroupnames){
    y <- cbind(y,x)
    }


    return(y)
}
