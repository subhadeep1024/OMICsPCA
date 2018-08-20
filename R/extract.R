extract <- function(name,groups, integrated = TRUE, Assay = NULL,
                    rand = NULL, PC = c(1,2,3,4)){
    if(integrated){
    ##reading entire data frame of integrated assays in tmp
    tmp <- eval(parse(text = paste0(name,
    "@integratedPCA$ind$coord")))

    ## if groups == "all" read all rows of original data
    ##into tmp1
    tmp1 <- eval(parse(text = paste0(name,"@groups$all")))

    ## if groups are mentioned, subset tmp1
    if(length(groups) == 1 && groups != "all"){
    tmp1 <- tmp1[tmp1$group %in% groups,,drop=FALSE]
    }

    ## if groups radom rows needs to be selected from tmp
    if(is.null(rand)){
    group_individuals <- tmp[row.names(tmp) %in% row.names(
    tmp1),PC,drop = FALSE]
    } else{
    tmp <- tmp[row.names(tmp) %in% row.names(
    tmp1),PC,drop = FALSE]

    ## next line selects the random rows
    group_individuals <- tmp[sample(nrow(tmp), rand), ,
    drop = FALSE]
    }
    } else {
    ## reading data from individual assay, in this
    ##case, "Assay" argument should be passed
    tmp <- eval(parse(text = paste0(name,"@PCA$",Assay,
    "$ind$coord")))

    ## if groups == "all" read all rows of original data
    ##into tmp1
    tmp1 <- eval(parse(text = paste0(name,"@groups$all")))

    ## if groups are mentioned, subset tmp1
    if(length(groups) == 1 && groups != "all"){
    tmp1 <- tmp1[tmp1$group %in% groups,,drop=FALSE]
    }

    ## if groups radom rows needs to be selected from tmp
    if(is.null(rand)){
    group_individuals <- tmp[row.names(tmp) %in% row.names(
    tmp1),PC,drop = FALSE]
    } else{
    tmp <- tmp[row.names(tmp) %in% row.names(
    tmp1),PC,drop = FALSE]

    ## next line selects the random rows
    group_individuals <- tmp[sample(nrow(tmp), rand), ,
    drop = FALSE]
    }
    }
    return(group_individuals)
}
