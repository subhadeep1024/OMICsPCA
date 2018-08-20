extractAssay <- function(name, Assay, groups){

    subset <- NULL
    x <- eval(parse(text = paste0(name,"@groups$all")))
    x <- x[x$group %in% groups,,drop = FALSE]
    for(Assay in Assay){
        y <- eval(parse(text = paste0(name,"@InputAssays$",
        Assay)))
        y =  y[rownames(y) %in% rownames(x),]
        subset[[Assay]] <- y
    }

    subset <- as.data.frame(subset)
    names(subset) <- names(eval(parse(text = paste0(name,
                        "@InputAssays$", Assay))))

    subset <- transform(subset, group = x$group)
    return(subset)
}
