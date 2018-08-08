

integrate_variables <- function(Assays, name, groups, ...){

    factors = Assays
    for(Assay in Assays){
        subset <- list()
        tmp1 <- list()

        factor_value <- eval(parse(text=paste0(name,
        "@InputAssays$",Assay)))

        for(i in seq(from = 1, to = length(groups))){
            tmp1[[i]] <- row.names(eval(parse(
            text = paste0(name,"@groups$", groups[i]))))

            subset <- c(subset, tmp1[[i]])
        }

        tmp0 <- factor_value[row.names(factor_value)
        %in% subset,]

        dim = ncol(eval(parse(text = paste0(name,
        "@InputAssays$",Assay))))

        PCA1 <- PCA(tmp0, ncp = dim, ...)

        eval(parse(text = paste0(
        name,"@PCA[[\"",Assay,"\"]] <<- PCA1")))
    }

}
