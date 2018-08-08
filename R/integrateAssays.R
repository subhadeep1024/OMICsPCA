

integrateAssays <- function(Assays, name, exclude,
                        mergetype = 1, ...){

    all <- data.frame(x = rep(0, nrow(eval(parse(text = paste0(
    name,"@InputAssays$",Assays[1]))))))

    start <- vector()

    end <- vector()

    count = 1
    locatestart <- 0

    for(Assay in Assays){

        table <- eval(parse(text = paste0(name,"@InputAssays$",
        Assay)))

        if(length(exclude[[count]]) >  1){
            table <- eval(parse(text = paste0(
            "table[,-c(", exclude[count],")]")))
        } else if(exclude[[count]] != 0){
                        table <- eval(parse(text = paste0(
                        "table[,-c(", exclude[count],")]")))
                }

        count = count+1
        start <- c(start, locatestart+1)
        end <- c(end, locatestart+ncol(table))
        locatestart <- end
        all <- cbind(all, table)
    }

    start_end <- list(start = start, end = end)

    if(mergetype == 1){
groups <- readline(
prompt = "Enter a vector of strings containing group names: ")

        groups <- eval(parse(text = groups))

        tmp <- eval(parse(text = paste0(name,"@groups$all")))

        tmp <- tmp[tmp$group %in% groups,,drop = FALSE]
        tmp1 <- all[row.names(all) %in% row.names(tmp),-1]
        PCA <- PCA(tmp1, ncp = ncol(tmp1), ...)

        class(PCA) <- "list"
        eval(parse(text = paste0(name,"@integratedPCA <<- PCA")))
    }

    if(mergetype == 2){
        tmp <- eval(parse(text = paste0(name,"@groups$all")))
        tmp <- tmp[,-ncol(tmp)]
        tmp1 <- all[,-1]
        PCA <- PCA(tmp1, ncp = ncol(tmp1), ...)
        class(PCA) <- "list"
        eval(parse(text = paste0(name,"@integratedPCA <<- PCA")))

    }

    return(start_end)
}
