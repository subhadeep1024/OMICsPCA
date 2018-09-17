

integrate_pca <- function(Assays, name, exclude,
mergetype = 1, groupinfo = NULL, ...){

    allassays <- assays(name)
    all <- data.frame(x = rep(0, nrow(allassays[[1]])))

    start <- vector()

    end <- vector()

    count = 1
    locatestart <- 0

    for(Assay in Assays){

        table <- allassays[[Assay]]

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

        tmp <- groupinfo

        tmp <- tmp[tmp$group %in% groups,,drop = FALSE]
        tmp1 <- all[row.names(all) %in% row.names(tmp),-1]


        int_PCA <- PCA(tmp1, ncp = ncol(tmp1), ...)
    }

    if(mergetype == 2){

    tmp1 <- all[,-1]
    int_PCA <- PCA(tmp1, ncp = ncol(tmp1), ...)


    }

    return(list(start_end = start_end, int_PCA = int_PCA))
}
