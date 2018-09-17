
create_group <- function(name,
                    group_names = NULL,
                    grouping_factor = NULL,
                    comparison = NULL,
                    condition = NULL){

    allassays <- assays(name)
    factor <- allassays[[grouping_factor]]
    factor <- transform(factor, count = rowSums(factor != 0))

    all <- data.frame()

    for(i in seq(from = 1, to = length(group_names))){
        tmp1 <- length(factor)-1

        tmp2 <- paste("factor[factor$count ",
            comparison[i], " " ,condition[i], ",1:tmp1]")

        tmp3 <- eval(parse(text = tmp2))

        tmp4 <- transform(tmp3, group = rep(group_names[i], nrow(tmp3)))


        all <- rbind(all, tmp4)
    }
        return(all[,ncol(all),drop=FALSE])
}


