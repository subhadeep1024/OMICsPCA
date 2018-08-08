
create_group <- function(name,
                    group_names = NULL,
                    grouping_factor = NULL,
                    comparison = NULL,
                    condition = NULL,
                    predefinedgroups = FALSE,
                    groupinfo = NULL){
    if(!predefinedgroups){

    factor_name <- grouping_factor
    factor <- eval(parse(text = paste0(name,"@InputAssays$",grouping_factor)))
    factor <- transform(factor, count = rowSums(factor != 0))

    all <- data.frame()

    for(i in seq(from = 1, to = length(group_names))){
        tmp1 <- length(factor)-1

        tmp2 <- paste("factor[factor$count ",
            comparison[i], " " ,condition[i], ",1:tmp1]")

        tmp3 <- eval(parse(text = tmp2))

        tmp4 <- transform(tmp3, group = rep(group_names[i], nrow(tmp3)))

        all <- rbind(all, tmp4)

        eval(parse(text = paste0(name,"@groups[[group_names[i]]] <<- tmp3")))

    }

    if(length(group_names) == 1 && group_names[1] == "all"){
        print('Creating single group of name = all')
    } else {
        eval(parse(text = paste0(name,"@groups[[\"all\"]] <<- all")))
    }

    } else{

        groupnames <- levels(as.factor(groupinfo[,1]))
        for(group in groupnames){
            tmp3 <- groupinfo[groupinfo[,1] == group,,drop = FALSE]
            eval(parse(text = paste0("as.data.frame(",
            name,"@groups[[group]] <<- tmp3",")")))
        }

        eval(parse(text = paste0(name,"@groups[[\"all\"]] <<- groupinfo")))
    }

}


