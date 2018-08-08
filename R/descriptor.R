
descriptor <- function(name, factors,
                groups,
                choice = 1,
                choice2group = NULL,
                title = NULL){

    variable <- value <- exp_group <- no_of_factors <- NULL

    # if(!is.null(grouping_factor)){
    #    factors = c(factors, grouping_factor)
    # }


    tmp2 <- list()

    for(i in seq(from = 1, to = length(factors))){
        tmp1 <- eval(parse(text = paste0(name,"@InputAssays$",factors[i])))
        tmp2[[i]] <- (rowSums(tmp1 != 0)/ncol(tmp1))*100
    }

    tmp3 <- data.frame(tmp2)
    names(tmp3) <- factors

    count <- data.frame()

    for(i in seq(from = 1, to = length(groups))){
        tmp4 <- eval(parse(text = paste0(name,"@groups$",groups[i])))
        tmp5 <- tmp3[row.names(tmp3) %in% row.names(tmp4),]
        tmp5 <- cbind(tmp5,rep(groups[i], nrow(tmp4)))
        names(tmp5)[ncol(tmp5)] <- "group"
        count <- rbind(count, tmp5)
    }


    switch(choice,

        "1" = {

                d <- melt(count[,c(seq(from = 1, to = (ncol(count)-1)),
                ncol(count))],
                id.vars = names(count)[ncol(count)])

                p <- ggplot(d,aes(x=variable, y=value))+geom_boxplot(
                position = position_dodge(width=0.8),
                aes(fill = group))+ facet_wrap( ~ variable,
                scales="free")

                p <- p+xlab("Epigenetic mark") + ylab(
                "percentage of cell types ")+ggtitle(title)+theme(
                plot.title=element_text(hjust=0.5),
                legend.position = "top")

                p
    },

    "2" = {

        group_name <- choice2group

        tmp6 <- count[count$group == group_name, seq(from = 1,
        to = (ncol(count)-2))]
        tmp6 <- transform(tmp6, no_of_factors = rowSums(tmp6 != 0))

        d <- melt(tmp6[tmp6$no_of_factors !=0,],
        id.vars  = "no_of_factors")

        p <- ggplot(d, aes(x=variable, y = value))+geom_boxplot(
        position = position_dodge(width=1),aes(
        fill=as.factor(no_of_factors)))+facet_wrap(~variable, scales = "free")

        p <- p+xlab("Number of factors")+ylab(
        "percentage of cell types")+ggtitle(title)+theme(
        plot.title=element_text(hjust=0.5),legend.position = "top")+guides(
        fill=guide_legend(title="Number of factors acting together"))

        p
    }

    )
}

