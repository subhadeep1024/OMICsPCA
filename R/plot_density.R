
################### plot_density()#########################
plot_density <- function(name, Assay, PC = 1, groups, ...){

    x <- NULL
    tmp2 <- data.frame()


    for(i in groups){
        tmp <- data.frame()

        tmp1 <- data.frame()

        group <- vector()

        tmp <- eval(parse(text = paste0(name,"@groups$all")))

        tmp <- tmp[tmp$group == i,,drop=FALSE]

        tmp1 <- eval(parse(text = paste0(name,"@PCA$",
        Assay,"$ind$coord")))

        tmp1 <- as.data.frame(tmp1[row.names(tmp1)
        %in% row.names(tmp),])

        group = rep(i,nrow(tmp1))

        tmp1 <- cbind(tmp1,group)

        tmp2 <- rbind(tmp2, tmp1)
    }


    label <- paste(Assay,"PC",PC,sep="_")
    tmp2 <- tmp2[,c(PC,ncol(tmp2))]
    names(tmp2)[1] <- "x"

    p <- ggplot(tmp2, aes(x, colour = group)) + geom_density(
    size=1, ...)+labs(x=label, y=NULL)

    print(p)
    return(p)

}
################ End of plot_density()########################


################ plot_integrated_density#####################

plot_integrated_density <- function(name,
                                PC = 1, groups,
                                ...){

    x <- group <- NULL

    tmp2 <- eval(parse(text = paste0(name,"@groups$all")))

    tmp2 <- tmp2[tmp2$group %in% groups,,drop = FALSE]

    label <- paste("integratedAssays_PC",PC,sep="_")

    tmp1 <- eval(parse(text = paste0(name,
    "@integratedPCA$ind$coord")))

    tmp3 <- merge(tmp1, tmp2, by = "row.names")

    row.names(tmp3) <- tmp3[,1]

    tmp3 <- tmp3[,-1]

    tmp3 <- tmp3[,c(PC,ncol(tmp3))]

    names(tmp3)[1] <- "x"

    p <- ggplot(tmp3, aes(x, colour = group)) + geom_density(
    size=1, ...)+labs(x=label, y=NULL)

    print(p)

    return(p)
}
############## End of plot_integrated_density ##############
