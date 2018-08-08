plot_density_3D <- function(name, Assay, group, PC1 = 1,
                    PC2 = 2, static = FALSE,
                    gridsize = 100, ...){

    tmp <- data.frame()

    tmp1 <- data.frame()


    tmp <- eval(parse(text = paste0(name,"@groups$all")))

    tmp <- tmp[tmp$group == group,,drop=FALSE]

    tmp1 <- eval(parse(text = paste0(name,
    "@PCA$",Assay,"$ind$coord")))

    scores <- as.data.frame(tmp1[row.names(
    tmp1) %in% row.names(tmp),,drop=FALSE])

    bivn.kde <- kde2d(scores[,1], scores[,2], n = gridsize)

    col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]

    main = paste("Density plot of",group,"for the assay",
    Assay,"on PC", PC1,"and",PC2, sep = " ")

    labx = paste("PC",PC1)

    laby = paste("PC",PC2)

    labz = "density"

    if(static == FALSE){

        persp3d(x=bivn.kde, col = col1, xlab = labx,
        ylab = laby, zlab = labz)

    } else {

        persp(x = bivn.kde, col = col1, xlab = labx,
        ylab = laby, zlab = labz, ...)
    }
}

############ End of plot_integrated_density################

plot_integrated_density_3D <- function(name, PC1 = 1,
                                PC2 = 2, group,
                                gridsize = 100,
                                static = FALSE, ...){

    tmp2 <- eval(parse(text = paste0(name,"@groups$all")))

    tmp2 <- tmp2[tmp2$group %in% group,,drop=FALSE]

    labx = paste("PC",PC1)

    laby = paste("PC",PC2)

    labz = "density"

    tmp1 <- eval(parse(text = paste0(name,
    "@integratedPCA$ind$coord")))

    tmp3 <- merge(tmp1, tmp2, by = "row.names")

    row.names(tmp3) <- tmp3[,1]

    tmp3 <- tmp3[,-1]

    scores <- as.data.frame(tmp3[,c(PC1,PC2)])

    bivn.kde <- kde2d(scores[,1], scores[,2], n = gridsize)

    col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]

    if(static == FALSE){
        persp3d(x=bivn.kde, col = col1, xlab = labx,
        ylab = laby, zlab = labz)

    } else {
        persp(x = bivn.kde, col = col1, xlab = labx,
        ylab = laby, zlab = labz, ...)
    }
}
