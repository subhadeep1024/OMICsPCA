plot_density_3D <- function(name, Assay, group, PC1 = 1,
                    PC2 = 2, static = FALSE,
                    gridsize = 100, groupinfo= NULL, ...){


    ind_coord <- name[[Assay]]$ind$coord

    ind_from_group <- groupinfo[
    groupinfo$group %in% group, ,drop = FALSE]

    ind_coord_group <- ind_coord[rownames(ind_from_group),]

    x <- ind_from_group[match(rownames(ind_coord_group),
    rownames(ind_from_group)),,drop=FALSE]

    scores <- ind_coord_group[,c(PC1,PC2)]

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
                                static = FALSE,
                                groupinfo = NULL, ...){

  ind_coord <- name$ind$coord

  ind_from_group <- groupinfo[
    groupinfo$group %in% group, ,drop = FALSE]

  ind_coord_group <- ind_coord[rownames(ind_from_group),]

  x <- ind_from_group[match(rownames(ind_coord_group),
                            rownames(ind_from_group)),,drop=FALSE]

  scores <- ind_coord_group[,c(PC1,PC2)]

  bivn.kde <- kde2d(scores[,1], scores[,2], n = gridsize)

  col1 <- rainbow(length(bivn.kde$z))[rank(bivn.kde$z)]

  main = paste("Density plot of integrated assays on PC", PC1,"and",PC2, sep = " ")

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

