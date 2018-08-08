analyse_integrated_individuals <- function(start_end = start_end,
                                name, choice = 1, geom = "point",
                                PC = c(1,2), group = NULL, ...)
    {
        PCA <- eval(parse(text = paste0(name,"@integratedPCA")))
        class(PCA) <- "PCA"
        switch(choice,
                "1" = {
                        PC_coord <- PCA$ind$coord

                        normal_coord  <- eval(parse(text=paste0(
                        name,"@groups$all")))

                        groups = normal_coord[row.names(normal_coord) %in%
                        row.names(PC_coord),"group"]

                        fviz_pca_ind(PCA,
                        geom.ind = geom, # show points only
                        col.ind = groups, # color by groups,
                        axes = PC,
                        legend.title = "Groups", ...)
                },


                "2" = {
                        tmp <- data.frame()

                        tmp1 <- data.frame()

                        tmp <- eval(parse(text = paste0(name,"@groups$all")))

                        tmp <- tmp[tmp$group %in% group,,drop = FALSE]

                        tmp1 <- PCA$ind$coord

                        scores <- as.data.frame(tmp1[row.names(tmp1)
                        %in% row.names(tmp),])

                        plot3d(scores[,PC], xlab = paste0("PC",PC[1]),
                        ylab = paste0("PC",PC[2]),zlab = paste0("PC",PC[3]),
                        ...)
                }
    )

}
