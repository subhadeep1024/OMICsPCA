analyse_individuals <- function(name, Assay, choice, PC, group, ...)
{

    PCA <- eval(parse(text=paste0(name,"@PCA$",Assay)))

    switch(choice,
        "1" = {
                rownames <- eval(parse(text=paste0("row.names(",name,"@PCA$",
                Assay,"$ind$coord)")))
                tmp  <- eval(parse(text=paste0(name,"@groups$all")))
                group <- tmp[row.names(tmp) %in% rownames,
                "group"]
                fviz_pca_ind(PCA,
                    geom.ind = "point", # show points only
                    col.ind = group, # color by groups,
                    axes = PC,
                    legend.title = "Groups", ...)
        },

        "2" = {
                tmp <- data.frame()
                tmp1 <- data.frame()
                tmp <- eval(parse(text = paste0(name,"@groups$all")))
                tmp <- tmp[tmp$group %in% group, ,drop = FALSE]
                tmp1 <- PCA$ind$coord
                scores <- as.data.frame(
                tmp1[row.names(tmp1) %in% row.names(tmp),])
                plot3d(scores[,PC], xlab = paste0("PC",PC[1]),
                ylab = paste0("PC",PC[2]),
                zlab = paste0("PC",PC[3]), ...)
        }
    )
}
