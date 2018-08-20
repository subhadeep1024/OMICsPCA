analyse_integrated_variables <- function(start_end = start_end, Assay = "all",
name, choice, title = NULL, PC = 1,
var_type = "contrib", ...){


    subset <- eval(parse(text = name))
    if(Assay  != "all"){

        PCA <- subset@integratedPCA
        class(PCA) <- "PCA"
        start <- start_end$start[[Assay]]
        end <- start_end$end[[Assay]]
        PCA$var$coord <- PCA$var$coord[start:end,]
        PCA$var$cor <- PCA$var$cor[start:end,]
        PCA$var$cos2 <- PCA$var$cos2[start:end,]
        PCA$var$contrib <- PCA$var$contrib[start:end,]
    } else {
        PCA <-subset@integratedPCA
        class(PCA) <- "PCA"

    }

    switch(choice,

            "1" = {
                p <- fviz_eig(PCA, main = title, ...)
                print(p)
                t <- get_eig(PCA)
                print(t)
    },

            "2" = {
                p <- fviz_pca_var(PCA, col.var = var_type,
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE,
                title = title, ...)

                print(p)
                print(eval(parse(text = paste0("PCA$var$",var_type))))
                # var_type allowed above: coord, cos2, contrib
            },

            "3" = {
                tmp = PCA$var$cor
                p <- corrplot(tmp, ...)
                print(p)
            },

            "4" = {
                tmp = PCA$var$cos2
                print(tmp)
                fviz_cos2(PCA, choice = "var", axes = PC, title = title, ...)
            },

            "5" = {
                p <- fviz_contrib(PCA, choice = "var", axes = PC,
                title = title,...)

                print(p)
                print(PCA$var$contrib)

            }

    )

}
