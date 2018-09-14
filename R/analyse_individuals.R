analyse_individuals <- function(name, Assay, choice, PC, group,
groupinfo = NULL, ...)
{

  PCA <- name[[Assay]]

  ordered_group <- groupinfo[match(
    rownames(PCA$ind$coord),rownames(groupinfo)),
    ,drop=FALSE]

  switch(choice,
         "1" = {

           fviz_pca_ind(PCA,
                        geom.ind = "point", # show points only
                        col.ind = ordered_group$group, # color by groups,
                        axes = PC,
                        legend.title = "Groups", ...)
         },

         "2" = {


           plot3d(PCA$ind$coord[,PC], xlab = paste0("PC",PC[1]),
                  ylab = paste0("PC",PC[2]),
                  zlab = paste0("PC",PC[3]), ...)
         }
  )
}
