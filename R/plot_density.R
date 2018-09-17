
################### plot_density()#########################
plot_density <- function(name, Assay, PC = 1, groups,
groupinfo = NULL, ...){

    x <- group <- NULL

    ind_coord <- name[[Assay]]$ind$coord

    ind_from_group <- groupinfo[
    groupinfo$group %in% groups, ,drop = FALSE]

    ind_coord_group <- ind_coord[rownames(ind_from_group),]

    x <- ind_from_group[match(rownames(ind_coord_group),
    rownames(ind_from_group)),,drop=FALSE]

    ind_coord_group <- cbind(ind_coord_group,x)

    label <- paste(Assay,"PC",PC,sep="_")

    ind_coord_group <- ind_coord_group[,c(PC,
    ncol(ind_coord_group))]

    names(ind_coord_group)[1] <- "x"

    p <- ggplot(ind_coord_group, aes(x, colour = group)) + geom_density(
    size=1, ...)+labs(x=label, y=NULL)

    print(p)
    return(p)

}
################ End of plot_density()########################


################ plot_integrated_density#####################

plot_integrated_density <- function(name,
PC = 1, groups, groupinfo = NULL, ...){

  x <- group <- NULL

  ind_coord <- name$ind$coord

  ind_from_group <- groupinfo[
  groupinfo$group %in% groups, ,drop = FALSE]

  ind_coord_group <- ind_coord[rownames(ind_from_group),]

  x <- ind_from_group[match(rownames(ind_coord_group),
  rownames(ind_from_group)),,drop=FALSE]

  ind_coord_group <- cbind(ind_coord_group,x)

  #label <- paste(Assay,"PC",PC,sep="_")

  ind_coord_group <- ind_coord_group[,c(PC,
  ncol(ind_coord_group))]

  names(ind_coord_group)[1] <- "x"

  p <- ggplot(ind_coord_group, aes(x, colour = group)) + geom_density(
    size=1, ...)

  print(p)
  return(p)

}
############## End of plot_integrated_density ##############
