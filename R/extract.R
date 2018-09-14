extract <- function(name,groups, integrated = TRUE, Assay = NULL,
                    rand = NULL, PC = c(1,2,3,4),
                    groupinfo = NULL){

  if(integrated){
  ind_coord <- name$ind$coord

  ind_from_group <- groupinfo[
  groupinfo$group %in% groups, ,drop = FALSE]

  ind_coord_group <- ind_coord[rownames(ind_from_group), PC, drop = FALSE]

  x <- ind_from_group[match(rownames(ind_coord_group),
  rownames(ind_from_group)),,drop=FALSE]

# ind_coord_group <- cbind(ind_coord_group, x)

  if(!is.null(rand)){
        ind_coord_group[sample(seq(from = 1,
        to = nrow(ind_coord_group)), rand),, drop = FALSE]
  }
  } else{

  ind_coord <- name[[Assay]]$ind$coord

  ind_from_group <- groupinfo[
  groupinfo$group %in% groups, ,drop = FALSE]

  ind_coord_group <- ind_coord[rownames(ind_from_group), PC, drop = FALSE]

  x <- ind_from_group[match(rownames(ind_coord_group),
  rownames(ind_from_group)),,drop=FALSE]

#  ind_coord_group <- cbind(ind_coord_group, x)

  if(!is.null(rand)){
  ind_coord_group[sample(seq(from = 1,
  to = nrow(ind_coord_group)), rand),, drop = FALSE]
  }
  }
}
