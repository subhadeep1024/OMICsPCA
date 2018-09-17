cluster_boxplot <- function(name, Assay, clusterobject,
clustercolumn, choice = NULL){

    specificcluster <- subassay <- clusterdata <- NULL
    variable <- value <- NULL

clusternumber <- levels(as.factor(clusterobject[, ncol(clusterobject)]))
clusternumber <- as.numeric(clusternumber)

    for(i in clusternumber){
specificcluster[[i]] = clusterobject[clusterobject[,clustercolumn] == i,]
    }

    for(j in clusternumber){
assay = name[[Assay]]
subassay[[j]] = assay[rownames(assay) %in% rownames(
specificcluster[[j]]),]
    }

    for(k in clusternumber){
subassay[[k]] = cbind(
subassay[[k]], rep (k,nrow(subassay[[k]])))
    }

    for(l in clusternumber){
clusterdata <- rbind(clusterdata, subassay[[l]])
    }

    names(clusterdata)[ncol(clusterdata)] <- "cluster"

    d <- melt(clusterdata, id.vars  = "cluster")

    p <- ggplot(d,aes(x=variable, y=value))+geom_boxplot(
aes(fill = variable))+ facet_wrap(~cluster, scales="free")

    return(p)
}
