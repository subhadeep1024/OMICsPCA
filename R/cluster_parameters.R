cluster_parameters <- function(name,
comparisonAlgorithm = "clValid",
optimal = FALSE,
n = 2:6, clusteringMethods = c("kmeans","pam"),
validationMethods = c("internal","stability"),
distance = "euclidean",  ...){


min.nc = min(n)
max.nc = max(n)
clusterstats <- NULL

    set.seed(123)


switch(comparisonAlgorithm,

        "clValid" = {
        # if(is.null(rand)){
        clusterstats <- clValid(name, n,
        clMethods=clusteringMethods,
        validation=validationMethods,
        metric = distance, ...)
        summary(clusterstats)
        # }  else {
        # clusterstats <- clValid(data, n,
        # clMethods=clusteringMethods,
        # validation=validationMethods,
        # metric = distance,
        # maxitems = rand, ...)
        # #summary(clusterstats)
        # }

        if(optimal){
        clusterstats <- optimalScores(clusterstats)

        }
        },

        "NbClust" = {
          clusterstats <- NbClust(name, distance =  distance,
          min.nc = min.nc, max.nc = max.nc,
          index = validationMethods,
          method = clusteringMethods)
          #clusterstats <- t(clusterstats$Best.nc)
        }
)

    return(clusterstats)
}

