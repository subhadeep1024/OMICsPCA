\name{cluster_parameters}
\alias{cluster_parameters}

\title{
Detection of algorithm and number of clusters
}
\description{
Detection of appropriate clustering algorithm and cluster number
for given data using "clvalid" and "NbClust" in background
for cluster validation.
}
\usage{
cluster_parameters(name, comparisonAlgorithm = "clValid",
optimal = FALSE, n = 2:6,
clusteringMethods = c("kmeans", "pam"),
validationMethods = c("internal", "stability"),
distance = "euclidean", ...)
}

\arguments{
    \item{name}{
dataframe returned by "extract()".
}
    \item{optimal}{
logical. If TRUE, returns a dataframe of optimal results
}
    \item{n}{
a vector of numbers corresponding to the number of clusters
to be tested or validated
}

    \item{comparisonAlgorithm}{
2 choices available: "clValid" or "NbClust"
}

    \item{clusteringMethods}{
a vector of single or multiple names of clustering algorithms.
available choices are:

1) if comparisonAlgorithm = "clValid" :
"hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota",
"pam", "clara" and "agnes"

2) if comparisonAlgorithm = "NbClust" :
"ward.D", "ward.D2", "single", "complete", "average", "mcquitty",
"median", "centroid", "kmeans".

}
    \item{validationMethods}{
name of the method to validate clusters. Available options
(one or more):

1) if comparisonAlgorithm = "clValid" then one of:

"kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw",
"tracew", "friedman", "rubin", "cindex", "db", "silhouette",
"duda", "pseudot2", "beale", "ratkowsky", "ball", "ptbiserial",
"gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn",
"hubert", "sdindex", "dindex", "sdbw", "all"
(all indices except GAP, Gamma, Gplus and Tau),
"alllong" (all indices with Gap, Gamma, Gplus and Tau included).


2) if comparisonAlgorithm = "NbClust" then one or more of:

"internal", "stability", and "biological"

}
    \item{distance}{
metric used to calculate distance matrix. options:

1) for "clValid" :

"euclidean", "correlation", and "manhattan".

2) for "NbClust" :

This must be one of: "euclidean", "maximum", "manhattan",
"canberra", "binary", "minkowski"

}

    \item{\dots}{
additional non-conflicting arguments to "clValid" or
"Nbclust"
}
}

\value{
1) for "clValid"

an object of class "clValid" (optimal = FALSE) or a
dataframe of optimal values (optimal = TRUE)

2) for "NbClust"

a list of :

All.index, All.CriticalValues, Best.nc and Best.partition.

See the help pages of "clValid" (?clValid) and "NbClust"
(?NbClust) for more details.

}
\references{
Brock, G., Pihur, V., Datta, S. and Datta, S. (2008) clValid:
An R Package for Cluster Validation Journal of Statistical Software
25(4) http://www.jstatsoft.org/v25/i04

Charrad M., Ghazzali N., Boiteau V., Niknafs A. (2014). "NbClust:
An R Package for Determining the Relevant Number of Clusters in a
Data Set.", "Journal of Statistical Software, 61(6), 1-36.",
"URL http://www.jstatsoft.org/v61/i06/".
}
\author{
Subhadeep Das
}

\examples{

data(MultiAssay)
data(groupinfo_ext)

exclude <- list(0,c(1,9))

int_PCA <- integrate_pca(Assays = c("H2az",
"H3k9ac"),
groupinfo = groupinfo,
name = MultiAssay, mergetype = 2,
exclude = exclude, graph = FALSE)

name = int_PCA$int_PCA

data <- extract(name = name, PC = c(1:4),
groups = c("WE","RE"), integrated = TRUE, rand = 600,
groupinfo = groupinfo_ext)

#### Using "clValid" ####

clusterstats <- cluster_parameters(name = data,
optimal = FALSE, n = 2:4, comparisonAlgorithm = "clValid",
distance = "euclidean", clusteringMethods = c("kmeans"),
validationMethods = c("internal"))

}
