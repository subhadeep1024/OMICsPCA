\name{cluster}
\alias{cluster}

\title{
Cluster data points
}
\description{
Clustering of data points using various algorithms
}
\usage{
cluster(name, n = NULL, graph = NULL, choice, title = NULL, ...)
}
\arguments{
    \item{name}{
name of the dataframe or matrix object containing the coordinates
of data points. The output of "extract()" may be directly put here.
}
    \item{n}{
Number of clusters
}
    \item{graph}{
logical. Plots the clusterplot on first 2 dimensions if set TRUE
}
    \item{choice}{
Clustering algorithm to use. Available choices are:
"density", "kmeans", "pam"
}
    \item{title}{
Title of the plot
}
    \item{\dots}{
additional non-conflicting arguments to cluster functions
}
}

\value{
returns a list containing the cluster and plot information
}
\references{
Martin Ester, Hans-Peter Kriegel, Joerg Sander, Xiaowei Xu (1996).
A Density-Based Algorithm for Discovering Clusters in Large Spatial
Databases with Noise. Institute for Computer Science, University of
Munich. Proceedings of 2nd International Conference on Knowledge
Discovery and Data Mining (KDD-96).

Forgy, E. W. (1965). Cluster analysis of multivariate data:
efficiency vs interpretability of classifications.
Biometrics, 21, 768–769.

Hartigan, J. A. and Wong, M. A. (1979). Algorithm AS 136: A K-means
clustering algorithm. Applied Statistics, 28, 100–108.
doi: 10.2307/2346830.

Lloyd, S. P. (1957, 1982). Least squares quantization in PCM.
Technical Note, Bell Laboratories. Published in 1982 in IEEE
Transactions on Information Theory, 28, 128–137.

MacQueen, J. (1967). Some methods for classification and analysis
of multivariate observations. In Proceedings of the Fifth Berkeley
Symposium on Mathematical Statistics and Probability,
eds L. M. Le Cam & J. Neyman, 1, pp. 281–297. Berkeley,
CA: University of California Press.

}
\author{
Subhadeep Das
}


\examples{

data(MultiAssay)
data(groupinfo_ext)

exclude <- list(0,c(1,9))

int_PCA <- integratePCA(Assays = c("H2az",
"H3k9ac"),
groupinfo = groupinfo,
name = MultiAssay, mergetype = 2,
exclude = exclude, graph = FALSE)

name = int_PCA$int_PCA

data <- extract(name = name, PC = c(1:4),
groups = c("WE","RE"), integrated = TRUE,
rand = 300, groupinfo = groupinfo_ext)

clusters <- cluster(name = data, n = 2,
choice = "kmeans",
title = "kmeans on 2 clusters")

}

\keyword{ density-based clustering }
\keyword{ kmeans clustering }
