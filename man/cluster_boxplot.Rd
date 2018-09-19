\name{cluster_boxplot}
\alias{cluster_boxplot}

\title{
Comparison of clusters by boxplot
}
\description{
boxplot of the assay values in each cluster
}
\usage{
cluster_boxplot(name, Assay, clusterobject, clustercolumn, choice = NULL)
}

\arguments{
    \item{name}{

}
    \item{Assay}{
name of the assay which is to be compared
}
    \item{clusterobject}{
a cluster object returned by the cluster() function
}
    \item{clustercolumn}{
the column in the clusterobject containing the group membership
information
}
    \item{choice}{
type of graph
}
}

\value{
a ggplot object
}

\author{
Subhadeep Das
}

\examples{

data(multi_assay)
data(clustered_data)

bp <- cluster_boxplot(name = multi_assay,
Assay = "H2az", clusterobject = clustered_data,
clustercolumn = 5)


bp

}
