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
#library(OMICsPCAdata)

Assays <- c("H2az_Cell_wise", "H3k9ac_Cell_wise",
"H3k4me1_Cell_wise", "CAGE_Cell_wise_v2")

# OMICsdata <- OMICs()
#
# path <- system.file("extdata/groupinfo", package = "OMICsPCAdata")
#
# groupinfo <- read.table(file = path, header = TRUE,
# row.names = 1, sep ="\t", comment.char = "",
# stringsAsFactors = FALSE, quote = "")
#
# create_group(name = "OMICsdata", predefinedgroups = TRUE,
# groupinfo = groupinfo)
#
# integrate_variables(Assays = c("H2az_Cell_wise","H3k4me1_Cell_wise",
# "H3k9ac_Cell_wise"), name = "OMICsdata",
# groups = c("WE","RE","NE","IntE"),scale.unit = FALSE,
#   graph = FALSE)
#
# data <- extract(name = "OMICsdata", PC = c(1:4),
# groups = c("WE","RE"), integrated = TRUE, rand = 500)

# clusters <- cluster(name = data, choice = "density",
# eps = 2, MinPts = 100, graph = TRUE,
# title = "eps = 1 and MinPts = 1.5")

# clustered_data <- as.data.frame(cbind(data,clusters$cluster$cluster))
# bp <- cluster_boxplot(name = "OMICsdata",
# Assay = "H2az_Cell_wise", clusterobject = clustered_data,
# clustercolumn = 5)

#library(ggplot2)

# bp <- bp+xlab("Cell lines") + ylab(
#     "value")+ggtitle(
#     "Distribution of H2az_cell_wise in various clusters")+theme(
#     plot.title=element_text(hjust=0.5),
#     legend.position = "top")+guides(
#     fill=guide_legend("Cell lines"))
#
# bp

}
