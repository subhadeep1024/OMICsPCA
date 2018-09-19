\name{integrate_variables}
\alias{integrate_variables}

\title{
Integration of an experiment/ Assay done on many Cell lines/ time points
into linear combinations by PCA.
}
\description{

If an assay (e.g. ChIP-seq for the histone modification H3k9ac) is done
on multiple cells/conditions/treatment/time, it might be, sometimes,
necessary to integrate or combine them. Such combination may be done by
several techniques like Principal Component Analysis (PCA) or
Factor Analysis (FA). This function integrates an assay experimented on
multiple cells or conditions into many linear combinations using PCA.
}
\usage{
integrate_variables(Assays, name, groups, groupinfo = NULL, ...)
}

\arguments{

\item{Assays}{
A vector of names of an assays
}
\item{name}{
Name of the "MultiAssayExperiment" object containing the assay data
}

\item{groups}{
Name of all or subset of groups
}
\item{groupinfo}{
output of create_group or similar object containing group information
}
\item{...}{
Additional arguments supplied to "PCA()" imported from "FactoMineR"
}
}

\value{
an object of class "PCA"
}

\author{
Subhadeep Das <subhadeep1024@gmail.com>
}

\examples{

data(multi_assay)
data(groupinfo_ext)

PCAlist <- integrate_variables(Assays = c("H2az","H3k4me1",
"H3k9ac"), name = multi_assay,
groups = c("WE","RE"), groupinfo = groupinfo_ext,
scale.unit = FALSE, graph = FALSE)
}
