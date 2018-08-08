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
integrate_variables(Assays, name, groups, ...)
}

\arguments{

\item{Assays}{
A vector of names of an Assays read by "InputOMICs()"
}

\item{name}{
Name of the "OMICs" object containing the assay data
}

\item{groups}{
Name of all or subset of groups crated by "create_group()"
}

\item{...}{
Additional arguments supplied to "PCA()" imported from "FactoMineR"
}
}

\value{
Enters value to the slot "PCA" of the "OMICs" object.
}

\author{
Subhadeep Das <subhadeep1024@gmail.com>
}

\examples{

Assays <- c("H2az_Cell_wise", "H3k9ac_Cell_wise",
"H3k4me1_Cell_wise", "CAGE_Cell_wise_v2")
#
#OMICsdata <- OMICs()
#path = system.file("extdata/data", package = "OMICsPCAdata")
#
#
#InputOMICs(name = "OMICsdata", Assays = Assays,
#typeisfile = TRUE, path = path)
#
#create_group(name = "OMICsdata",
#group_names = c("WE","RE","NE","IntE"),
#grouping_factor = "CAGE_Cell_wise_v2",
#comparison = c(">=","\%in\%","==","\%in\%"),
#condition = c("25","1:5","0","6:24"))
#
#integrate_variables(Assays = c("H2az_Cell_wise",
#"H3k4me1_Cell_wise","H3k9ac_Cell_wise"), name = "OMICsdata",
#groups = c("WE","RE", "NE","IntE"),scale.unit = FALSE,
#graph = FALSE)
}
