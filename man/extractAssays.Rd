\name{extractAssay}
\alias{extractAssay}

\title{
Data extraction from OMICs object
}
\description{
Extraction of individuals according to user supplied group and Assay
}
\usage{
extractAssay(name, Assay, groups)
}
\arguments{
    \item{name}{
name of the "OMICs" object from which data is to be retrieved.
}
    \item{Assay}{
name of the "Assay" from which data is to be retrieved.
}
  \item{groups}{
name of the "groups" from which data is to be retrieved.
}
}

\value{
a data frame containing the intended information
}

\author{
Subhadeep Das
}

\examples{

Assays <- c("H2az_Cell_wise", "H3k9ac_Cell_wise",
"H3k4me1_Cell_wise", "CAGE_Cell_wise_v2")
#
#OMICsdata <- OMICs()
#
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
#extractAssay(name = "OMICsdata", Assay = "H2az_Cell_wise",
#groups = c("WE","RE"))
}

