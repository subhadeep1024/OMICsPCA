\name{extract}
\alias{extract}

\title{
extraction of projected cooordinates from OMICs object.
}
\description{
funtion to extract projected coordinates of individuals
on principal components.
}
\usage{
extract(name, groups, integrated = TRUE, Assay = NULL,
rand = NULL, PC = c(1,2,3,4))
}

\arguments{
    \item{name}{
name of the OMICs object.
}
    \item{groups}{
one or more groups created by or supplied through
"create_group()"
}
    \item{integrated}{
whether to extract coordinates from integrated or individual assay.
}
    \item{Assay}{
name of the assay if integrated = FALSE
}
    \item{rand}{
number of rows to be extracted randomly.
}
    \item{PC}{
principal component numbers. The projected informaton on this PCs
will be extracted.
}
}

\value{
a data frame of the extracted value.
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
# data <- extract(name = "OMICsdata", PC = c(1,2,3,4),
# groups = c("WE","RE"), integrated = TRUE, rand = 500)

}
