\name{integrateAssays}
\alias{integrateAssays}

\title{
Integration of multiple Assays
into linear combinations by PCA.
}
\description{
This function integrates multiple assays read by InputOMICs()
into linear combinations using PCA. Following earlier analyses,
user may want to exclude some variables/columns/cell lines
which should be supplied through the "exclude" argument (see
vignettes for more explanation)

PCA is done by the function PCA() from package "FactoMineR".
Additional arguments may be supplied to PCA() through "..." .
}

\usage{
integrateAssays(Assays, name, exclude, mergetype = 1, ...)
}

\arguments{

\item{Assays}{
A vector of names of an Assays read by "InputOMICs()"
}


\item{name}{
Name of the "OMICs" object containing the assay data
}

\item{exclude}{
A list that indicates which columns to exclude.
See vignettes for details
}

\item{mergetype}{ 2 options :
\preformatted{
1 = subset of groups should be entered by user and PCA is run
on this subset to integrate Assays into PCs

2 = PCA is run on all groups
    }
}

\item{...}{
aditional arguments supplied to function "PCA()" from package "FactoMineR"
}

}



\value{
"integrateAssays()" returns a list containing the start and end
column of each Assay as ordered in the "Assays" argument.
}

\details{
The "mergetype" argument lets user decide whether to combine all groups
or a subset of groups created by "create_group". If mergetype is set to 1,
"integrateAssays" asks for the name of groups which should be a charatcer
or charagter vector (see vignettes for more details).
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
#InputOMICs(name = "OMICsdata", Assays = Assays, typeisfile = TRUE,
#path = path)
#
#create_group(name = "OMICsdata",
#group_names = c("WE","RE","NE","IntE"),
#grouping_factor = "CAGE_Cell_wise_v2",
#comparison = c(">=","\%in\%","==","\%in\%"),
#condition = c("25","1:5","0","6:24"))
#
#exclude <- list(0,c(1,9))
#start_end <- integrateAssays(Assays = c("H2az_Cell_wise", "H3k9ac_Cell_wise"),
#name = "OMICsdata", mergetype = 2,
#exclude = exclude, graph = FALSE)
#
#start_end
}
