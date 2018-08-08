\name{InputOMICs}
\alias{InputOMICs}

\title{
Read data into an object of class "OMICS"
}
\description{
The data is read to the "InputAssays" slot of a "OMICS" object.
The data can be read object diresctly from the environment or
from user defined file.

To read from file we need to pass to additional argument `path`
and `typeisfile` . By default `typeisfile` is set to FALSE and
needs to be changed to TRUE in order to read from file. `path`
should contain the path to the directory containing files to be
read.
}
\usage{

InputOMICs(name, Assays,  typeisfile = FALSE, path = NULL)
}

\arguments{
\item{name}{
name of the OMICS object in which the data is to be read
}
\item{Assays}{
a character string of name of the Assay files
}
\item{typeisfile}{
logical TRUE if type is file. default = FALSE
}
\item{path}{
path to the directory containing files to be read
}
}

\value{
Enters data into the slot "InputAssays".
}

\author{
Subhadeep Das <subhadeep1024@gmail.com>
}

\examples{

library(OMICsPCAdata)

Assays <- c("H2az_Cell_wise", "H3k9ac_Cell_wise", "H3k4me1_Cell_wise",
"CAGE_Cell_wise_v2")

path = system.file("extdata/rawdata", package = "OMICsPCAdata")

##creating object "OMICsdata" of class "OMICS"

OMICsdata <- OMICs()

##inserting data to "OMICS" object

InputOMICs(name = "OMICsdata", Assays = Assays, typeisfile = TRUE,
path = path)


}
