\name{plot_density}
\alias{plot_density}

\title{
Visualization of Density of various groups on Principal components
}

\description{
This function displays density of individuals from various groups
created by create_group()
}

\usage{
plot_density(name, Assay, PC = 1, groups, ...)
}

\arguments{
\item{name}{
Name of the "OMICs" object containing the assay data
}
\item{Assay}{
Name of an Assay read by "InputOMICs()"
}
\item{PC}{
A number corresponding to the principal component on which density
is to be calculated
}

\item{groups}{
A vector of names or subset of names of groups created by "create_group()"
}

\item{...}{
additional arguments of base function "geom_density" from package "ggplot2"
}
}

\value{
a "gg" "ggplot" object
}

\author{
Subhadeep Das <subhadeep1024@gmail.com>
}

\examples{

Assays <- c("H2az_Cell_wise", "H3k9ac_Cell_wise",
"H3k4me1_Cell_wise", "CAGE_Cell_wise_v2")
#
#OMICsdata <- OMICs()
#
#path = system.file("extdata/data", package = "OMICsPCAdata")
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
#integrate_variables(Assays = c("H2az_Cell_wise","H3k4me1_Cell_wise",
#"H3k9ac_Cell_wise"),
#
#name = "OMICsdata", groups = c("WE","RE","NE","IntE"),
#scale.unit = FALSE, graph = FALSE)
#
#plot_density(name = "OMICsdata", Assay = "H2az_Cell_wise",
#PC = 1, groups = c("WE","RE","NE","IntE"),
#adjust = 1)

}
