\name{plot_integrated_density}
\alias{plot_integrated_density}

\title{
Visualization of Density of various groups on Principal components
of integrated assays
}

\description{
This function works similarly as "plot_density()". See the man page
of "plot_density()" for details.
}

\usage{
plot_integrated_density(name, PC = 1, groups, ...)
}

\arguments{
\item{name}{
Name of the "OMICs" object containing the assay data
}

\item{PC}{
A number corresponding to the principal component on which density
is to be calculated. default = 1
}

\item{groups}{
A vector of names or subset of names of groups created by "create_group()"
}

\item{...}{
additional arguments allowed to base function "geom_density" of package
"ggplot2"
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
#exclude <- list(0,c(1,9))
#
#start_end <- integrateAssays(Assays = c("H2az_Cell_wise",
#"H3k9ac_Cell_wise"),
#name = "OMICsdata", mergetype = 2,
#exclude = exclude, graph = FALSE)
#
#densityplot <- plot_integrated_density(name = "OMICsdata",
#PC = 1, groups = c("WE","RE","IntE"))
#
#class(densityplot)
#
#densityplot
}
