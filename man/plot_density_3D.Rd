\name{plot_density_3D}
\alias{plot_density_3D}

\title{
Visualization of 3D Density of various groups on Principal components
}

\description{
This function displays density of individuals from various
groups created by create_group(). This wraps "persp" from
package "graphics" and thus takes additional graphical input
shown in example.
}

\usage{
plot_density_3D(name, Assay, group, PC1 = 1,
PC2 = 2, static = FALSE,
gridsize = 100, ...)
}

\arguments{
\item{name}{
Name of the "OMICs" object containing the assay data
}
\item{Assay}{
Name of an Assay read by "InputOMICs()"
}

\item{group}{
Names of a group created by "create_group()"
}

\item{PC1, PC2}{
Numbers corresponding to the principal components on which density
is to be calculated
}

\item{static}{
Logical if TRUE a static plot is generated. default = FALSE
}

\item{gridsize}{
A number used in kernel smoothing. default is 100
}

\item{...}{
additional arguments passed to base function "persp" from
package "graphics"
}

}

\details{
2D density is calculated using the "kde2d"" function from package
"MASS"" which use kernel density estimation (KDE) to calculate
density of 2D data. If the variance on either or both of the
PCs are 0, the KDE can't be calculated.
}

\author{
Subhadeep Das <subhadeep1024@gmail.com>
}


\value{
Displays 3D density plots of PCs
}

\examples{

Assays <- c("H2az_Cell_wise", "H3k9ac_Cell_wise",
"H3k4me1_Cell_wise", "CAGE_Cell_wise_v2")
#
#OMICsdata <- OMICs()
#
#path = system.file("extdata/data", package = "OMICsPCAdata")
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
#"H3k4me1_Cell_wise","H3k9ac_Cell_wise"),
#name = "OMICsdata", groups = c("WE","RE","NE","IntE"),
#scale.unit = FALSE, graph = FALSE)
#
#plot_density_3D(name = "OMICsdata", Assay = "H2az_Cell_wise",
#group = "WE",  PC1 = 1,PC2 = 2, grid_size = 100, static = FALSE)

}
