\name{plot_integrated_density_3D}
\alias{plot_integrated_density_3D}

\title{
Visualization of 3D Density of various groups on Principal
components of multiple Assays
}

\description{
works similarly as "plot_density_3D" . See vignettes and
man page of "plot_density_3D" for more details.
}

\usage{
plot_integrated_density_3D(name, PC1 = 1, PC2 = 2, group,
gridsize = 100, static = FALSE, ...)
}

\arguments{
\item{name}{
Name of the "OMICs" object containing the assay data
}

\item{PC1, PC2}{
Numbers corresponding to the principal components on
which density
is to be calculated
}

\item{group}{
Names of a group created by "create_group()"
}

\item{gridsize}{
A number used in kernel smoothing. default is 100
}

\item{static}{
Logical if TRUE a static plot is generated. default = FALSE
}

\item{...}{
additional arguments allowed to base function "persp"
of package "graphics"
}

}

\details{
2D density is calculated using the "kde2d"" function from
package "MASS"" which use kernel density estimation (KDE)
to calculate density of 2D data. If the variance on either
or both of the PCs are 0, the KDE can't be calculated.
}

\value{
Displays 3D density plots.
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
#start_end <- integrateAssays(Assays = c("H2az_Cell_wise", "H3k9ac_Cell_wise"),
#name = "OMICsdata", mergetype = 2,
#exclude = exclude, graph = FALSE)
#
#plot_integrated_density_3D(name = "OMICsdata", PC1 = 1, PC2 = 2, group = "WE",
#gridsize = 100, static = FALSE)
#
#plot_integrated_density_3D(name = "OMICsdata", PC1 = 1, PC2 = 2, group = "WE",
#gridsize = 100, static = TRUE)
}
