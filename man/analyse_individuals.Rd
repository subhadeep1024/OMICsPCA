\name{analyse_individuals}
\alias{analyse_individuals}

\title{
Quick analysis and visualization of the individuals/annotations/rows
(e.g. Tss, gene) from integrated Assay.
}

\description{
This function displays scatterplot of the individuals from integrated assays.
This allows the display of scatterplots both in 2D and 3D.
}

\usage{
analyse_individuals(name,Assay, choice, PC, group, ...)
}

\arguments{
\item{name}{
Name of the "OMICs" object containing the assay data
}
\item{Assay}{
Name of an Assay read by "InputOMICs()"
}
\item{choice}{
\preformatted{
1 = 2D scatterplot on selected PCs through PC argument.
2 = 3D scatterplot on selected PCs through PC argument

for details of the choices see the vignette
}
}

\item{PC}{
A vector of numbers corresponding to principal components
}

\item{group}{
A vector of group names as created by "create_group()";
required only if choice = 2
}

\item{...}{
additional arguments allowed to base function "plot3d" of
package "rgl"
}
}

\value{
Displays various plots and tables as per the combination of
input arguments
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
#"H3k4me1_Cell_wise","H3k9ac_Cell_wise"),
#name = "OMICsdata", groups = c("WE","RE","NE","IntE"),
#scale.unit = FALSE, graph = FALSE)
#
#analyse_individuals(name = "OMICsdata",
#Assay = "H3k4me1_Cell_wise",
#choice = 1, PC = c(1,2))
#
#analyse_individuals(name = "OMICsdata",
#Assay = "H3k4me1_Cell_wise",group = c("WE", "RE"),
#choice = 2, PC = c(1,2,3), col = c("RED", "BLACK"))
#

}
