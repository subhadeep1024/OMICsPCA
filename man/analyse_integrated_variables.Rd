\name{analyse_integrated_variables}
\alias{analyse_integrated_variables}

\title{
Quick analysis and visualization of the integrated Assays by
integrate() function.
}

\description{

This function is designed for a quick analysis and visualization
of the data integrated by "integrateAssays()"" function. It works
similarly as analyse_variables. In addition to that it takes an
additinal argument "start_end" returned by "integrateAssays()".
}

\usage{
analyse_integrated_variables(start_end = start_end, Assay = "all",
name, choice, title = NULL, PC = 1,
var_type = "contrib", ...)
}

\arguments{

\item{start_end}{
a list as returned by "integrateAssays()"
}

\item{Assay}{
Name of an Assay read by "InputOMICs()". default = "all"
}


\item{name}{
Name of the "OMICs" object containing the assay data
}

\item{choice}{
\preformatted{
1 = variance barplot
2 = Loadings of cell lines on selected PCs
3 = hCorrelation matrix
4 = Squarred loadings of Cell lines on a PC
5 = Contribution of Cell lines on a PC

for details of the choices see the vignette
}
}

\item{title}{

title of plots
}

\item{PC}{
The PC on which the plots will be drawn. used in choice 4 and 5. default = 1
}

\item{var_type}{
Used when choice = 2. available options are: "coord", "cos2" and "contrib"
}

\item{...}{
additional arguments allowed to base functions "used in various choices.
See vignettes for details.
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
#exclude <- list(0,c(1,9))
#
#start_end <- integrateAssays(Assays = c("H2az_Cell_wise",
#"H3k9ac_Cell_wise"),
#name = "OMICsdata", mergetype = 2,
#exclude = exclude, graph = FALSE)
#
#
#analyse_integrated_variables(start_end = start_end, Assay = 1,
#name = "OMICsdata", choice = 2 ,
#title = "Loadings of cell lines on PC1 and PC2",
#var_type = "contrib")
#
#analyse_integrated_variables(start_end = start_end, Assay = 1,
#name = "OMICsdata", choice = 4, PC = 1,
#title = "Squarred loadings of Cell lines,
#on PC1 of integrated Assays")

}
