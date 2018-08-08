\name{analyse_variables}
\alias{analyse_variables}

\title{
Quick analysis and visualization of the integrated Assays by
integrate() function.
}

\description{

This function is designed for a quick analysis and visualization
of the data integrated by integrate() function. It takes 3 compulsory
arguments name, Assay and choice. The type of analysis should be selected
through the "choice" argument. This function acts as a wrapper around a
collection of functions of package "factoextra" and "corrplot" and thus
can take additional arguments specific and non-conflicting to such
functions.
}

\usage{
analyse_variables(name, Assay, choice, title = NULL, PC = 1,
var_type = "contrib", ...)
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
1 = variance barplot
2 = Loadings of cell lines on selected PCs
3 = hCorrelation matrix
4 = Squarred loadings of Cell lines on a PC
5 = Contribution of Cell lines on a PC

for details of the choices see the vignette
}
}

\item{title}{
Title of the plot. default = NULL
}
\item{PC}{
used in choice = 4 and 5. Indicates the PC onwhich choice 4 and 5 will depend.
default = 1
}

\item{var_type}{
used in choice = 2. available options are: "coord", "cos2" and "contrib".
default = "contrib"
}

\item{...}{
extra arguments passed through the functions imported from various packages.
See vignettes for details.
}
}

\value{
Displays various plots and tables as per the combination of input arguments
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
#integrate_variables(Assays = c("H2az_Cell_wise",
#"H3k4me1_Cell_wise","H3k9ac_Cell_wise"),
#name = "OMICsdata", groups = c("WE","RE","NE","IntE"),
#scale.unit = FALSE, graph = FALSE)
#
#
#analyse_variables(name = "OMICsdata",
#Assay = "H3k4me1_Cell_wise", choice = 1,
#title = "variance barplot", addlabels = TRUE)
#
#analyse_variables(name = "OMICsdata",
#Assay = "H3k4me1_Cell_wise", choice = 2,
#title = "Loadings of cell lines on PC1 and PC2",
#var_type = "cos2")
#
#analyse_variables(name = "OMICsdata", Assay = "H2az_Cell_wise",
#choice = 3, is.cor = TRUE)
#
#analyse_variables(name = "OMICsdata",
#Assay = "H2az_Cell_wise",choice = 4,
#PC = 1, title = "Squarred loadings of Cell lines on PC1")
#
#analyse_variables(name = "OMICsdata",
#Assay = "H2az_Cell_wise",choice = 5, PC=1,
#title = "Contribution of Cell lines on PC1")

}
