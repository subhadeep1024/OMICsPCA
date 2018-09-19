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
Name of the "PCA" object
}
\item{Assay}{
Name of an Assay.
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
used in choice = 4 and 5. Indicates the PC on which choice 4 and
5 will depend. default = 1
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

data(multi_assay)
data(groupinfo_ext)

PCAlist <- integrate_variables(Assays = c("H2az","H3k4me1",
"H3k9ac"), name = multi_assay,
groups = c("WE","RE"), groupinfo = groupinfo_ext,
scale.unit = FALSE, graph = FALSE)

analyse_variables(name = PCAlist, Assay = "H2az", choice = 1,
title = "variance barplot", addlabels = TRUE)

}
