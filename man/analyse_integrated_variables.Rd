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
a list as returned by "integrate_pca()"
}
\item{Assay}{
Name of an Assay. default = "all"
}
\item{name}{
Name of the "PCA" object returned by "integrate_pca()"
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

data(multi_assay)
data(groupinfo_ext)

exclude <- list(0,c(1,9))

int_PCA <- integrate_pca(Assays = c("H2az",
"H3k9ac"), name = multi_assay, mergetype = 2,
exclude = exclude, graph = FALSE)

start_end = int_PCA$start_end

name = int_PCA$int_PCA


analyse_integrated_variables(start_end = start_end, name = name,
choice = 1, title = "variance barplot", Assay = 1, addlabels = TRUE)
}
