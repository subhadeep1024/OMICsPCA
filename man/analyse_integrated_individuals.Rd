\name{analyse_integrated_individuals}
\alias{analyse_integrated_individuals}

\title{
Quick analysis and visualization of the individuals/annotations/rows
(e.g. Tss, gene)
}

\description{
This function works similarly as "analyse_individuals()".
The only extra argument needs to be supplied here is "start_end",
(if choice = 1.) returned by "integrate_pca()"
}

\usage{
analyse_integrated_individuals(
name, choice = 1, geom = "point",
PC = c(1,2), groupinfo = NULL, ...)
}

\arguments{

\item{name}{
Name of the "PCA" object containing the integrated PCA.
}

\item{choice}{
\preformatted{
1 = 2D scatterplot on selected PCs through PC argument.
2 = 3D scatterplot on selected PCs through PC argument

for details of the choices see the vignette
}
}

\item{geom}{
Used when choice = 1. Avalilabe options are : "point", "text",
c("point","text"). default = "point"
}

\item{PC}{
A vector of numbers corresponding to principal components
}

\item{groupinfo}{
same as integrate_variables()
}
\item{...}{
additional arguments allowed to base function "plot3d" of package "rgl"
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

exclude <- list(0,c(1,9))

int_PCA <- integrate_pca(Assays = c("H2az",
"H3k9ac"), name = MultiAssay, mergetype = 2,
exclude = exclude,
groupinfo = groupinfo_ext, graph = FALSE)


name = int_PCA$int_PCA

analyse_integrated_individuals(
name = name,
choice = 2, PC = c(1,2,3),
col = c("RED", "BLACK","GREEN"),
groupinfo = groupinfo_ext)

}
