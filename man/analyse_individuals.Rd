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
analyse_individuals(name,Assay, choice, PC, group,
groupinfo = NULL, ...)
}

\arguments{
\item{name}{
Name of the "PCA" object containing the assay data
}
\item{Assay}{
Name of an Assay
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
A vector of group names.
Required only if choice = 2
}
\item{groupinfo}{
output of create_group, or similar object containing
group information.
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

data(multi_assay)
data(groupinfo_ext)

PCAlist <- integrate_variables(Assays = c("H2az","H3k4me1",
"H3k9ac"), name = multi_assay,
groups = c("WE","RE"), groupinfo = groupinfo_ext,
scale.unit = FALSE, graph = FALSE)

analyse_individuals(name = PCAlist,
Assay = "H3k9ac", groupinfo = groupinfo_ext,
choice = 1, PC = c(1,2))

}
