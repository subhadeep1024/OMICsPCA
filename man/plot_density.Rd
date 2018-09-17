\name{plot_density}
\alias{plot_density}

\title{
Visualization of Density of various groups on Principal components
}

\description{
This function displays density of individuals
}

\usage{
plot_density(name, Assay, PC = 1, groups,
groupinfo = NULL, ...)
}

\arguments{
\item{name}{
Name of the "PCA" object containing the assay data
}
\item{Assay}{
Name of an Assay
}
\item{PC}{
A number corresponding to the principal component on which density
is to be calculated
}

\item{groups}{
A vector of names or subset of names of groups
}
\item{groupinfo}{
same as integrate_variables()
}
\item{...}{
additional arguments of base function "geom_density" from package "ggplot2"
}
}

\value{
a "gg" "ggplot" object
}

\author{
Subhadeep Das <subhadeep1024@gmail.com>
}

\examples{

data(MultiAssay)
data(groupinfo_ext)

PCAlist <- integrate_variables(Assays = c("H2az","H3k4me1",
"H3k9ac"), name = MultiAssay,
groups = c("WE","RE"), groupinfo = groupinfo_ext,
scale.unit = FALSE, graph = FALSE)

densityplot <- plot_density(name = PCAlist,
Assay = "H2az", groupinfo = groupinfo_ext,
PC = 1, groups = c("WE","RE"),
adjust = 1)

}
