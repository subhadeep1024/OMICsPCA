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
gridsize = 100, groupinfo = NULL, ...)
}

\arguments{
\item{name}{
Name of the "PCA" object containing the assay data
}
\item{Assay}{
Name of an Assay
}

\item{group}{
Names of a group
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
\item{groupinfo}{
Same as integrate_variables()
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

data(MultiAssay)
data(groupinfo_ext)

PCAlist <- integrate_variables(Assays = c("H2az"),
name = MultiAssay,
groups = c("WE","RE"), groupinfo = groupinfo_ext,
scale.unit = FALSE, graph = FALSE)

plot_density_3D(name = PCAlist, Assay = "H2az",
group = "WE", PC1 = 1, PC2 = 2, grid_size = 100,
static = FALSE, groupinfo = groupinfo_ext)
}
