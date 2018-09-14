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
gridsize = 100, static = FALSE, groupinfo = NULL, ...)
}

\arguments{
\item{name}{
Name of the integrated "PCA" object
}

\item{PC1, PC2}{
Numbers corresponding to the principal components on
which density
is to be calculated
}

\item{group}{
Names of a group
}

\item{gridsize}{
A number used in kernel smoothing. default is 100
}

\item{static}{
Logical if TRUE a static plot is generated. default = FALSE
}
\item{groupinfo}{
same as integrate_variables()
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

data(MultiAssay)
data(groupinfo_ext)

exclude <- list(0,c(1,9))

int_PCA <- integratePCA(Assays = c("H2az",
"H3k9ac"), name = MultiAssay, mergetype = 2,
exclude = exclude, groupinfo = groupinfo_ext,
,graph = FALSE)

name = int_PCA$int_PCA


plot_integrated_density_3D(name = name, PC1 = 1, PC2 = 2,
group = c("WE","RE"), gridsize = 100, static = FALSE,
groupinfo = groupinfo_ext)
}
