\name{plot_integrated_density}
\alias{plot_integrated_density}

\title{
Visualization of Density of various groups on Principal components
of integrated assays
}

\description{
This function works similarly as "plot_density()". See the man page
of "plot_density()" for details.
}

\usage{
plot_integrated_density(name, PC = 1, groups,
groupinfo = NULL, ...)
}

\arguments{
\item{name}{
Name of the integrated "PCA"" object
}

\item{PC}{
A number corresponding to the principal component on which density
is to be calculated. default = 1
}

\item{groups}{
A vector of names or subset of names of groups
}
\item{groupinfo}{
same as plot_density()
}
\item{...}{
additional arguments allowed to base function "geom_density" of package
"ggplot2"
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

exclude <- list(0,c(1,9))

int_PCA <- integratePCA(Assays = c("H2az",
"H3k9ac"), name = MultiAssay, mergetype = 2,
exclude = exclude, groupinfo = groupinfo_ext,
,graph = FALSE)

name = int_PCA$int_PCA

densityplot <- plot_integrated_density(name = name, PC = 1,
groups = c("WE","RE","IntE","NE"), groupinfo = groupinfo_ext)

# additional graphical functions (e.g. xlim, ylim, theme) may be
#added with densityplot (see section VIII. Density analysis)

densityplot
}
