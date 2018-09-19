\name{integrate_pca}
\alias{integrate_pca}

\title{
Integration of multiple Assays
into linear combinations by PCA.
}
\description{
This function integrates multiple assays read by InputOMICs()
into linear combinations using PCA. Following earlier analyses,
user may want to exclude some variables/columns/cell lines
which should be supplied through the "exclude" argument (see
vignettes for more explanation)

PCA is done by the function PCA() from package "FactoMineR".
Additional arguments may be supplied to PCA() through "..." .
}

\usage{
integrate_pca(Assays, name, exclude, mergetype = 1,
groupinfo = NULL, ...)
}

\arguments{

\item{Assays}{
A vector of names of an Assays read by "InputOMICs()"
}


\item{name}{
Name of the "MultiAssayExperiment" object containing the assay data
}

\item{exclude}{
A list that indicates which columns to exclude.
See vignettes for details
}

\item{mergetype}{ 2 options :
\preformatted{
1 = subset of groups should be entered by user and PCA is run
on this subset to integrate Assays into PCs

2 = PCA is run on all groups
    }
}
\item{groupinfo}{
same as plot_density()
}
\item{...}{
aditional arguments supplied to function "PCA()" from package "FactoMineR"
}

}

\value{
"integrateAssays()" returns a list containing the start and end
column of each Assay as ordered in the "Assays" argument.
}

\details{
The "mergetype" argument lets user decide whether to combine all groups
or a subset of groups created by "create_group". If mergetype is set to 1,
"integrateAssays" asks for the name of groups which should be a charatcer
or charagter vector (see vignettes for more details).
}

\author{
Subhadeep Das <subhadeep1024@gmail.com>
}

\examples{

data(multi_assay)
data(groupinfo_ext)

int_PCA <- integrate_pca(Assays = c("H2az",
"H3k9ac"), name = multi_assay, mergetype = 2,
exclude = list(0,c(1,9)), graph = FALSE,
groupinfo = groupinfo_ext)
}
