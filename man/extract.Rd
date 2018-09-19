\name{extract}
\alias{extract}

\title{
extraction of projected cooordinates from "PCA" object.
}
\description{
funtion to extract projected coordinates of individuals
on principal components.
}
\usage{
extract(name, groups, integrated = TRUE, Assay = NULL,
rand = NULL, PC = c(1,2,3,4), groupinfo = NULL)
}

\arguments{
    \item{name}{
name of the OMICs object.
}
    \item{groups}{
one or more groups created by or supplied through
"create_group()"
}
    \item{integrated}{
whether to extract coordinates from integrated or individual assay.
}
    \item{Assay}{
name of the assay if integrated = FALSE
}
    \item{rand}{
number of rows to be extracted randomly.
}
    \item{PC}{
principal component numbers. The projected informaton on this PCs
will be extracted.
}
\item{groupinfo}{
same as integrate_variables()
}
}

\value{
a data frame of the extracted value.
}

\author{
Subhadeep Das
}

\examples{

data(multi_assay)
data(groupinfo_ext)

exclude <- list(0,c(1,9))

int_PCA <- integrate_pca(Assays = c("H2az",
"H3k9ac"),
groupinfo = groupinfo,
name = multi_assay, mergetype = 2,
exclude = exclude, graph = FALSE)

name = int_PCA$int_PCA

data <- extract(name = name, PC = c(1:4),
groups = c("WE","RE"), integrated = TRUE, rand = 600,
groupinfo = groupinfo_ext)

}
