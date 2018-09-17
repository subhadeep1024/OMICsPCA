\name{extract_assay}
\alias{extract_assay}

\title{
Data extraction from "MultiAssayExperiment" object
}
\description{
Extraction of individuals according to user supplied group and Assay
}
\usage{
extract_assay(name, Assay, groups, groupinfo = NULL,
addgroupnames = TRUE)
}
\arguments{
    \item{name}{
name of the "OMICs" object from which data is to be retrieved.
}
    \item{Assay}{
name of the "Assay" from which data is to be retrieved.
}
    \item{groups}{
name of the "groups" from which data is to be retrieved.
}
    \item{groupinfo}{
    output of create_group() or similar object
    }
    \item{addgroupnames}{
    logical argument. If "TRUE", adds the group membership info
    of returned individuals
    }
}

\value{
a data frame containing the intended information
}

\author{
Subhadeep Das
}

\examples{

data(MultiAssay)
data(groupinfo_ext)

x <- extract_assay(name = MultiAssay, Assay = "H2az",
groups = c("WE","RE"), groupinfo = groupinfo_ext)

x

}
