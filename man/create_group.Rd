\name{create_group}
\alias{create_group}

\title{
Subsets an Assay dataframe into smaller groups
}
\description{
This function subsests an user defined Assay read by "InputOMICs()"
into smaller dataframes according to user supplied instructions.
}
\usage{

create_group(name, group_names = NULL,
                grouping_factor = NULL,
                comparison = NULL,
                condition = NULL,
                predefinedgroups = FALSE,
                groupinfo = NULL)
}

\arguments{
\item{name}{
Name of the "OMICs" object containing the assay data
}
\item{group_names}{
A vector containing the user defined names of the groups to be created
}
\item{grouping_factor}{
name of the dataframe on which grouping will be done
}
\item{comparison}{
A vector of comparison symbols such as >, <, ==, >=, <=,\%in\%,  etc
}

\item{condition}{
A vector of conditions corresponding to `comparison`.
"condition"" should be a vector or range of digits (e.g. c(1,3,7,9)
or 1:5) if\%in\% is chosen as comparison. Otherwise, a single digit
should be chosen.
}
\item{predefinedgroups}{
logical. If set TRUE then "groupinfo" should be supplied instead
of group_names, grouping_factor, comparison and condition
}
\item{groupinfo}{
a dataframe of single column containing the group names.
}
}

\details{
The smaller groups are saved as dataframes in the "groups" slot of the
"OMICS" object
}
\value{
Fills the slot "groups" of the "OMICs" object supplied through
"name" argument.
}
\author{
Subhadeep Das <subhadeep1024@gmail.com>
}

\examples{

Assays <- c("H2az_Cell_wise", "H3k9ac_Cell_wise",
"H3k4me1_Cell_wise", "CAGE_Cell_wise_v2")

OMICsdata <- OMICs()

path = system.file("extdata/rawdata", package = "OMICsPCAdata")


InputOMICs(name = "OMICsdata", Assays = Assays, typeisfile = TRUE,
path = path)

create_group(name = "OMICsdata",
group_names = c("WE","RE","NE","IntE"),
grouping_factor = "CAGE_Cell_wise_v2",
comparison = c(">=","\%in\%","==","\%in\%"),
condition = c("25","1:5","0","6:24"))

}
