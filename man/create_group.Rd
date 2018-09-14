\name{create_group}
\alias{create_group}

\title{
Subsets an Assay dataframe into smaller groups
}
\description{
This function subsests an user defined Assay into smaller
groups according to user supplied instructions.
}
\usage{

create_group(name, group_names = NULL,
                grouping_factor = NULL,
                comparison = NULL,
                condition = NULL
                )
}

\arguments{
\item{name}{
Name of the "MultiAssayExperiment"" object containing the assay data
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
}

\value{
returns the group membership of individuals as a dataframe
}
\author{
Subhadeep Das <subhadeep1024@gmail.com>
}
\examples{

data(MultiAssay)
data(groupinfo_ext)

groupinfo <- create_group(name = MultiAssay,
group_names = c("WE","RE","NE","IntE"),
grouping_factor = "CAGE",
comparison = c(">=","\%in\%","==","\%in\%"),
condition = c("25","1:5","0","6:24"))

head(groupinfo)
}
