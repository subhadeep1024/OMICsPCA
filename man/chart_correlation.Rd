\name{chart_correlation}
\alias{chart_correlation}

\title{
Pairwise correlation, scatter plot and histogram on selected groups.
}
\description{

This function creates pairwise correlation plots and tables;
scatter plots and histograms on selected groups. The type of
results should be passed through the argument `choice`. This
is a wrapper on various functions and thus can take additional
and non-conflicting arguments specific to them.
}
\usage{
chart_correlation(name, Assay, groups, choice,
groupinfo = NULL,
...)
}

\arguments{
\item{name}{
Name of the "MultiAssayExperiment" object containing the assay data
}
\item{Assay}{
Name of an assay
}
\item{groups}{
Name of all or subset of groups
}

\item{choice}{
\preformatted{

"table" = correlation table

"scatter" = scatterplot of each pair of columns/variables of
selected groups

"hist" = histogram of each column/variables

"all" = all of 1,2,3 together

}
}
\item{groupinfo}{
output of create_group or similar object.
}
\item{...}{
additional arguments for base functions "chart.Correlation"
and "pairs"
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

chart_correlation(name = multi_assay,
Assay = "H2az", groupinfo = groupinfo_ext,
groups = "WE", choice = "table")

}
