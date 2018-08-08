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
chart_correlation(name, Assay, groups, choice, ...)
}

\arguments{
\item{name}{
Name of the "OMICs" object containing the assay data
}
\item{Assay}{
Name of an Assays read by "InputOMICs()"
}
\item{groups}{
Name of all or subset of groups crated by "create_group()"
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

\item{...}{
additional arguments for base functions "chart.Correlation" and "pairs"
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

Assays <- c("H2az_Cell_wise", "H3k9ac_Cell_wise",
"H3k4me1_Cell_wise", "CAGE_Cell_wise_v2")
#
#OMICsdata <- OMICs()
#path = system.file("extdata/data", package = "OMICsPCAdata")
#
#
#InputOMICs(name = "OMICsdata", Assays = Assays, typeisfile = TRUE,
#path = path)
#
#create_group(name = "OMICsdata",
#group_names = c("WE","RE","NE","IntE"),
#grouping_factor = "CAGE_Cell_wise_v2",
#comparison = c(">=","\%in\%","==","\%in\%"),
#condition = c("25","1:5","0","6:24"))
#
#
#chart_correlation(name = "OMICsdata", Assay = "H2az_Cell_wise",
#groups = "WE", choice = "scatter")
#
#chart_correlation(name = "OMICsdata", Assay = "H2az_Cell_wise",
#groups = "WE", choice = "table")
#
#chart_correlation(name = "OMICsdata", Assay = "H2az_Cell_wise",
#groups = "WE", choice = "hist", bins = 10)
#
#chart_correlation(name = "OMICsdata", Assay = "H2az_Cell_wise",
#groups = "WE", choice = "all")

}
