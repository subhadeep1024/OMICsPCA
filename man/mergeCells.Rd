\name{mergeCells}
\alias{mergeCells}

\title{
Storage of intersected factors from multiple cell types as columns
into a dataframe
}

\description{
This function takes the output list of the function "intersect" and
stores the intersected value of the factor and annotation files into
a dataframe. The values corrsponding to the entries in annotation file
represents the corresponding value of in each cell line. If an entry
is not found in a factor file, its corresponding value will be 0.
}

\usage{
mergeCells(list, Cells)
}

\arguments{
\item{list}{
Full path of the file containing name of the Annotations (e.g. gene)
in a single column
}
\item{Cells}{
output list of function "integrate()"
}
}

\value{
A dataframe wich rows corresponding to each entry in annotation file and
columns corresponding to the intersected value with the corresponding
annotation in each cell line.
}

\author{
Subhadeep Das
}

\examples{

anno <- system.file("extdata/annotation2/TSS_groups.bed",
package = "OMICsPCAdata")

fact <- system.file("extdata/factors2/demofactor",
package = "OMICsPCAdata")

Cells <- intersect(fact = fact, anno = anno)

list <- system.file("extdata/annotation2/TSS_list",
package = "OMICsPCAdata")

merged_Cells <- mergeCells(list = list, Cells = Cells)

head(merged_Cells)

}
