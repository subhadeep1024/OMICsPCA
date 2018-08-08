\name{intersect}
\alias{intersect}

\title{
support function of prepareDataset
}
\description{
intersects a factor file and an annotation file
}
\usage{
intersect(fact, anno, ...)
}

\arguments{
\item{fact}{
Full path of the directory containing only the bed (.bed) files
corresponding to several cell lines of the specified factor.
}
\item{anno}{
Full path of the annotation file (e.g. gene, TSS, exon etc.)
}
\item{...}{
additional arguments received from "prepareDataset()" and
suppliet to  "R_bedtools_intersect()"
}
}

\value{
A list containing the intersections of the files (paths of which are
supplied throgh the arguments).
}
\author{
Subhadeep Das
}

\examples{

anno <- system.file("extdata/annotation/TSS_groups.bed",
package = "OMICsPCAdata")
#
#fact <- system.file("extdata/factors/H2az", package = "OMICsPCAdata")

#intersect(fact = fact, anno = anno)
}
