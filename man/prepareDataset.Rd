\name{prepareDataset}
\alias{prepareDataset}

\title{
Intersects and merge data from multiple experiments
into a dataframe
}
\description{
This function intersects feature files (e.g. ChIP-seq, RNA seq)
with annotation files(e.g.exons, genes) and represents the
intersected values of each annotation from all cell lines in a
data frame.
}
\usage{
prepareDataset(factdir, annofile, annolist, ...)
}

\details{
If an annotation (e.g. gene) does not present in an assay
file (e.g ChIP-seq of H3k9ac in the Cell Gm12878), this
function puts a 0 as its value.
}
\arguments{
\item{factdir}{
Full path of directory containing (only) feature (e.g ChIP-seq,
CAGE) files in bed format
}
\item{annofile}{
Full path of the annotation file (e.g. TSS, exon, gene) in bed
format
}
\item{annolist}{
Full path of the file containing name of the annotations
(e.g. name of exons)
}

\item{...}{
additional arguments to pass to  "R_bedtools_intersect()"
through "intersect()"
}

}

\value{
a dataframe of intersected values.
}

\author{
Subhadeep Das <subhadeep1024@gmail.com>
}

\examples{

anno <- system.file("extdata/annotation2/TSS_groups.bed",
                    package = "OMICsPCAdata")

list <- system.file("extdata/annotation2/TSS_list",
                    package = "OMICsPCAdata")

fact <- system.file("extdata/factors2/demofactor",
                    package = "OMICsPCAdata")

prepareDataset(factdir = fact, annofile = anno, annolist = list)

}
