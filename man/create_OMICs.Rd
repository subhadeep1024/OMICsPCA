\name{create_OMICs}
\alias{create_OMICs}
\alias{OMICs}

\title{
constructor function of class "OMICs"
}
\description{
Creates an object of class "OMICs"
}
\usage{

create_OMICs()

OMICs(InputAssays = list(), groups = list(),
    PCA = list(), integratedPCA = list(), ...)
}

\arguments{
\item{InputAssays}{
Default = an empty list. This is a slot in object type "OMICs",
which is supposed to be filled by the function call to
"InputOMICs()"
}

\item{groups}{
Default = an empty list. This is a slot in object type "OMICs",
which is supposed to be filled by the function call to
"create_group()"
}

\item{PCA}{
Default = an empty list. This is a slot in object type "OMICs",
which is supposed to be filled by the function call to
"integrate_variables()"
}

\item{integratedPCA}{
Default = an empty list. This is a slot in object type "OMICs",
which is supposed to be filled by the function call to
"integrateAssays()"
}

\item{...}{
aditional arguments to "new()"
}

}
\value{
An object of class "OMICs"
}

\author{
Subhadeep Das
}

\examples{

#x <- create_OMICs()
x <- OMICs()

}
