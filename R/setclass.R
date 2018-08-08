setClass("OMICs",  representation(InputAssays = "list",
                groups = "list",
                PCA = "list",
                integratedPCA = "list"))

OMICs <- function(InputAssays = list(),
                groups = list(), PCA = list(),
                integratedPCA = list(), ...) {

    new("OMICs", InputAssays = InputAssays,
    groups = groups, PCA = PCA,
    integratedPCA = integratedPCA, ...)
}
