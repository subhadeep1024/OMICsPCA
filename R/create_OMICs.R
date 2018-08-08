
create_OMICs <- function(){

    setClass("OMICs",  representation(InputAssays = "list",
            groups = "list", group_names = "character"))

    x <- new("OMICs")

    return(x)
}






