
InputOMICs <- function(name, Assays, typeisfile = FALSE, path = NULL){

    tmp <- list()

    if(typeisfile == FALSE){

        for(Assay in Assays){
            tmp[[Assay]] <- eval(parse(text = Assay))
        }

        eval(parse(text=paste0(name,"@InputAssays <<- tmp")))
    } else {

    olddir <- setwd(path)

    for(Assay in Assays){

        filename <- paste0(path,"/", Assay)
        if(file.exists(filename)){
        table <- read.table(filename,
        header=TRUE, row.names=1, quote="",
        comment.char="", stringsAsFactors=FALSE)

        tmp[[Assay]] <- table
        } else {
          print (c("file name", Assay, "does not exist"))
        }
    }

    eval(parse(text=paste0(name,"@InputAssays <<- tmp")))

    setwd(olddir)
    }
}
