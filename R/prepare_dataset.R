

prepare_dataset <- function(factdir, annofile,
                annolist, ...){

    time1 <- Sys.time()
    print("Running intersect... This may take some time")

    Cells <- intersect(fact = factdir, anno = annofile)

    time2 <- Sys.time()
    print("Merging cell lines... This may take some time")

    merged_Cells <- merge_cells(list = annolist, Cells = Cells)
    time3 <- Sys.time()

    print(paste0("Total time taken is: ",time3-time1))

    print(paste0("time taken to run intersect() is: ",
    time2-time1))

    print(paste0("time taken to run merge_cells() is: ",
    time3-time2))

    return(merged_Cells)

}

############# supporting functions######################

intersect <- function(fact, anno, ...){

    value <- NULL
    olddir <- setwd(fact)
    Cells <- list.files()
    path <- anno
    intersected_data <- list()

    for(i in seq(from = 1,to = length(Cells))){
        tmp <- data.table()
        keys <- character()

        code <- R_bedtools_intersect(a = path,
        b = Cells[i], wa =TRUE, wb = TRUE, ...)

        ans <- eval(code)
        tmp <- data.table()

        #### checking for multiple overlaps #####
        # newvalue <- NULL
        # for(value in ans@second$name){
        #     if(grepl(";",value)){
        #         x <- strsplit(value,";")
        # #### average values if overlap found ####
        #         newvalue <- c(newvalue,
        #         mean(as.numeric(x[[1]])))
        #     } else {
        #       newvalue <- c(newvalue, value)
        #     }
        # }
        # name_value <- data.frame(name = ans@first$name,
        # value = newvalue, stringsAsFactors = FALSE)

        name_value <- data.frame(name = ans@first$name,
        value = ans@second$name, stringsAsFactors = FALSE)


        keys <- colnames(name_value)[!grepl('value',
        colnames(name_value))]

        tmp <- as.data.table(name_value)

        tmp$value <- as.numeric(tmp$value)

        tmp <- as.data.frame(tmp[,list(value = mean(value)),
        keys])

        intersected_data[[Cells[i]]] <- tmp

    }

    setwd(olddir)

    return(intersected_data)

}
##############

merge_cells <- function(list, Cells){

    path <- list

    annotation_list <- as.data.frame(readLines(con = path))

    annotation_list = data.frame(
    name = annotation_list$`readLines(con = path)`,
    value = rep(0, nrow(annotation_list)))

    row.names(annotation_list) <- annotation_list$name

    annotation_list = annotation_list[order(
    row.names(annotation_list)),]

    all_cells <- data.frame(rep(0, nrow(annotation_list)))


    for(cellname in names(Cells)){
        singleCell <- data.frame()
        singleCell_all_TSS <- data.frame()
        singleCell <- Cells[[cellname]]

        singleCell_all_TSS <- as.data.frame(
        rbind(singleCell[singleCell$name
        %in% annotation_list$name,],
        annotation_list[!annotation_list$name
        %in% singleCell$name,]))

        row.names(singleCell_all_TSS)<-singleCell_all_TSS$name

        singleCell_all_TSS <- as.data.frame(
        singleCell_all_TSS[order(row.names(
        singleCell_all_TSS)),])

        singleCell_all_TSS <- singleCell_all_TSS[,-1,drop = FALSE]

        all_cells <- cbind(all_cells, singleCell_all_TSS)
    }

    all_cells <- all_cells[,-1]
    names(all_cells) <- names(Cells)
    return(all_cells)

}
############ End of Supporting functions ########################



