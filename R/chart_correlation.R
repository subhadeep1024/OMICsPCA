chart_correlation <- function(name,
                        Assay,
                        groups,
                        choice = "all", ...){

    key <- NULL
    value <- NULL
    tmp1 <- eval(parse(text = paste0(name,"@groups$all")))
    tmp1 <- tmp1[tmp1$group %in% groups,,drop=FALSE]
    tmp2 <- eval(parse(text = paste0(name,"@InputAssays$",Assay)))
    tmp2 <- tmp2[row.names(tmp2) %in% row.names(tmp1),]


    switch(choice,

        "all" = {
            chart.Correlation(data.matrix(tmp2), histogram = TRUE, ...)
        },

        "table" = {
            cor(tmp2, ...)
        },

        "scatter" = {
            pairs(tmp2, ...)
        },

        "hist" = {
            ggplot(gather(tmp2), aes(value)) +
            geom_histogram(...) +
            facet_wrap(~key, scales = 'free_x')
        },

        print("Invalid selection of choice argument")


    )

}
