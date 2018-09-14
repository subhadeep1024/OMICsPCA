chart_correlation <- function(name,
Assay, groups, choice = "all",
groupinfo = NULL, ...){

  key <- NULL
  value <- NULL

  allassays <- assays(name)

  selectedassay <- allassays[[Assay]]

  groupdata <- groupinfo[groupinfo$group == groups,,drop = FALSE]

  groupdata_of_selected_assay = selectedassay[
    rownames(selectedassay) %in% rownames(groupdata),]



  switch(choice,

         "all" = {
           chart.Correlation(data.matrix(groupdata_of_selected_assay),
                             histogram = TRUE, ...)
         },

         "table" = {
           cor(groupdata_of_selected_assay, ...)
         },

         "scatter" = {
           pairs(groupdata_of_selected_assay, ...)
         },

         "hist" = {
           ggplot(gather(groupdata_of_selected_assay), aes(value)) +
             geom_histogram(...) +
             facet_wrap(~key, scales = 'free_x')
         },

         print("Invalid selection of choice argument")


  )

}
