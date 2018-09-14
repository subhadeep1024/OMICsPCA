

analyse_variables <- function(name, Assay, choice, title = NULL,
                              PC = 1, var_type = "contrib", ...){



  if(choice != 6){
    PCA <- name[[Assay]]
    myvar <- get_pca_var(PCA)
  }
  title = title
  mydata <- NULL


  switch(choice,

         "1" = {
           p <- fviz_eig(PCA, main = title, ...)
           print(p)
           t <- get_eig(PCA)
           print(t)
         },

         "2" = {
           p <- fviz_pca_var(PCA, col.var = var_type,
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                             repel = TRUE,
                             title = title, ...)

           print(p)

           print(eval(parse(text = paste0("PCA$var$",var_type))))
           # var_type allowed : coord, cos2, contrib
         },

         "3" = {

           tmp = eval(parse(text = paste0("PCA$var$cor")))

           p <- corrplot(tmp, ...)

           print(p)
         },

         "4" = {

           tmp = PCA$var$cos2

           print(tmp)

           fviz_cos2(PCA, choice = "var", axes = PC, title = title, ...)
         },

         "5" = {
           p <- fviz_contrib(PCA, choice = "var",
                             axes = PC, title = title, ...)

           print(p)

           print(myvar$contrib)

         },

         "6" = {

           variable <- value <- NULL
           factor = name[[Assay[1]]]$eig

           mydata <- as.data.frame(factor)[,2, drop = FALSE]
           factornames <- Assay

           mincol <- nrow(mydata)

           Assay = Assay[-1]


           for(factor in Assay){

             tmp <- name[[factor]]$eig[,2]

             mincol <- min(mincol,length(tmp))

             mydata <- as.data.frame(cbind(mydata[seq(
               from = 1 , to = mincol),],tmp[seq(from = 1 ,
                                                 to = mincol)]))
           }

           names(mydata) <- factornames
           mydata <- transform(mydata, PC = rownames(mydata))
           mydata$PC <- factor(mydata$PC,
                               levels = mydata$PC[order(mydata$PC)])
           mydata.m <- melt(mydata)

           ############ heatmap##################################################
           heatmap <- ggplot(mydata.m, aes(variable, PC)) + geom_tile(
             aes(fill = value), color = "white") + scale_fill_gradient(
               low = "grey",high = "steelblue", guide="colorbar") + ylab(
                 "Percentage of variance explained") +
             theme(

               plot.title = element_text(size=17, hjust=0.5),
               axis.title.x = element_blank(),
               axis.text.x = element_text(size=11, angle=60, hjust=1,
                                          vjust=1, color="black"),
               axis.text.y = element_text(size=13, color="black"),
               axis.title.y = element_text(size=13),
               axis.line = element_blank(),
               axis.ticks =  element_blank(),
               panel.background = element_blank()

             )

           heatmap <- heatmap + ggtitle("Variance explained by Principal
                                        components") +
             guides(fill=guide_colorbar("percentage"))


           ############ barplot##############################################
           colsums <- as.data.frame(colSums(mydata[,-ncol(mydata)]))
           colsums <- transform(colsums, name = rownames(colsums))
           names(colsums)[1] <- "y"

           barplot <- ggplot(colsums, aes_string(x = "name", y = "y")) +
             ggtitle("Total variance explained per Assay") +
             geom_bar(stat="identity", fill="steelblue",
                      width=0.8) +xlab("") + ylab(
                        "Total percentage") +
             scale_y_continuous(expand=c(0.01,0.01)) +
             theme(
               plot.margin = unit(c(1,3.5,0,0), "cm"),
               panel.background = element_blank(),
               plot.title = element_text(size=17, hjust=0.5),
               axis.ticks.x = element_blank(),
               axis.text.x = element_blank(),
               axis.text.y = element_text(size=12, color="black"),
               axis.title.y = element_text(size=13, color="black"),
               axis.line = element_line(size=rel(1.0),
                                        color="black")
             )

           ############ both plot together#######################################
           both <- plot_grid(barplot, heatmap, align = "v",
                             nrow = 2, rel_heights=c(1/3,2/3), axis="l")

           print(both)



         }
  )
}

