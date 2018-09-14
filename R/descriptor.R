descriptor <- function(name, factors,
                       groups,
                       choice = 1,
                       choice2group = NULL,
                       title = NULL,
                       groupinfo = NULL){

percentage <- variable <- value <- no_of_factors <- NULL
group <- NULL

allassays <- assays(name)

selectedassays <- allassays[factors]

for(i in seq(from = 1, to = length(names(selectedassays)), by = 1)){
assay <- selectedassays[[names(selectedassays)[i]]]
percentage[[i]] <- (rowSums(assay != 0)/ncol(assay))*100
}

percentage_dataframe <- data.frame(percentage)
names(percentage_dataframe) <- factors

count <- data.frame()

for(i in seq(from = 1, to = length(groups))){
groupdata <- groupinfo[groupinfo$group == groups[i],,drop = FALSE]

percentagegroup <- percentage_dataframe[row.names(percentage_dataframe)
%in% row.names(groupdata),]

percentagegroup <- cbind(percentagegroup,rep(groups[i],
nrow(groupdata)))

names(percentagegroup)[ncol(percentagegroup)] <- "group"

count <- rbind(count, percentagegroup)

}

switch(choice,

        "1" = {

         d <- melt(count[,c(seq(from = 1, to = (ncol(count)-1)),
                            ncol(count))],
                   id.vars = names(count)[ncol(count)])

         p <- ggplot(d,aes(x=variable, y=value))+geom_boxplot(
           position = position_dodge(width=0.8),
           aes(fill = group))+ facet_wrap( ~ variable,
                                           scales="free")

         p <- p+xlab("Epigenetic mark") + ylab(
           "percentage of cell types ")+ggtitle(title)+theme(
             plot.title=element_text(hjust=0.5),
             legend.position = "top")

         p
       },

       "2" = {

         group_name <- choice2group

         tmp6 <- count[count$group == group_name, seq(from = 1,
                                                      to = (ncol(count)-2))]
         tmp6 <- transform(tmp6, no_of_factors = rowSums(tmp6 != 0))

         d <- melt(tmp6[tmp6$no_of_factors !=0,],
                   id.vars  = "no_of_factors")

         p <- ggplot(d, aes(x=variable, y = value))+geom_boxplot(
           position = position_dodge(width=1),aes(
             fill=as.factor(no_of_factors)))+facet_wrap(~variable, scales = "free")

         p <- p+xlab("Number of factors")+ylab(
           "percentage of cell types")+ggtitle(title)+theme(
             plot.title=element_text(hjust=0.5),legend.position = "top")+guides(
               fill=guide_legend(title="Number of factors acting together"))

         p
       }

)

}
