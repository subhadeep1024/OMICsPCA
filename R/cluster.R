cluster <- function(name, n = NULL, graph = NULL, choice,
title = NULL, ...){

    set.seed(123)
cl <- p <- NULL
#xdim = somdim[1]
#ydim = somdim[2]

    switch (choice,
        "density" = {
            cl <- dbscan(data = name, ...)

            if(graph){
            #print("pltting graph")
            p <- fviz_cluster(cl, data = name, stand = FALSE,
            ellipse = TRUE, show.clust.cent = FALSE,
            geom = "point", palette = "jco", ggtheme = theme_classic(),
            main = title, ...)
            }


        },

        "kmeans" ={
            cl <- kmeans(x = name, centers = n, ...)
            p <- fviz_cluster(cl, data = name, stand = FALSE,
            ellipse = TRUE, show.clust.cent = FALSE,
            geom = "point", palette = "jco", ggtheme = theme_classic(),
            main = title, ...)

            },

        "pam" = {
            cl <- pam(x = name, k = n, ...)
            p <- fviz_cluster(cl, data = name, stand = FALSE,
            ellipse = TRUE, show.clust.cent = FALSE,
            geom = "point", palette = "jco",
            ggtheme = theme_classic(),
            main = title, ...)

        }

    )

    return(list(cluster = cl, plot = p))

}
