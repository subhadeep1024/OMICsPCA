plot_density_mm <- function(data, summary = TRUE, ...){
    model <- densityMclust(data)
    if(summary){
        print(summary(model))
    }

    plot(model, ...)
}
