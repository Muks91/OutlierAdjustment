#' Identifies and adjust outliers in data
#'
#' This function identifies and plots outliers using geom_boxplot
#' @param dataframe Object of data table or dataframe
#' @param column Column in questions with double quotes
#' @export get_outlier_plot
#' @examples
#' Example <- get_outlier_plot(dataframe,"AllVisits")
get_outlier_plot <- function(dataframe,column){

  geombox<- dataframe %>%
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(mapping = aes_string(y=column)) +
    ggtitle(paste0("Boxplot of", " ",column)) +
    ylab(column)
  p<- plotly_build(geombox)

  for(i in 1:length(p$x$data))
    p$x$data[[i]]$marker$size = 10
  p$x$data[[i]]$marker$line$color = "red"

  p
}
