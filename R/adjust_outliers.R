#' Adjusts outliers based off of geom_boxplot
#'
#' This function adjusts outliers using the kNN algorithm
#' @param dataframe Object of data table or dataframe
#' @param column Column in question with double quotes
#' @export adjust_outliers
#' @examples
#' Example <- adjust_outliers()
#' 
#' The original and imputed series can be compared with the below
#' 
#' plot<- Final_Data %>% 
#' ggplot(aes(x=Date,y=Imputed)) + #Where Imputed is the name of the imputed series
#' geom_line(col="black") +
#' geom_point()
#' 
#' comp_plot<- plot + geom_line(aes(x=Date,y=Original), col = "red") + 
#' geom_point() + ggtitle("Original vs Imputed Series") + ylab("Series") #Where Original is the name of the Original series
#'                   
#' ggplotly(comp_plot)
#' 
adjust_outliers <- function(dataframe,column){
  
  geombox<- dataframe %>%
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(mapping = aes_string(y=column)) +
    ggtitle(paste0("Boxplot of", " ",column)) +
    ylab(column)

  stats<- ggplot_build(geombox)$data
  stats<- as.data.table(stats)
  
  Outliers<- as.data.table(stats$outliers)
  names(Outliers)[1]<- "Values"
  
  dataframe[dataframe == 0] <- NA
  
  dataframe_1<- dataframe %>%
    mutate(dataframe[[column]], Imputed = as.numeric(ifelse(dataframe[[column]]>=Outliers$Values, NA, dataframe[[column]]))) 
  
  k<- sqrt(nrow(dataframe_1))  #determine optimal k

  new_data<- data.table(kNN(dataframe_1, variable = "Imputed",k = k, numFun = weightedMean, weightDist=TRUE, trace = FALSE))#perform knn Imputation
  new_data<- data.table(round(new_data$Imputed,digits=0))
  names(new_data)[1]<- paste0(column,"" ,"_Imputed")
  Final_Data<<- cbind(dataframe,new_data)
  
  write.csv(Final_Data,"Final_Data_Original_vs_Imputed.csv")
  
  Final_Data
  
}
