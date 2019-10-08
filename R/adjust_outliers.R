#' Adjusts outliers based off of geom_boxplot
#'
#' This function identifies and plots outliers
#' @param dataframe Object of data table or dataframe
#' @param column Column in questions with double quotes
#' @export adjust_outliers
#' @examples
#' Example <- adjust_outliers(test,"All_Visits")
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
