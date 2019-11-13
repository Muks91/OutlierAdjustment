#' Adjusts outliers based off of geom_boxplot
#'
#' This function adjusts outliers using the kNN algorithm
#' @param dataframe Object of data table or dataframe
#' @param column Column in question with double quotes
#' @export adjust_outliers_knn
#' @examples
#' Example <- adjust_outliers_knn(dataframe,"AllVisits","impacts")
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
adjust_outliers_knn <- function(dataframe,column){
  
  options(scipen =999)
  
  geombox<- dataframe %>%
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(mapping = aes_string(y=column)) +
    ggtitle(paste0("Boxplot of", " ",column)) +
    ylab(column) #create geom_box plot
  
  stats<- ggplot_build(geombox)$data #obtain outlier values
  stats<- as.data.table(stats)
  
  Outliers<- as.data.table(stats$outliers)
  names(Outliers)[1]<- "Values"
  Outliers<- Outliers[Values!=0,]
  
  Outliers <- tibble::rowid_to_column(Outliers, "ID")
  
  #Add ID if not already present in dataframe
  if("ID" %in% colnames(dataframe)){
    cat("ID already exisits")
  } else {
    dataframe <- tibble::rowid_to_column(dataframe, "ID")
  }
  
  #dataframe <- tibble::rowid_to_column(dataframe, "ID")
  
  d<- dataframe[dataframe[[column]] %in% Outliers$Values,]
  
  Outliers<- mutate(d,Seq = c(0, diff(d$ID, ) > 1))
  
  Outliers<- if(diff(Outliers$ID)>1){
    Outliers %>% 
      mutate(d,Seq = c(1))
  } else {
    print("No sequential outliers")
  }
  
  dk<- Outliers %>% 
    filter(Seq==1)
  
  dataframe_knn<- dataframe %>%
    mutate(dataframe[[column]], Imputed = as.numeric(ifelse(dataframe[[column]] %in% dk[[column]], 0, dataframe[[column]]))) %>% 
    mutate(dataframe[[column2]], dataframe[[column2]])
  
  dataframe_knn<- data.table(dataframe_knn)
  
  dataframe_knn[Imputed == 0 & dataframe[[column]] !=0 , Imputed := NA]
  
  knn_fun<- function(knn_adj) { 
    
    knn_adj<- if(any(is.na(dataframe_knn$Imputed))){
      
      dataframe_knn<- data.table(dataframe_knn)
      
      k<- sqrt(nrow(dataframe))  #determine optimal k
      
      dataframe_k<- dataframe_knn[,c("Imputed","impacts")]
      
      new_data<- data.table(kNN(dataframe_k, variable = "Imputed",k = k, numFun = weightedMean, weightDist=TRUE, trace = FALSE,impNA = TRUE))#perform knn Imputation
      new_data<- data.table(round(new_data$Imputed,digits=0))
      names(new_data)[1]<- paste0(column,"" ,"_Imputed")
      new_data<- cbind(new_data,dataframe)
      
      Final_Data<- new_data %>% 
        select(Date,starts_with(glue::glue({column})),starts_with(glue::glue({column2})))
      
      write.csv(Final_Data,"Optimal_Imputed_Data_kNN_method.csv")
    } else   {
      message("No outliers to impute")
    }
  }
  apply(dataframe_knn, MARGIN = 2, FUN = function(x) knn_fun(x))
}
  
