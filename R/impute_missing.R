#' Imputes missing values 
#'
#' This function imputes missing values using MICE
#' @param dataframe Object of data table or dataframe
#' @param column Dependent variable column in question with double quotes
#' @param column2 Independent variable column in question with double quotes
#' @export impute_missing
#' @examples
#' Example <- impute_missing(dataframe,"AllVisits","impacts")
#' 
#' All imputed series as well as the optimal are saved in your working directory
#' 
impute_missing <- function(dataframe,column,column2){
  
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
  
  d<- dataframe[dataframe[[column]] %in% Outliers$Values,]
  
  Outliers<- mutate(d,Seq = c(0, diff(d$ID) > 1))
  
  dm<- Outliers %>% 
    filter(Seq==0)
  
  dataframe_mice<- dataframe %>%
    mutate(dataframe[[column]], Imputed = as.numeric(ifelse(dataframe[[column]] %in% dm[[column]], 0, dataframe[[column]]))) %>% 
    mutate(dataframe[[column2]], dataframe[[column2]])
  
  dataframe_mice<- dataframe_mice %>%
    group_by(Imputed, grp = with(rle(Imputed), rep(seq_along(lengths), lengths))) %>%
    mutate(Counter = seq_along(grp)) %>%
    ungroup()
  
  dataframe_mice<- dataframe_mice %>% 
    mutate(grp, Duplicate = as.character(ifelse(duplicated(grp) | duplicated(grp, fromLast=TRUE),"D","ND")))
  
  
  mice_fun <- function(mice_imp) { 
    
    mice_imp<- if(any(dataframe_mice$Duplicate=="D")){
      
      dataframe1<- dataframe_mice %>% 
        mutate(Date = as.Date(Date, format = '%d/%m/%Y')) %>% 
        mutate(weekday = weekdays(Date)) %>% 
        reshape2::dcast(Date + dataframe[[column]] + dataframe[[column2]] + grp + Imputed ~ weekday, fun.aggregate = length, value.var = "weekday")
      
      dataframe1$Imputed[duplicated(dataframe1$grp) | duplicated(dataframe1$grp, fromLast = TRUE)] <- NA
      
      dataframe1<- tbl_df(dataframe1)
      
      dataframe1 <- dataframe1 %>%  
        dplyr::select(Date,Imputed,`dataframe[[column2]]`)
      
      mice_imp <- mice(dataframe1,m=5, maxit = 50, method = 'pmm', printFlag = FALSE)
      
      fitm <- with(mice_imp, summary(lm(Imputed ~ dataframe[[column2]] ))$r.squared)
      sort<- sort(unlist(fitm[4]$analyses), index.return = TRUE, decreasing =TRUE)
      chosen<- sort[[2]][[1]]
      
      comb<- mice::complete(mice_imp, "broad", include = TRUE)
      
      names(comb)<- gsub(x = names(comb), pattern = "Imputed", replacement = column)
      
      comb1<- comb %>%
        select(Date.0,starts_with(glue::glue({column})),starts_with(glue::glue({column2})))
      
      names(comb1)[1]<- "Date"
      
      write.csv(comb1,"All_Imputed_Data_MICE.csv")
      
      imp_prime<- mice::complete(mice_imp,chosen)
      
      write.csv(imp_prime,"Optimal_Imputed_Data_MICE_method.csv")
      
    } else {
      message("No missing data to impute")
    }
  };
  
  #apply function
  apply(dataframe_mice, MARGIN = 2, FUN = function(x) mice_fun(x))
  }


