% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute_missing_or_sequential.R
\name{impute_missing_or_sequential}
\alias{impute_missing_or_sequential}
\title{Imputes missing data or sequential outliers}
\usage{
impute_missing_or_sequential(dataframe, column, column2)
}
\arguments{
\item{dataframe}{Object of data table or dataframe}

\item{column}{Dependent variable column in question with double quotes}

\item{column2}{Independent variable column in question with double quotes}
}
\description{
This function imputes missing values using MICE
}
\examples{
Example <- impute_missing_or_sequential(dataframe,"AllVisits","impacts")

All imputed series as well as the optimal are saved in your working directory

}
