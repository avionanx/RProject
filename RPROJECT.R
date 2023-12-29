library(dplyr)
library(readxl)
library(ggplot2)
library(here)
main <- function(){
  rows_to_read <- c(53,92,131,170,209,248,287,326)
  suicideData <- read_excel("Rproject/suicide.xls", skip = 4)
  View(suicideData)
  unemploymentData <- read_excel("Rproject/unemployment.xls", skip = 6, col_names = c("Year","Count","Percentage"))
  inflationData <- read_excel("Rproject/inflation.xls", skip = 4, col_names = c("Year","January","February","March","April","May","June","July","August","September","October","November","December"))
  
  unemploymentData <- unemploymentData |>
    slice(0:8)
  unemploymentData[8,1] = "2021"
  inflationData <- inflationData |>
    slice(12:20)
  suicideData <- suicideData |>
    slice(rows_to_read)|>
    select(-c(1,2,3))|>
    mutate(Year = c(2021,2020,2019,2018,2017,2016,2015,2014), .before = Total)|>
    arrange(-dplyr::row_number())

  View(inflationData)

  ggplot() +
    geom_col(data=suicideData,aes(x=as.numeric(Year), y = (as.numeric(Total))))

  ggplot() +
    geom_line(data=unemploymentData,aes(x=as.numeric(Year),y=as.numeric(Count)))
  
  
}

main()