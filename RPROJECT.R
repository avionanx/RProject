library(dplyr)
library(readxl)
library(ggplot2)
main <- function(){
  rows_to_read <- c(53,92,131,170,209,248,287,326)
  rows_to_read_2 <- c(41,80,119,158,197,236,275,314)
  suicideData <- read_excel("Rproject/suicide.xls", skip = 4)
  unemploymentData <- read_excel("Rproject/unemployment.xls", skip = 6, col_names = c("Year","Count","Percentage"))
  priceIndex <- read_excel("Rproject/inflation.xls", skip = 4, col_names = c("Year","January","February","March","April","May","June","July","August","September","October","November","December"))
  
  unemploymentData <- unemploymentData |>
    slice(0:8)
  unemploymentData[8,1] = "2021"
  priceIndex <- priceIndex |>
    slice(12:19)|>
    mutate(across(2:last_col(), as.numeric))|>
    mutate(Total = rowSums(across(January:December)), .before = January)
  suicideData <- suicideData |>
    slice(rows_to_read_2)|>
    select(-c(1,2,3))|>
    mutate(Year = c(2021,2020,2019,2018,2017,2016,2015,2014), .before = Total)|>
    arrange(-dplyr::row_number())
  View(priceIndex)
  View(unemploymentData)
  
  
#   ggplot() +
#     geom_line(data=suicideData,aes(x=as.numeric(Year), y = (as.numeric(Total))), color="#aa0000", size=1.2) +
#     geom_line(data=priceIndex, aes(x=as.numeric(Year),y=as.numeric(Total)/2), color="#0000aa", size=1.2) +
#     scale_y_continuous(
#       name = "Suicide Rates",
#       sec.axis = sec_axis(~.*2, name="Consumer Price Index")
#     ) +
#     theme(
#       axis.title.y = element_text(color = "#aa0000", size=12),
#       axis.title.y.right = element_text(color = "#0000aa", size=12)
#     ) +
#     labs(
#       title="Price-Suicide Plot",
#       caption = "In HLFS, the series is not comparable to previous years due to the adjustments in the definition, scope and design of the survey since 2021.
# "
#     ) +
#     xlab("Year")
ggplot() +
  geom_line(data=suicideData,aes(x=as.numeric(Year), y = (as.numeric(Total))), color="#aa0000", size=1.2) +
  geom_line(data=unemploymentData, aes(x=as.numeric(Year),y=as.numeric(Count)), color="#0000aa", size=1.2) +
  scale_y_continuous(
    name = "Suicide Rates",
    sec.axis = sec_axis(~., name="Unemployment Count (Thousand)")
  ) +
  theme(
    axis.title.y = element_text(color = "#aa0000", size=12),
    axis.title.y.right = element_text(color = "#0000aa", size=10)
  ) +
  labs(
    title="Price-Unemployment Plot",
    caption = "*in 2021, suicide data were revised with updated administrative records."
  ) +
  xlab("Year")
  
  
  #ggplot() +
  #  geom_line(data=unemploymentData,aes(x=as.numeric(Year),y=Count))
  
  #ggplot() +
  #  geom_line(data=priceIndex, aes(x=Year,y=Total), group=1)
}

main()
