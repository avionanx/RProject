library(dplyr)
library(ggplot2)
library(readxl)

main <- function() {
  rows_to_read_male <- c(42,81,120,159,198,237,276,315)
  rows_to_read_female <- c(43,82,121,160,199,238,277,316) # c(43,82,121,160,199,238,277,316)
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
  
  
  suicideData_male <- suicideData |>
    slice(rows_to_read_male)|>
    select(-c(1,2))|>
    mutate(Year = c(2021,2020,2019,2018,2017,2016,2015,2014), .before = Sex)|>
    arrange(-dplyr::row_number())
  
  suicideData_female <- suicideData |>
    slice(rows_to_read_female)|>
    select(-c(1,2))|>
    mutate(Year = c(2021,2020,2019,2018,2017,2016,2015,2014), .before = Sex)|>
    arrange(-dplyr::row_number())
  
  
  View(priceIndex)
  View(unemploymentData)
  View(suicideData)
  
  
  plot_suicide_vs_price <- ggplot() +
    geom_bar(data = suicideData_male, aes(x = as.numeric(Year) - 0.2, y = as.numeric(Total), fill = "Male"), 
             color = "cyan", position = "dodge", stat = "identity", width = 0.4) +
    geom_bar(data = suicideData_female, aes(x = as.numeric(Year) + 0.2, y = as.numeric(Total), fill = "Female"), 
             color = "pink", position = "dodge", stat = "identity", width = 0.4) +
    geom_line(data = priceIndex, aes(x = as.numeric(Year), y = as.numeric(Total) / 2, group = 1), 
              color = "#0000aa", size = 1.2) +
    scale_y_continuous(
      name = "Suicide Rates",
      sec.axis = sec_axis(~ . * 2, name = "Inflation", breaks = seq(0, max(priceIndex$Total), by = 500))
    ) +
    scale_fill_manual(values = c("Male" = "cyan", "Female" = "pink")) +
    theme(
      axis.title.y = element_text(color = "black", size = 12),
      axis.title.y.right = element_text(color = "#0000aa", size = 12)
    ) +
    labs(
      title = "Suicide Rates Vs. Inflation",
      caption = "Inflation and the change in suicide rates over the years."
    ) +
    xlab("Year")
  
  plot_suicide_vs_price
  
  
  
  plot_suicide_vs_unemployment <- ggplot() +
    geom_bar(data = suicideData_male, aes(x = as.numeric(Year) - 0.2, y = as.numeric(Total), fill = "Male"), 
             color = "cyan", position = "dodge", stat = "identity", width = 0.4) +
    geom_bar(data = suicideData_female, aes(x = as.numeric(Year) + 0.2, y = as.numeric(Total), fill = "Female"), 
             color = "pink", position = "dodge", stat = "identity", width = 0.4) +
    geom_line(data = unemploymentData, aes(x = as.numeric(Year), y = Count), 
              color = "#0000aa", size = 1.2, group = 1) + 
    scale_y_continuous(
      name = "Suicide Rates",
      sec.axis = sec_axis(~ ., name = "Unemployment", 
                          breaks = seq(0, max(unemploymentData$Count), by = 500))
    ) +
    scale_fill_manual(values = c("Male" = "cyan", "Female" = "pink")) +
    theme(
      axis.title.y = element_text(color = "black", size = 12),
      axis.title.y.right = element_text(color = "#0000aa", size = 12)
    ) +
    labs(
      title = "Suicide Rates Vs. Unemployment",
      caption = "*in 2021, suicide data were revised with updated administrative records."
    ) +
    xlab("Year")
  
  plot_suicide_vs_unemployment
  
  
}  

main()
