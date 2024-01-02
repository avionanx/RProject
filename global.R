library(dplyr)
library(readxl)
library(ggplot2)
library(tidyverse)



unemployee_data <- read.csv("C:/Users/kahra/Downloads/unemployee_rate.csv")

unemployee_data_oecd <- unemployee_data %>% filter(LOCATION == "OECD")
unemployee_data_oecd_filtered <- unemployee_data_oecd[, !(names(unemployee_data_oecd) %in% c("MEASURE", "SUBJECT", "FREQUENCY", "Flag.Codes"))]
head(unemployee_data_oecd_filtered)

# unemployee_data_youth <- read.csv("C:/Users/kahra/Downloads/youth_unemployee_rate.csv")
# unemployee_data_oecd_youth <- unemployee_data_youth %>% filter(LOCATION == "OECD")
# unemployee_data_oecd_youth_filtered <- unemployee_data_oecd_youth[, !(names(unemployee_data_oecd_youth) %in% c("MEASURE", "SUBJECT", "FREQUENCY", "Flag.Codes"))]
# head(unemployee_data_oecd_youth_filtered)
 

inflation_data  <- read.csv("C:/Users/kahra/Downloads/inflation_oecd.csv")
inflation_data_cleared <- inflation_data[, !(names(inflation_data) %in% c("INDICATOR","MEASURE", "SUBJECT", "FREQUENCY", "Flag.Codes"))]
inflation_data_filtered <- inflation_data_cleared %>%
  filter(between(TIME, 2000, 2019))
head(inflation_data_filtered)







suicide_data  <- read.csv("C:/Users/kahra/Downloads/global_suicide_rates.csv", skip = 1)


suicide_data_cleared <- suicide_data %>%
  mutate(across(starts_with("X"), ~gsub("\\[.*?\\]", "", .)))

suicide_data_filtered <- suicide_data_cleared %>%
  gather(key = "year_rate", value = "rate", starts_with("X")) %>% # Y??llar?? ve de??erleri tek bir s??tunda topla
  mutate(year = gsub("X\\.", "", year_rate)) %>% # "X." k??sm??n?? kald??rarak y??l?? al
  separate(year, into = c("year", "X"), sep = "\\.") %>% # Y??l s??tununu ay??r
  select(-X, -year_rate) %>% # Gereksiz s??tunlar?? kald??r
  arrange(year) # Y??llara g??re s??rala

suicide_data_filtered_avg <- suicide_data_filtered %>%
  group_by(year, Sex) %>%
  summarise(rate = mean(as.numeric(rate), na.rm = TRUE))

suicide_data_filtered_avg_unisex <- suicide_data_filtered_avg %>%
  filter(Sex == " Male")

head(suicide_data_filtered_avg_unisex)

View(unemployee_data_oecd_filtered)
View(inflation_data_filtered)
view(suicide_data_filtered_avg)

ggplot() +
  geom_line(data=suicide_data_filtered_avg_unisex,aes(x=as.numeric(year), y = (as.numeric(rate))), color="#aa0000", linewidth=1.2) +
  geom_line(data=inflation_data_filtered, aes(x=as.numeric(TIME),y=as.numeric(Value)), color="#0000aa", linewidth=1.2) +
  scale_y_continuous(
    name = "Suicide Rates",
    sec.axis = sec_axis(~.*2, name="Consumer Price Index")
  ) +
  theme(
    axis.title.y = element_text(color = "#aa0000", size=12),
    axis.title.y.right = element_text(color = "#0000aa", size=12)
  ) +
  labs(
    title="Price-Suicide Plot",
    caption = "In HLFS, the series is not comparable to previous years due to the adjustments in the definition, scope and design of the survey since 2021."
  ) +
  xlab("Year")

ggplot() +
  geom_line(data=suicide_data_filtered_avg_unisex,aes(x=as.numeric(year), y = (as.numeric(rate))), color="#aa0000", linewidth=1.2) +
  geom_line(data=unemployee_data_oecd_filtered, aes(x=as.numeric(TIME),y=as.numeric(Value)), color="#0000aa", linewidth=1.2) +
  scale_y_continuous(
    name = "Suicide Rates",
    sec.axis = sec_axis(~.*2, name="Consumer Price Index")
  ) +
  theme(
    axis.title.y = element_text(color = "#aa0000", size=12),
    axis.title.y.right = element_text(color = "#0000aa", size=12)
  ) +
  labs(
    title="Price-Suicide Plot",
    caption = "In HLFS, the series is not comparable to previous years due to the adjustments in the definition, scope and design of the survey since 2021."
  ) +
  xlab("Year")
