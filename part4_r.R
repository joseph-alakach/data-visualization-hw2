library(ggplot2)
library(dplyr)
library(ggthemes)


cancer_data <- read.csv("lung_cancer_prediction_dataset.csv")
air_pollution_data <- read.csv("global_air_pollution_dataset.csv")



# --2
cancer_data_grouped <- cancer_data %>%
  group_by(Country) %>%
    summarise(Annual_Lung_Cancer_Deaths = sum(Annual_Lung_Cancer_Deaths, na.rm = TRUE))
air_pollution_data_grouped <- air_pollution_data %>%
  group_by(Country) %>%
      summarise(PM2.5_AQI_Value = mean(PM2.5_AQI_Value, na.rm = TRUE))


merged_data <- inner_join(cancer_data_grouped, air_pollution_data_grouped, by = "Country")

ggplot(data = merged_data, aes(x=PM2.5_AQI_Value, y = Annual_Lung_Cancer_Deaths, color = Country, size=Annual_Lung_Cancer_Deaths)) +
  geom_point() +
    geom_text(aes(label = Country), color= "black") +
      labs(title = "PM2.5 AQI vs. Annual Lung Cancer Deaths", x = "PM2.5 AQI Value", y = "Annual Lung Cancer Deaths", color = "red") +
        theme_minimal()




# --3
cleaned_cancer_data <- cancer_data %>%
  filter(Years_of_Smoking > 0, Cancer_Stage != "None")
ggplot(data = cleaned_cancer_data, aes(x=Years_of_Smoking, y = Cancer_Stage, color= Gender, shape = Gender)) +
  geom_jitter(size = 2.7, alpha=0.75) +
    scale_color_manual(values=c("Male"="#5469f1", "Female"="#d554f1")) +
      scale_shape_manual(values = c("Male" = 19, "Female" = 17)) +
        labs(title = "Lung Cancer Stage vs. Smoking Years", subtitle = "Comparison by Gender", y="Cancer Stage", x="Years of Smoking") +
          facet_wrap(.~Gender) +
            theme_minimal() +
              theme(legend.position = "bottom")


# --4
air_pollution_data_filtered <- air_pollution_data %>%
  filter(Country %in% c("Brazil", "Germany", "India", "Italy", "Russian Federation", "United States of America"))
ggplot(data = air_pollution_data_filtered, aes(x=PM2.5_AQI_Value, fill=Country)) +
  geom_histogram( bins = 50, color = 'black') +
    scale_fill_manual(values=c("Brazil"="blue", "Germany"="purple", "India"="pink", "Italy"="brown", "Russian Federation"="orange", "United States of America"="yellow")) +
    labs(title = "PM2.5 AQI Distribution Across Countries", subtitle = "Comparison of Air Pollution Levels", y="Frequency", x="PM2.5 AQI Value") +
      facet_wrap(.~Country, scales = "free_y") +
        theme_minimal() +
          theme(legend.position = "bottom")


