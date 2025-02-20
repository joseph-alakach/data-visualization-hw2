library(ggplot2)
library(dplyr)
library(ggthemes)


cancer_data <- read.csv("lung_cancer_prediction_dataset.csv")
air_pollution_data <- read.csv("global_air_pollution_dataset.csv")


# --1
ggplot(data = cancer_data, aes(y=Annual_Lung_Cancer_Deaths)) + geom_boxplot()


# --2
ggplot() +
  geom_boxplot(data = air_pollution_data, aes(y=PM2.5_AQI_Value) )


# --3
ggplot() +
  geom_density(data = cancer_data, aes(x=Mortality_Rate))

# --4
df <- data.frame(x = rnorm(100), y = rlogis(100))
ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "brown") +
  theme_solarized(light = FALSE)