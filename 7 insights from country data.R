library(dplyr)
library(ggplot2)
cnt_data<-readr::read_csv(".\\umldata\\Country-data.csv")

View(cnt_data)

#average GDP per capita across country
average_gdp <- cnt_data %>% 
  group_by (country) %>%
summarise(avg_gdp= mean(gdpp, na.rm = TRUE ))
print(average_gdp)

#which country has the highest life expectancy?
highest_life_expentancy <- cnt_data %>%
  filter(life_expec == max(life_expec,na.rm = TRUE))
print(highest_life_expentancy)

#what is the correlation between GDP per capita and life expectancy?
correlation <- cor(cnt_data$gdpp, cnt_data$life_expec,use = "pairwise.complete.obs")
print(correlation)

#Which country has the lowest imports
lowest_import <- cnt_data %>%
  filter(imports == min(imports, na.rm = TRUE))
print(lowest_import)

#relationship between income and child mortality

correlation <- cor(cnt_data$income, cnt_data$child_mort,
                   use = "pairwise.complete.obs")
print(correlation)
ggplot(cnt_data, aes(x = income, y = child_mort))+
  geom_point()+
  geom_smooth(method = "lm")

#countries with the highest and lowest total fertility rates
highest_total_fertility <- cnt_data %>%
  filter(total_fer == max(total_fer, na.rm = TRUE))
lowest_total_fertility <- cnt_data %>%
  filter(total_fer == min(total_fer, na.rm = TRUE))
print(highest_total_fertility)
print(lowest_total_fertility)

#relationship between total fertility rate and child mortality
correlation <- cor(cnt_data$total_fer, cnt_data$child_mort, use = "pairwise.complete.obs")
print(correlation)
ggplot(cnt_data,aes(x = total_fer, y = child_mort))+
  geom_point()+
  geom_smooth(method = "lm")


 







