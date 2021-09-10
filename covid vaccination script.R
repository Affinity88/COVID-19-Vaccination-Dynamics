library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#1 What vaccination schemes (combination of vaccines) are used and in which countries?
covid_vaccination <- read_csv('country_vaccinations.csv')
Combination_of_Vaccine_per_Country <- covid_vaccination %>%
  #remove all columns except country and vaccine, then use distinct to filter out the types
  select(-iso_code, -date, -total_vaccinations, -people_vaccinated, 
         -people_fully_vaccinated, -daily_vaccinations, - total_vaccinations_per_hundred, -people_vaccinated_per_hundred, 
         -people_fully_vaccinated_per_hundred, -daily_vaccinations, -source_name, -source_website, -daily_vaccinations_raw,
         -daily_vaccinations_per_million) %>%
  distinct()
Combination_of_Vaccine_per_Country
  #The answer is in the data, Afghanistan uses AstraZeneca, Albania uses Pfizer, etc.
  

#2 What is the vaccine used in the largest number of countries?
Number_of_Vaccines_used <- Combination_of_Vaccine_per_Country %>%
  arrange(vaccines) %>%
  count(vaccines)
Number_of_Vaccines_used

Number_Of_Countries_Using_Moderna <- Number_of_Vaccines_used %>%
  filter(grepl("Moderna", vaccines)) %>%
  summarize(Moderna_Countries=sum(n))
Number_Of_Countries_Using_Moderna
#if you want to isolate a single vaccine

Number_of_Vaccines_used <- Number_of_Vaccines_used %>%
  mutate(vaccines = strsplit(vaccines, ", ")) %>% 
  unnest(vaccines) %>% 
  group_by(vaccines) %>% 
  summarise(n = sum(n))
Number_of_Vaccines_used
  #Moderna = 35 Countries
  #Covaxin = 1 Country
  #EpiVacCorona = 1 Country
  #Johnson&Johnson = 1 Country
  #AstraZeneca = 105 Countries
  #pfizer = 82 countries
  #Sinopharm = 24 Countries
  #Sputnik V = 20 Countries
  #Sinovac = 18 Countries
  
  #AstraZeneca is the vaccine used in the majority of countries.

#3 What country has vaccinated more people?
People_Vaccinated_in_Country <- covid_vaccination %>%
  select(-iso_code, -total_vaccinations,  
         -people_fully_vaccinated, -daily_vaccinations, - total_vaccinations_per_hundred, -people_vaccinated_per_hundred, 
         -people_fully_vaccinated_per_hundred, -daily_vaccinations, -source_name, -source_website, -daily_vaccinations_raw,
         -daily_vaccinations_per_million, -vaccines) %>%
    group_by(country) %>%
    slice_max(as.Date(date, '%m/%d/%Y')) %>%
    arrange(desc(people_vaccinated, na.rm=TRUE))
People_Vaccinated_in_Country


  # "people vaccinated" is ordered from largest to smallest.
  # The country with the most vaccinated people is the United States on April 5th, 2021.

#4 What country has (fully) immunized the largest percent from its population?
Immunized_percentage <- covid_vaccination %>%
  select(-iso_code, -total_vaccinations,  -people_vaccinated, -people_vaccinated_per_hundred,
         -people_fully_vaccinated, -daily_vaccinations, - total_vaccinations_per_hundred, 
          -daily_vaccinations, -source_name, -source_website, -daily_vaccinations_raw,
         -daily_vaccinations_per_million, -vaccines) %>%
  na.omit() %>%
  group_by(country) %>%
  slice_max(as.Date(date, '%m/%d/%Y')) %>%
  arrange(desc(people_fully_vaccinated_per_hundred))
Immunized_percentage
  #column "people_fully_vaccinated_per_hundred" is ordered from largest to smallest.
  #Gibraltar fully immunized 84.93% of the country, making it the country that has fully immunized the largest percent from its population.

#5 What is the country that vaccinated completely most of the population?
Vaccination_percentage <- covid_vaccination %>%
  select(-iso_code, -total_vaccinations,  -people_vaccinated, 
         -people_fully_vaccinated, -daily_vaccinations, - total_vaccinations_per_hundred, 
         -people_fully_vaccinated_per_hundred, -daily_vaccinations, -source_name, -source_website, -daily_vaccinations_raw,
         -daily_vaccinations_per_million, -vaccines) %>%
  na.omit() %>%
  group_by(country) %>%
  slice_max(as.Date(date, '%m/%d/%Y')) %>%
  arrange(desc(people_vaccinated_per_hundred))
Vaccination_percentage
  #Column "people_vaccinated_per_hundred" is ordered from largest to smallest.
  #Gibraltar vaccinated most of the population compared to other countries, vaccinating a total of 95.85% of their population as of April 2nd 2021.

#6 Can you trace the daily vaccinations dynamic?
vaccination_daily <- covid_vaccination %>%
  select(-iso_code, -total_vaccinations,  -people_vaccinated, -people_vaccinated_per_hundred,
         -people_fully_vaccinated, - total_vaccinations_per_hundred, 
         -people_fully_vaccinated_per_hundred, -source_name, -source_website, -vaccines) 
vaccination_daily
  
  #completed, the code only contains data related to the daily vaccination in each country.

#to track every country's daily vaccination dynamic via graph
vaccination_daily2 <- vaccination_daily %>%
  select(-daily_vaccinations_raw, -daily_vaccinations_per_million) %>%
  na.omit()
vaccination_daily2

daily_vaccinations_graph <- vaccination_daily2 %>%
  ggplot(data=vaccination_daily2, mapping=aes(x=date, y=daily_vaccinations)) + geom_point(alpha=.5, aes(color='Country')) 
daily_vaccinations_graph + labs(title = 'Covid-19 Daily Vaccination', subtitle = 'December 13th 2020 - April 5th, 2021') +
  xlab('date') + ylab("Vaccination Daily")+ scale_y_continuous(name="Vaccination Daily", labels = comma) 


#to track the daily vaccination dynamic in Canada via graph 
vaccination_daily3 <- vaccination_daily2 %>%
  filter(country == 'Canada') 
vaccination_daily3

daily_vaccinations_graph_canada <- vaccination_daily3 %>%
  ggplot(data=vaccination_daily3, mapping=aes(x=date, y=daily_vaccinations)) + geom_point(alpha=.5, aes(color='Canada')) 
  daily_vaccinations_graph_canada + labs(title = 'Covid-19 Daily Vaccination in Canada', subtitle = 'December 13th 2020 - April 5th, 2021') +
  xlab('date') + ylab("Vaccination Daily")+ scale_y_continuous(name="Vaccination Daily", labels = comma) +
  geom_smooth(method='lm', formula= y~x, colour="black", size=0.5)



#finished "Track the Progress of Covid-19 Vaccination: Spring Edition"
  #https://www.kaggle.com/gpreda/covid-world-vaccination-progress/tasks?taskId=3897


  

