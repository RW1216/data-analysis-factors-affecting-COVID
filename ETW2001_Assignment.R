library(tidyverse)
library(ggplot2)
#install.packages("reshape2")
library(reshape2)

#####Section A#####
#Data cleaning
covid_data = read.csv("C:/Users/ryuya/Desktop/ETW2001 Assignment/owid-covid-data.csv")

#Description of raw data
summary(covid_data)
str(covid_data)
#Check the number of NA's
colSums(is.na(covid_data))  

#Removing variables with too many NA's or variables not used
###IMPORTANT: Variables such as people_vaccinated is sampled periodically(once a month for example) so
###it is bound to have many NA's in a daily recorded dataset. 
covid_data_filter = covid_data[,c("continent", "location", "date", "total_cases", "new_cases", 
                                  "total_deaths", "new_deaths", "population", "population_density", 
                                  "median_age", "aged_65_older", "aged_70_older", "gdp_per_capita", 
                                  "life_expectancy")]   
summary(covid_data_filter)

#Convert chr to date type
str(covid_data_filter)
covid_data_filter$date = as.Date(covid_data_filter$date)
str(covid_data_filter)

#Remove empty string
#Empty string in continent
unique(covid_data_filter$continent)
covid_data_filter = covid_data_filter[!(covid_data_filter$continent==""), ]

#Removing NA's
covid_clean = na.omit(covid_data_filter)
summary(covid_clean)

#Check and remove outliers for all numerical variables
#Check if there is any outliers
covid_clean$total_cases[covid_clean$total_cases %in% boxplot.stats(covid_clean$total_cases) $ out]   
covid_clean$new_cases[covid_clean$new_cases %in% boxplot.stats(covid_clean$new_cases) $ out]
covid_clean$total_deaths[covid_clean$total_deaths %in% boxplot.stats(covid_clean$total_deaths) $ out]
covid_clean$new_deaths[covid_clean$new_deaths %in% boxplot.stats(covid_clean$new_deaths) $ out]
covid_clean$population[covid_clean$population %in% boxplot.stats(covid_clean$population) $ out]
covid_clean$population_density[covid_clean$population_density %in% boxplot.stats(covid_clean$population_density) $ out]
covid_clean$median_age[covid_clean$median_age %in% boxplot.stats(covid_clean$median_age) $ out]
covid_clean$aged_65_older[covid_clean$aged_65_older %in% boxplot.stats(covid_clean$aged_65_older) $ out]
covid_clean$aged_70_older[covid_clean$aged_70_older %in% boxplot.stats(covid_clean$aged_70_older) $ out]
covid_clean$gdp_per_capita[covid_clean$gdp_per_capita %in% boxplot.stats(covid_clean$gdp_per_capita) $ out]
covid_clean$life_expectancy[covid_clean$life_expectancy %in% boxplot.stats(covid_clean$life_expectancy) $ out]

#botplot
boxplot(covid_clean[c("total_cases", "new_cases", "total_deaths", "new_deaths", "population", 
                      "population_density", "median_age", "aged_65_older", "aged_70_older", "gdp_per_capita", 
                      "life_expectancy")], 
        main = "Outliers for each numerical variables")

# ggplot(data = covid_clean[c("total_cases", "new_cases", 
#                             "total_deaths", "new_deaths", "population", "population_density", 
#                             "median_age", "aged_65_older", "aged_70_older", "gdp_per_capita", 
#                             "life_expectancy")], aes(factor(total_cases), life_expectancy),
#        aes(x = "")) +
#   geom_boxplot()
  
#Removing outliers, creating new dataset for each process
covid_clean2 = covid_clean[! covid_clean$total_cases %in% boxplot.stats(covid_clean$total_cases) $ out,]
covid_clean3 = covid_clean2[! covid_clean2$new_cases %in% boxplot.stats(covid_clean2$new_cases) $ out,]
covid_clean4 = covid_clean3[! covid_clean3$total_deaths %in% boxplot.stats(covid_clean3$total_deaths) $ out,]
covid_clean5 = covid_clean4[! covid_clean4$new_deaths %in% boxplot.stats(covid_clean4$new_deaths) $ out,]
covid_clean6 = covid_clean5[! covid_clean5$population %in% boxplot.stats(covid_clean5$population) $ out,]
covid_clean7 = covid_clean6[! covid_clean6$population_density %in% boxplot.stats(covid_clean6$population_density) $ out,]
covid_clean_final = covid_clean7[! covid_clean7$gdp_per_capita %in% boxplot.stats(covid_clean7$gdp_per_capita) $ out,]

nrow(covid_clean_final)
summary(covid_clean_final)

#####Section B#####
covid_medical = read.csv("C:/Users/ryuya/Desktop/ETW2001 Assignment/DATA.csv")

#Description of raw data
summary(covid_medical)
tail(covid_medical)

#NOTE: We only want to use average temp, hospital beds, and medical doctors to merge with out first data. 
#NOTE: So we filter them and remove duplicate rows. 
#Filter variable not used
covid_medical_filter = covid_medical[,c("Entity", "Continent", "Average.temperature.per.year",
                                        "Hospital.beds.per.1000.people", "Medical.doctors.per.1000.people")]

#Remove duplicate rows
covid_medical_final <- covid_medical_filter[!duplicated(covid_medical_filter),]

#Rename Entity into location so it can be joined with the first data
colnames(covid_medical_final)[colnames(covid_medical_final) == "Entity"] = "location"

#Description of processed data
summary(covid_medical_final)
nrow(covid_medical_final)

#Data Joining
#Inner join
covid_inner_join = covid_clean_final %>%
  inner_join(covid_medical_final)
summary(covid_inner_join)

#Anti join
covid_anti_join = anti_join(covid_clean_final, covid_medical_final)
summary(covid_anti_join)  

#Full join
covid_full_join = full_join(covid_clean_final, covid_medical_final)
summary(covid_full_join)  

head(covid_inner_join)


#####Section C#####

#Plot1
#correlation map
plot_data1 = select_if(covid_clean_final, is.numeric)
head(plot_data1)

#Creating correlation matrix
corr_mat<-round(cor(plot_data1),2)
#Reduce the size of correlation matrix
meltedcorr<-melt(corr_mat)

ggplot(data=meltedcorr,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+
  geom_text(aes(Var2,Var1,label=value), color="white",size=3) +
  labs(title = "Correlation matrix for different covid variables") +
  theme(plot.title = element_text(size = 18, hjust = 0.5))

#plot2
plot_data2 <- covid_inner_join %>%
  group_by(location) %>%
  summarize(total_deaths = max(total_deaths),
            Hospital.beds.per.1000.people = mean(Hospital.beds.per.1000.people),
            Medical.doctors.per.1000.people = mean(Medical.doctors.per.1000.people))

ggplot(data = plot_data2) +
  geom_point(mapping = aes(x = Hospital.beds.per.1000.people, y = total_deaths)) +
  geom_smooth(mapping = aes(x = Hospital.beds.per.1000.people, y = total_deaths)) +
  labs(title = "Relationship between Hospital beds and total deaths", 
       x = "hospital beds per 1000 people", y = "total deaths") +
  theme(plot.title = element_text(size = 18, hjust = 0.5))

#plot3
plot_data3 <- covid_clean_final %>%
  group_by(location) %>%
  summarize(population_density = mean(population_density),
            total_cases = max(total_cases))

ggplot(data = plot_data3) +
  geom_point(mapping = aes(x = population_density, y = total_cases)) +
  geom_smooth(mapping = aes(x = population_density, y = total_cases)) +
  labs(title = "Scatter plot of Total Cases against Population Density for every country", 
       x = "population density", y = "total cases") +
  theme(plot.title = element_text(size = 15, hjust = 0.5))

#Plot4
#Using covid data with outliers
plot_data4 <- covid_clean %>%
  group_by(continent, date) %>%
  summarize(total_cases = sum(total_cases))

ggplot(data = plot_data4) +
  geom_smooth(aes(x = date, y = total_cases, colour = continent)) +
  labs(title = "Line chart of Total Cases for each Continent", x = "date", y = "total cases") +
  theme(plot.title = element_text(size = 16, hjust = 0.5))

#Plot5
plot_data5 <- covid_clean %>%
  group_by(continent) %>%
  summarize(population = sum(population))

ggplot(data = plot_data5, mapping = aes(x = reorder(continent, +population), y = population/1000000)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Population of each Continent in millions", x = "continent", y = "population") +
  geom_text(aes(label= as.integer(population/1000000)), vjust=-0.7, size=3.5, color = "Black") +
  theme(plot.title = element_text(size = 18, hjust = 0.5))






