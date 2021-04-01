library(tidyverse)
test_results=read_csv(file="/Users/sahillsharma/Desktop/BUSA8000/Week 3/2018_19_GT_Results.csv")
str(test_results)
head(test_results)

# Time stamp column
unique(test_results$Timestamp)

head(test_results)

test_results = test_results %>%
  mutate(Timestamp = replace(Timestamp,Timestamp == "3/27/2017","3/27/2018")) %>%
  mutate(Timestamp = replace(Timestamp,Timestamp == "Mar-27","3/27/2018")) %>%
  mutate(Timestamp = replace(Timestamp,Timestamp == "3/27/18","3/27/2018"))

# Grade Entering Level Column
unique(test_results$`Entering Grade Level`)

# change column name to 'grade'
test_results = test_results %>%
  rename(Grade = `Entering Grade Level`)

unique(test_results$Grade)

test_results = test_results %>%
  mutate(Grade = replace(Grade,Grade=="K","0")) %>%
  mutate(Grade = as.integer(Grade))

# District column
unique(test_results$District)

sum(is.na(test_results$District))
which(is.na(test_results$District))

test_results$District = as.character(test_results$District)
unique(test_results$District)

# Birth month column

test_results = test_results %>%
  rename(Birth_month = 'Birth Month')

unique(test_results$Birth_month)

test_results = test_results %>%
  mutate(Birth_month = replace(Birth_month, Birth_month == "Febrauary","February")) %>%
  mutate (Birth_month = str_to_title(Birth_month))

# OLSAT Verbal Score column

test_results = test_results %>%
  rename(OLSAT_VS = `OLSAT Verbal Score`)

unique(test_results$OLSAT_VS)

sum(is.na(test_results$OLSAT_VS))
which(is.na(test_results$OLSAT_VS))

test_results = test_results %>%
  mutate(OLSAT_VS = replace(OLSAT_VS, is.na(OLSAT_VS),mean(OLSAT_VS, na.rm=TRUE)))

# OLSAT Verbal Percentile Column
test_results = test_results %>%
  rename(OLSAT_VP = `OLSAT Verbal Percentile`)

unique(test_results$OLSAT_VP)

sum(is.na(test_results$OLSAT_VP))
which(is.na(test_results$OLSAT_VP))

student_6_district = test_results$District[6]

student_6_OVP = test_results %>%
  filter(District == student_6_district) %>%
  pull(OLSAT_VP)

district_median = median(student_6_OVP, na.rm=TRUE)

test_results = test_results %>%
  mutate(OLSAT_VP = replace(OLSAT_VP, is.na(OLSAT_VP),district_median))

test_results %>%
  slice(6)

#X10

test_results = test_results %>%
  rename(School_Preference = X10)

str(test_results)

unique(test_results$School_Preference)

test_results = test_results %>%
  mutate(School_Preference = str_to_title(School_Preference)) %>%
  mutate(School_Preference = replace(School_Preference,School_Preference == "Anderson?","Anderson")) %>%
  mutate(School_Preference = replace(School_Preference,substr(School_Preference,1,2)=="No",NA)) %>%
  mutate(School_Preference = replace(School_Preference,substr(School_Preference,1,1)==":",NA)) %>%
  mutate(School_Preference = replace(School_Preference,substr(School_Preference,1,3)=="N/A",NA))
  

# X11 and School Assigned columns
# best to delete these columns as they have no unique values
unique(test_results$X11)
unique(test_results$`School Assigned`)

test_results = test_results %>%
  select(-X11,-`School Assigned`)

str(test_results)

# Will you enrol there column - TRY THIS ONE MYSELF - similar edits to the School Preferences column)
unique(test_results$`Will you enroll there?`)

test_results = test_results %>%
  rename(Enroll = `Will you enroll there?`)

unique(test_results$Enroll)








# Test prep column
test_results = test_results %>%
  rename(Prep = `Test Prep?`)

unique(test_results$Prep)

test_results = test_results %>%
  mutate(Prep = str_to_title(Prep)) %>%
  mutate(Prep = replace(Prep,substr(Prep,1,1) == "N","No")) %>%
  mutate(Prep = replace(Prep,substr(Prep,1,1) == "Y","Yes")) %>%
  mutate(Prep = replace(Prep,substr(Prep,1,1) == "A","Yes")) %>%
  mutate(Prep = replace(Prep,substr(Prep,1,1) == "P","Yes")) %>%
  mutate(Prep = replace(Prep,substr(Prep,1,1) == "M",NA))
  
#X15 Column

unique(test_results$X15)

test_results %>%
  select(Prep,X15) %>%
  filter(!is.na(X15))

test_results = test_results %>%
  mutate(Prep = replace(Prep,!is.na(X15),"Yes")) %>%
  select(-X15)

str(test_results)

#write as a CSV file
write_csv(test_results,"/Users/sahillsharma/Desktop/BUSA8000/Week 3/2018_19_GT_Results_cleaned.csv")

# PART 2 - CREATING GRAPHS WITH THE CLIMATE DATA
temperature_table=read_csv(file="/Users/sahillsharma/Desktop/BUSA8000/Week 3/Climate_data.csv")
  
str(temperature_table)
head(temperature_table)

library(zoo)

temperature_table = temperature_table %>%
  select(-`Product code`,-`Bureau of Meteorology station number`,-Quality)

temperature_table = temperature_table %>%
  mutate(Date = as.yearmon(paste(temperature_table$Year,temperature_table$Month,sep="-")))

head(temperature_table)

temperature_table = temperature_table %>%
  rename(Temp = `Mean maximum temperature (°C)`)
 

# scatterplot of temperature against year
ggplot(temperature_table) + 
  theme_update(plot.title = element_text(hjust = 0.5)) +
  geom_point(aes(x=Year,y=Temp)) +
  xlab("Year") + 
  ylab("Temperature") + 
  ggtitle("Average max temperatures by year")

# histogram of temperatures

ggplot(temperature_table) + 
  geom_histogram(aes(x=Temp,fill=..count..),stat="bin",binwidth=0.5) + 
  xlab("Temperature") + 
  ggtitle("Histogram of temperatures") + 
  theme_light() + 
  scale_fill_continuous()

# Boxplots by month - CHANGE TITLE, X AND YLAB MYSELF
ggplot(temperature_table) + 
  geom_boxplot(aes(x=Month,y=Temp))


# line graph - CHANGE TITLE, X AND YLAB MYSELF
annual_mean_temp = group_by(temperature_table,Year) %>%
  summarise(mean_temp = mean(Temp))

ggplot(annual_mean_temp) + 
  geom_line(aes(x=Year,y=mean_temp),color="blue")







