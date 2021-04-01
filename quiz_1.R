# This code is for online quiz 1
library(tidyverse)

#Q1 - How many individual data scientists are included in this data set?
quiz_data=read_csv(file="/Users/sahillsharma/Desktop/BUSA8000/Week 5/quiz_1/data_scientist_hr.csv")

str(quiz_data)

unique(quiz_data$enrollee_id)
unique(quiz_data$gender)
unique(quiz_data$education_level)
unique(quiz_data$experience)

# Question 2 - How many data scientists are listed as Female in this data set?
quiz_female = quiz_data %>%
  filter(gender=="Female")

# Question 3 - For Female data scientists, how many had high school listed as their highest level of education?
female_HS = quiz_female %>%
  filter(education_level=="High School")

# Question 4 - How many data scientists are listed as having greater than 15 years experience in this data set?

quiz_ge15 = quiz_data %>%
  mutate(experience = replace(experience,experience == ">20","20")) %>%
  mutate(experience = replace(experience,experience == "<1","1"))

unique(quiz_ge15$experience)
quiz_ge15$experience <- as.numeric(quiz_ge15$experience)
str(quiz_ge15)

greater_than_15 = quiz_ge15 %>%
  filter(experience > 15)

unique(greater_than_15$experience)

# Question 5 - For data scientists with full-time enrolment in university, what proportion have relevant experience?
unique(quiz_data$enrolled_university)
unique(quiz_data$relevent_experience)

# get all the full-time enrolled Data Scientists first = 3757
ds_ft = quiz_data %>%
  filter(enrolled_university=="Full time course")

ds_pt = quiz_data %>% 
  filter(enrolled_university=="Part time course")

ds_no = quiz_data %>%
  filter(enrolled_university=="no_enrollment")

# now get all the 'relevent experience' from the above dataset = 1444
ds_rel = ds_ft %>%
  filter(relevent_experience=="Has relevent experience")

exp_ft = quiz_data %>% # proportion = 38.43%
  filter(enrolled_university=="Full time course" & relevent_experience=="Has relevent experience")

exp_pt = quiz_data %>% # proportion = 68.20%
  filter(enrolled_university=="Part time course" & relevent_experience=="Has relevent experience")

exp_no = quiz_data %>% # proportion = 82.04%
  filter(enrolled_university=="no_enrollment" & relevent_experience=="Has relevent experience")

# For Q7, 8, 9, separate the data set into 2 part: < 50 hrs training & >= 50 hr training)
str(quiz_data)
# For data scientists with less than 50 hours of training, how many do not have their years of experience listed?
data_lt50 = quiz_data %>% #TOTAL = 9914
  filter(training_hours < 50)

unique(data_lt50$experience)

sum(is.na(data_lt50$experience)) # TOTAL = 34

# What proportion of data scientists have at least 50 hours of training completed?
data_ge50 = quiz_data %>%
  filter(training_hours >= 50)

unique(data_ge50$training_hours)

unique(data_lt50$experience)
str(data_lt50)

data_lt50_NEW = quiz_data %>%
  filter(training_hours < 50)
unique(data_lt50_NEW$experience)

data_lt50_NEW = data_lt50_NEW %>%
  mutate(experience = replace(experience,experience == "<1","1")) %>%
  mutate(experience = replace(experience,experience == ">20","20"))

data_lt50_NEW$experience <- as.numeric(data_lt50_NEW$experience)

lt50_lt10 = data_lt50_NEW %>%
  filter(experience <= 10)

# QUESTION 10 - Estimate missing values in "education level" column using the knn algorithm
sum(is.na(quiz_data$education_level))
unique(quiz_data$experience)
unique(quiz_data$city_development_index)
unique(quiz_data$training_hours)

quiz_data = quiz_data %>%
  filter(!is.na(experience))

quiz_data_replace = quiz_data %>%
  mutate(experience = replace(experience,experience == "<1","0")) %>%
  mutate(experience = replace(experience,experience == ">20","20"))

quiz_data_replace$experience <- as.numeric(quiz_data_replace$experience)
str(quiz_data_replace)

library(class)
help(knn)

knn_test_data = quiz_data_replace %>%
  filter(is.na(education_level)) %>% #retrieve the unclassified data
  select(city_development_index,experience,training_hours) #only use necessary columns

knn_labels = na.omit(quiz_data_replace) %>%
  pull(education_level) #get just the entries without the tibble format

knn_train_data = na.omit(quiz_data_replace) %>% #removes any rows containing NA, use wisely
  select(city_development_index,experience,training_hours)

head(knn_train_data)

knn_output = knn(knn_train_data,knn_test_data,knn_labels,k=5) #run knn with k=5

head(knn_output)
head(knn_labels)

ggplot() +
  geom_point(aes(x=knn_test_data$city_development_index,y=knn_test_data$experience,color=knn_output)) + #plot knn estimates
  geom_point(aes(x=knn_train_data$city_development_index,y=knn_train_data$experience,color=knn_labels),alpha=0.1) + #plot original data in "background"
  xlab("City Dev Index") +
  ylab("Experience") +
  ggtitle("Plot of knn data") +
  labs(color="education_level")

id_24836 = quiz_data %>%
  filter(enrollee_id==24836)

# Question 11 and 12 - Performing preliminary investigation into factors influencing a data scientist's desire to change jobs

# filter for data scientists with STEM as major discipline = 14492 OBSERVATIONS
STEM_data = quiz_data %>%
  filter(major_discipline=="STEM")

STEM_newjob = STEM_data %>% # 3791 ()
  filter(target==1)

proportion_STEM <- proportion # proportion = 26.16%
remove(proportion)


unique(quiz_data$major_discipline)

# filter for data scientists with BUSINESS as major discipline = 327 OBSERVATIONS
BUS_data = quiz_data %>%
  filter(major_discipline=="Business Degree")

BUS_newjob = BUS_data %>%
  filter(target==1)

prop <- 86/327 # proportion = 26.30%
