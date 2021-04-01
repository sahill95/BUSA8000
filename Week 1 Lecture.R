install.packages(c("tidyverse", "zoo"))
library(tidyverse)
library(zoo)

install.packages("ellipsis")

sample_marks = read_csv(file="/Users/sahillsharma/Desktop/BUSA8000/Week 2/sample_data.csv")

str(sample_marks)

sample_marks[3,4] #first number = row, 2nd number = column
sample_marks$OL_quiz_1[3] #$ takes sample marks object, pulls column OL_quiz_1. Still requires specific row number to be known

sample_marks %>% # this is a pipe, allows a set of logical tests to be done
  filter(First_Name == "Romeo") %>% 
  select (OL_quiz_1) #logical test means double equals sign #filters out rows
#the above returns marks for ALL students named Romeo, as a tibble

#pipe takes starting object as a tibble, and therefore will always return a tibble

filter(sample_marks,First_Name == "Romeo")$OL_quiz_1
#select chooses columns

sample_marks %>%
  select(Surname,First_Name,Final_exam)

# detecting missing values
anyNA(sample_marks) #check whole dataset, if anything null, it will be TRUE
is.na(sample_marks) #tests each entry of the dataset to see if it is NULL
#False = 0, true = 1

sum(is.na(sample_marks))
colSums(is.na(sample_marks)) #takes the sum of NA in each column
which(is.na(sample_marks$Report)) #finds which specific entry number is null (in this case, 21st entry)
#1st function in report col is empty

# filter for students with 65+ in final exam

sample_marks %>%
  filter(Final_exam >= 65) %>%
  select(First_Name,Surname)

#student ID's starting with 1
sample_marks$Student_ID
substr(sample_marks$Student_ID,1,1)
filter(sample_marks,substr(sample_marks$Student_ID,1,1)==1) #filter for student id starting with 1

sample_marks %>%
  filter(substr(sample_marks$Student_ID,1,1)==1)


grep("1",sample_marks$Student_ID) #looks for things containing 1
grep("^1",sample_marks$Student_ID) #looks for things STARTING with 1
sample_marks[grep("^1",sample_marks$Student_ID),]

#BASIC CALCULATIONS
mean(sample_marks$OL_quiz_1)
median(sample_marks$OL_quiz_1)
mean(filter(sample_marks,Final_exam>=65)$OL_quiz_1)

mean(filter(sample_marks,Final_exam>=65)$OL_quiz_1)

mean(sample_marks$Report,na.rm=TRUE) #na.rm removes all NA values before performing the calculation

sample_marks$Student_ID #view student id's
sample_marks$Student_ID = as.character(sample_marks$Student_ID) #converted to characters as it doesn't make sense to leave them as numbers
sample_marks$Student_ID #view again to confirm

#add new rows
sample_marks_2 = sample_marks %>% #new dataset with new row
  add_row(Student_ID="5574", Surname="Snow", First_Name="Jon", OL_quiz_1=83, Report=68, OL_quiz_2=80, Final_exam=78)

#adding value for mR BEAN
select(filter(sample_marks_2,First_Name=="Mister"),Report) #view Bean's report mark

sample_marks_2 %>%
  mutate(Report = replace(Report,First_Name == "Mister", 62))


sample_marks_3 = sample_marks_2 %>%
  mutate(Report = replace(Report,First_Name == "Mister", 62))

#give everyone 3 more marks in final exam
sample_marks_3 %>%
  mutate(Final_exam = replace(Final_exam,TRUE,Final_exam+3))

sample_marks_4 = sample_marks_3 %>%
  mutate(Final_exam = replace(Final_exam,TRUE,Final_exam+3))

#add final grade column
sample_marks_4 %>%
  mutate(Final_Grade = 0.1*OL_quiz_1+0.1*OL_quiz_2+0.2*Report+0.6*Final_exam)

sample_marks_5 = sample_marks_4 %>%
  mutate(Final_Grade = 0.1*OL_quiz_1+0.1*OL_quiz_2+0.2*Report+0.6*Final_exam)
