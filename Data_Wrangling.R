library(tidyverse)

options(scipen = 999)

data <- read.csv("Clinic_Patients_messy.csv")
data
# splitting Systolic_BP and Diastolic BP
data1 <- separate(data, col= "Systolic_BP.Diastolic_BP", into=c("Systolic_BP","Diastolic_BP"),sep="/")

# changing column type to Integer
data1$Systolic_BP <- as.numeric(data1$Systolic_BP)
data1$Diastolic_BP <- as.numeric(data1$Diastolic_BP)

# Replacing entries like "25 years" to 25 and changing column type to Integer
data1$Age <- as.numeric(gsub("[^0-9]+", "", data1$Age))

data1$Age <- as.numeric(data1$Age)


glimpse(data1)

data1 %>% summarize_all(funs(sum(is.na(.))))
mean_weight <- mean(data1$Weight, na.rm = TRUE)

# Replace NA values with the mean in the "Weight" column
data1$Weight[is.na(data1$Weight)] <- mean_weight

#########################################################################################################

data <- read.csv("survey.csv")

# Finding missing values in weight and filling them with mean of weight column

# Task 1: Spltting timestamp into time and date
data <- separate(data, col= "Timestamp", into=c("Date","Time"),sep=" ")

data

#  Task 2:  Remove negative age from the data
data %>% distinct(data$Age)

data_age <- data %>% filter(Age >= 0 & Age <= 110)
data_age %>% distinct(data_age$Age)

#  Task 3:  Make smaller categories in Gender

data_age %>% distinct(data_age$Gender)
data_age_gender <- data_age %>% mutate(Gender = case_when(
  tolower(Gender) %in% c("male", "cismale", "m", "malie", "mail","make","male-ish",
                         "maile","cis male","mal","male (cis)","guy (-ish) ^_^","man",
                         "msle","malr","cis man","male ","male leaning androgynous",
                         "something kinda male?",
                         " ostensibly male, unsure what that really means",
                         "ostensibly male, unsure what that really means ",
                         "ostensibly male,  unsure what that really means",
                         " ostensibly male,  unsure what that really means"
                         ) ~ "Male",
  TRUE ~ Gender
))
data_age_gender %>% distinct(data_age_gender$Gender)


data_age_gender2 <- data_age_gender %>% mutate(Gender = case_when(
  tolower(Gender) %in% c("female", "cis female", "f", "woman", "female ","femake","cis-female/femme",
                         "female (cis)","femail") ~ "Female",
  TRUE ~ Gender
))
data_age_gender2 %>% distinct(data_age_gender2$Gender)


data_age_gender3 <- data_age_gender2 %>% mutate(Gender = case_when(
  tolower(Gender) %in% c("female (trans)", "trans-female", "trans woman") ~ "Trans Female",
  TRUE ~ Gender
))
data_age_gender3 %>% distinct(data_age_gender3$Gender)


data_age_gender4 <- data_age_gender3 %>% mutate(Gender = case_when(
  tolower(Gender) %in% c("non-binary", "enby", "androgyne", "agender",
                         "queer/she/they", "nah","Enby","genderqueer",
                         "fluid","queer") ~ "Non-Binary/Genderqueer",
  TRUE ~ Gender
))

data_age_gender4 %>% distinct(data_age_gender4$Gender)


data_age_gender5 <- data_age_gender4 %>% mutate(Gender = case_when(
  tolower(Gender) %in% c("agender", "neuter") ~ "Agender/Neuter",
  TRUE ~ Gender
))

data_age_gender5 %>% distinct(data_age_gender5$Gender)

data_age_gender6 <- data_age_gender5 %>% mutate(Gender = case_when(
  tolower(Gender) %in% c("a little about you") ~ "Unspecified",
  TRUE ~ Gender
))

data_age_gender6 %>% distinct(data_age_gender6$Gender)




data_age_gender7 <- data_age_gender6 %>% mutate(Gender = ifelse(grepl("unsure",
                                                        tolower(Gender)), "Male", Gender))

data_age_gender7 %>% distinct(data_age_gender7$Gender)

data2 <- data_age_gender7


data2 %>% distinct(data2$work_interfere)

# Task 4: Fill NA values in work_interfere columns 

data2 %>% summarize_all(funs(sum(is.na(.))))

         
data2 %>% summarise_all(list(~n_distinct(.)))
unique_values_workInterfere <- unique(data2$work_interfere)

summary_df <- as.data.frame(table(data2$work_interfere))
ggplot(data2, aes(x = work_interfere)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Bar Chart of Unique Values vs. Counts",
       x = "Work Interference",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


mode_value <- as.character(as.vector(names(sort(table(data2$work_interfere), decreasing = TRUE)[1])))
mode_value

data2 <- data2 %>%
  mutate(work_interfere = ifelse(is.na(work_interfere), mode_value, work_interfere))

data2 %>% distinct(data2$work_interfere)
