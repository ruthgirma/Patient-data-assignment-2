library(stringr)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(skimr)
library(kableExtra)
library(lubridate)

# Setting up the working directory
rm(list = ls())

# Importing all the files from excel to R
df_visit <- read_excel('~/Downloads/Visit (1).xlsx')
df_patient <- read_excel('~/Downloads/Patient (1).xlsx')
df_billing <- read_excel('~/Downloads/Billing (1).xlsx')

# Removed the first raw of the patient data because it was an example of the format of the data and irrelevant
df_patient_new <- df_patient[-c(1), ]

# Left joining the patient data to the visit data
df_visiting_patients <- left_join(df_visit, df_patient_new, by = c('PatientID'))

# Left joining the billing data to the new table we created by joining the patient and visit data
df_patient_information <- left_join(df_billing, df_visiting_patients, by = c('VisitID'))

skimr::skim(df_patient_information)


df_patient_information<-df_patient_information%>%
  dplyr::mutate(Reason = ifelse((Reason == 'UTI follow-up'), 'UTI',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Rhinitis follow-up'), 'Rhinitis',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Migraine follow-up'), 'Migraine',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Laceration follow-up')| (Reason == 'Laceration of right foot') | (Reason == 'Laceration of right calf') | (Reason == 'Laceration of left hand'),'Laceration',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Hypotension monitoring'), 'Hypotension',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Hypertension monitoring'), 'Hypertension monitoring',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Spotted fever rickettsiosis follow-up'), 'Spotted fever rickettsiosis',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Dermatitis follow-up'), 'Dermatitis',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Cyst removal follow-up'), 'Cyst removal',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Bronchitis follow-up'), 'Bronchitis',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Allergic reaction follow-up'), 'Allergic reaction',Reason))


# 1

# Reason for visit segmented (stacked bar chart)by month of the year. 
# convert the date column to date object 

df_patient_information$Month <- month(df_patient_information$VisitDate, label = TRUE)

df_visit_month <- df_patient_information %>%
  group_by(Reason, Month) %>%
  summarise(count = n()) 

ggplot(df_visit_month, aes(x = count, y = Reason, fill = Month)) +
  geom_bar(stat = "identity", width = 0.9) +
  labs(title = "Reason for visit by month",
       x = "Visit month",
       y = "Reason")

# 2

# Visit based on walk in or not. 
visit_walk_in <- df_patient_information %>%
  group_by(Reason, WalkIn) %>%
  summarize(count = n())

ggplot(visit_walk_in, aes(x = Reason, y = count, fill = WalkIn)) + 
  geom_bar(stat = "identity", width = 0.9) +
  labs(title = "Visit based on walk in", x = "WalkIn", y = "Count") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) 

# 3

#Reason for visit based on City/State or zip code
# Summarize the count of visits based on reason and city
summarized_data <- df_patient_information %>%
  group_by(Reason, City) %>%
  summarise(count = n())

ggplot(summarized_data, aes(x = City, y = count, fill = Reason)) + 
  geom_bar(stat = "identity") +
  labs(title = "Visit based on City", x = "City", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 4 

#Total invoice amount based on reason for visit. Segmented (stacked bar chart) with it was paid

summarized_invoice_data <- df_patient_information %>%
  group_by(Reason, InvoicePaid) %>%
  summarise(total_invoice_amount = sum(InvoiceAmt))

ggplot(summarized_invoice_data, aes(x = Reason, y = total_invoice_amount, fill = InvoicePaid)) + 
  geom_bar(stat = "identity") +
  labs(title = "Total invoice amount based on reason for visit", x = "Reason for Visit", y = "Total Invoice Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("skyblue", "pink"), name = "Invoice Paid") 

# 5 

#find one insight into the data that they find interesting
# Reason for visit grouped by the age of the patients

# First, calculate the age of the patients
df_patient_information$BirthDate <- ymd(df_patient_information$BirthDate)
df_patient_information$Age <- as.character(round((as.numeric(difftime(Sys.Date(), df_patient_information$BirthDate, units = "days")) / 365.25), 0))

df_patient_information %>%
  group_by(Age, Reason) %>%
  summarize(count = n()) 

ggplot(df_patient_information, aes(x = Age, fill = Reason)) + 
  geom_bar() +
  labs(title = "Visit based on Age", x = "Age group", y = "Count") +
  theme(axis.text = element_text(angle = 45, hjust = 1)) 
