setwd("/Users/jtorres0/Desktop/Public Pulse/") # change depending where the recent roper data dump is

library(tidyverse)
library(writexl)
library(readxl)

#### load in roper export + initial filtering
roper_data <- read_csv("Stanford_export_2026-02-03.csv", col_types = cols(QuestionID = col_character())) %>%
  mutate(
    BegDate = as.Date(BegDate),
    EndDate = as.Date(EndDate),
    ReleaseDate = as.Date(ReleaseDate),
    Database = "Roper"
  ) %>%
  select(-DatePublished) %>%
  # Filter to gun policy topics
  filter(str_detect(str_to_lower(Topics), "guns")) %>%
  # Filter to dates >= 1/1/2022
  filter(!is.na(EndDate) & EndDate >= as.Date("2022-01-01"))

### filtering to "approved orgs"
# Although note that no questions were filtered out.
# all questions in the export, after filtering to gun topic, already contain at least
# one of our approved orgs.
approved_pattern <- paste(c(
  "Gallup",
  "Pew",
  "Quinnipiac",
  "Wall Street Journal",
  "Fox News",
  "New York Times",
  "Washington Post",
  "CNN",
  "Marist",
  "NPR",
  "PBS",
  "Associated Press",
  "AP-NORC",
  "NORC",
  "GSS",
  "ANES",
  "Marquette",
  "ABC News"), collapse = "|")

roper_data <- roper_data %>%
  filter(str_detect(SurveyOrg, approved_pattern) | str_detect(SurveySponsor, approved_pattern))

### filtering for specific samples
# note that i filtered our likely voters and Exiting/early/absentee voters
# I decided to keep the survey that samples national gun owners for now
# I know this is not a representative sample but i can see a future where this might be insightful
#most of these get filtered out in non-policy anyway...see the google sheet for exact questions
roper_data <- roper_data %>%
  filter(str_detect(SampleDesc, "^National")) %>%
  filter(!str_detect(SampleDesc, "^National Black|^National black|^National adult Hispanic|^National adult Asian|^National likely|^National parents")) %>%
  filter(!is.na(SampleSize)) # this filters out 1 quinnipiac question that has no sample size...

### transforming the questionID column
roper_data <- roper_data %>%
  mutate(QuestionID = paste0("R", str_replace(QuestionID, "\\.", ".Q")))

### filtering out non-policy
coding_sheet <- roper_data %>%
  distinct(QuestionID, SurveySponsor, SurveyOrg, EndDate, SampleTypes, QuestionTxt) %>%
  arrange(EndDate) %>%
  mutate(`Policy?` = "")  # blank column you fill in manually

write_xlsx(coding_sheet, "question_coding.xlsx") # this is where i manually read them all and assign to policy/non policy

coding_key <- read_xlsx("question_coding_finished.xlsx") %>%
  select(QuestionID, `Policy?`) %>%
  filter(is.na(`Policy?`) | `Policy?` != "Non-Policy")

roper_data <- roper_data %>%
  semi_join(coding_key, by = "QuestionID")

### loading in non roper data
non_roper_data <- read_csv("NonRoperGunPolicyData - Non Roper Data.csv") %>% # this is worksheet 2 in our google doc!!!
  mutate(
    BegDate = as.Date(BegDate, format = "%m/%d/%Y"),
    EndDate = as.Date(EndDate, format = "%m/%d/%Y"),
    ReleaseDate = as.Date(ReleaseDate, format = "%m/%d/%Y"),
    SampleSize = as.numeric(SampleSize),
    RespPct = as.numeric(RespPct))

### combining roper and non-roper
combined_data <- bind_rows(roper_data, non_roper_data)

###checking to make sure there are no NAs in key columns
combined_data %>%
  filter(is.na(SampleSize)) %>%
  select(QuestionID, RespTxt, QuestionTxt, SurveySponsor, EndDate, SampleDesc, SampleSize)

### write the final csv
write_csv(combined_data, "GunPolicyDatabase.csv")
