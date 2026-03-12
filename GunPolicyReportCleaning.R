setwd("/Users/jtorres0/Desktop/Public Pulse/") # change depending where the recent roper data dump is

library(tidyverse)

#### load in roper export + initial filtering
roper_data <- read_csv("Stanford_export_2026-02-03.csv", col_types = cols(QuestionID = col_character())) %>%
  mutate(
    BegDate = as.Date(BegDate),
    EndDate = as.Date(EndDate),
    ReleaseDate = as.Date(ReleaseDate),
    Database = "Roper"
  ) %>%
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
# I know this is not a representative sample but i can see a fututre where this might be insightful

approved_samples <- c(
  "Adults",
  "Adults;Parents/guardians",
  "Registered voters",
  "Miscellaneous")

roper_data <- roper_data %>%
  filter(SampleTypes %in% approved_samples)

# loading in 
