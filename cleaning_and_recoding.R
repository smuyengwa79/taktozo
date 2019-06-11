library(readxl)
library(janitor)
data_new <- read_excel("recleaned_data.xlsx") %>%
  clean_names()

# Replace values for gender

data_new$gender_of_household_head <- ifelse(
  data_new$are_you_the_head_of_household == "Yes",
  as.character(
    data_new$indicate_gender_of_the_person_that_you_are_speaking_with
  ),
  as.character(data_new$gender_of_household_head)
)

# Marital Status
data_new$marital_status_of_household_head <- ifelse(
  data_new$are_you_the_head_of_household == "Yes",
  as.character(data_new$what_is_your_marital_status),
  as.character(data_new$marital_status_of_household_head)
)

# Ethnicity
data_new$ethnicity_of_the_head_of_household <- ifelse(
  data_new$are_you_the_head_of_household == "Yes",
  as.character(data_new$indicate_your_ethnic_group),
  as.character(data_new$ethnicity_of_the_head_of_household)
)

# Correct all the entries with "Other" category

data_new$ethnicity_of_the_head_of_household <- ifelse(
  data_new$ethnicity_of_the_head_of_household == "Other",
  as.character(data_new$specify_other_29),
  as.character(data_new$ethnicity_of_the_head_of_household)
)


# Education

data_new$education_status_of_head_of_household <- ifelse(
  data_new$are_you_the_head_of_household == "Yes",
  as.character(data_new$what_is_your_level_of_education),
  as.character(data_new$education_status_of_head_of_household)
)

# Employment status
data_new$employment_status_of_head_of_household <- ifelse(
  data_new$are_you_the_head_of_household == "Yes",
  as.character(data_new$are_you_formally_employed),
  as.character(data_new$employment_status_of_head_of_household)
)

library(forcats)
## Recoding data_new$employment_status_of_head_of_household into data_new$employment_status_of_head_of_household_rec
data_new$employment_status_of_head_of_household <-
  fct_recode(
    data_new$employment_status_of_head_of_household,
    "No" = "Other",
    "Yes" = "Employed",
    "No" = "Unemployed",
    "No" = "Self Employed",
    "No" = "Refused",
    "No" = "Don't Know"
  )
data_new$employment_status_of_head_of_household <-
  fct_explicit_na(data_new$employment_status_of_head_of_household, "No")
