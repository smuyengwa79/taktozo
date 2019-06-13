library(readxl)
library(janitor)
gef6 <- read_excel("UNDP_GEF6.xlsx") %>%
  clean_names()

# Replace values for gender

gef6$b_b3 <- ifelse(
  gef6$interviewee_hh == "Yes",
  as.character(gef6$interviewee_interviewee_gender),
  as.character(gef6$b_b3)
)

# Marital Status
gef6$b_b4 <- ifelse(
  gef6$interviewee_hh == "Yes",
  as.character(gef6$marital_s),
  as.character(gef6$b_b4)
)

# Ethnicity
gef6$b_b5 <- ifelse(
  gef6$interviewee_hh == "Yes",
  as.character(gef6$ethnicity),
  as.character(gef6$b_b5)
)
#
# # Correct all the entries with "Other" category
#
# gef6$ethnicity_of_the_head_of_household <- ifelse(
#   gef6$ethnicity_of_the_head_of_household == "Other",
#   as.character(gef6$specify_other_29),
#   as.character(gef6$ethnicity_of_the_head_of_household)
# )


# Education

gef6$b_b7 <- ifelse(
  gef6$interviewee_hh == "Yes",
  as.character(gef6$interviewee_education_level),
  as.character(gef6$b_b7)
)

# Employment status
gef6$b_b8 <- ifelse(
  gef6$interviewee_hh == "Yes",
  as.character(gef6$interviewee_occupation),
  as.character(gef6$b_b8)
)

# library(forcats)
# ## Recoding gef6$employment_status_of_head_of_household into gef6$employment_status_of_head_of_household_rec
# gef6$b_b8 <-
#   fct_recode(
#     gef6$employment_status_of_head_of_household,
#     "No" = "Other",
#     "Yes" = "Employed",
#     "No" = "Unemployed",
#     "No" = "Self Employed",
#     "No" = "Refused",
#     "No" = "Don't Know"
#   )
# gef6$b_b8<-
#   fct_explicit_na(gef6$b_b8, "No")

# Nutrition
gef6[,381:401] <- if_else(gef6[,381:401] == "Yes", 1, 0, 0)

gef6 <- gef6 %>% 
  mutate(nutrition = hdd_hddo2 +  hdd_hddo4 + hdd_hddo6 + hdd_hddo6 + 
           hdd_hddo8 + hdd_hddo10 + hdd_hddo12 + hdd_hddo14 + hdd_hddo16 + 
           hdd_hddo18 + hdd_hddo20)
# Nutrition Score 
gef6$nutrition_score <- ifelse(gef6$nutrition > 4, 0, 1)

# Education scoring for HHH and members failing to attend school
gef6$education_score <- ifelse(gef6$b_b7 > 2, 0, 1)

gef6$attendance_score <- ifelse(gef6$section_c_c6 == "Yes", 1, 0)

# Living standards
gef6$cooking_score <- if_else(gef6$i_i2 > 4, 0, 1, 1)
gef6$sanitation_score <- if_else(gef6$i_i1 > 2, 0,1,1)

# Drinking water
gef6$water_score <- if_else(gef6$i_i3 >2, 0, 1, 1)
#Electricity
gef6$electricity_score <- if_else(gef6$i_i2 == 6 & gef6$i_i4 == 6, 0,1,1)

# Housing 
gef6$housing_score <- if_else(gef6$ha_h1 < 4 | gef6$ha_h3 < 3 | gef6$ha_h2 < 5, 1,0,1)

#Assets 
gef6[,114:123] <- if_else(gef6[,114:123] > 0, 1, 0, 0)
gef6 <- gef6 %>% 
  mutate(assets = ga_g1 + ga_g2 + ga_g3 + ga_g4 + ga_g5 + ga_g6 + 
           ga_g7 + ga_g8 + ga_g9 + ga_g10)
gef6$asset_score <- if_else(gef6$assets >1, 0, 1, 1)

######################## Apply weights to the scores ###############

gef6 <- gef6 %>% 
  mutate (nutrition_score = nutrition_score*(1/3), 
          education_score = education_score*(1/6), 
          attendance_score = attendance_score*(1/6), 
          cooking_score = cooking_score*(1/18), 
          sanitation_score = sanitation_score*(1/18), 
          water_score = water_score*(1/18), 
          electricity_score = electricity_score*(1/18), 
          housing_score = housing_score*(1/18), 
          asset_score = asset_score*(1/18))

gef6 <- gef6 %>% 
  mutate(mpi_index = nutrition_score + education_score + attendance_score + 
           cooking_score + sanitation_score + water_score + electricity_score + 
           housing_score + asset_score)
gef6$mpi_poor <- if_else(gef6$mpi_index >= 0.3333, "MPI Poor", "Not Poor")

mpi_data <- gef6 %>%  select(a_a2, mpi_poor, mpi_index)
mp <- descrTable(a_a2~., data = mpi_data, show.all = TRUE)
 export2csv(mp, file = "mpdi_index.csv")

p_data <- gef6 %>% 
  select(a_a2, nutrition_score,nutrition_score:housing_score, asset_score)
p_dataL <- p_data %>% gather("Key", "Value", -a_a2)

ggplot(p_dataL, aes(a_a2, Value, fill = Key)) + geom_col()

