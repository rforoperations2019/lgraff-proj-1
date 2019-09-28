
library(dplyr)
library(ggplot2)
library(reshape2)
library(Hmisc)

# Read and examine fields of dataset
df_NY_schools <- read.csv(file = "GRAD_RATE_AND_OUTCOMES_2018.csv")
names(df_NY_schools)
str(df_NY_schools)

df_NY_schools[, c(20:36)] <- sapply(df_NY_schools[, c(20:36)], as.character)

# Change the string columns with the percent symbols to numeric percentages
to_percent <- function(orig_text) {
  without_symbol <- substr(orig_text,1, nchar(orig_text)-1)
  to_number <- as.numeric(without_symbol)
  to_percent <- to_number / 100
  return(to_percent)
}

new_pct <- sapply(df_NY_schools[, c("GRAD_PCT", "REG_PCT", "STILL_ENR_PCT", "GED_PCT",
                                    "DROPOUT_PCT", "LOCAL_PCT", "REG_ADV_PCT",
                                    "NON_DIPLOMA_CREDENTIAL_PCT")], to_percent)

df_NY_schools_withoutpct <- df_NY_schools %>% 
  select(-c("GRAD_PCT", "REG_PCT", "STILL_ENR_PCT", "GED_PCT",
            "DROPOUT_PCT", "LOCAL_PCT", "REG_ADV_PCT",
            "NON_DIPLOMA_CREDENTIAL_PCT"))

df_NY_schools_corrected <- cbind(df_NY_schools_withoutpct, new_pct)

# Now convert the counts to numeric
df_NY_schools_corrected[, c(20:28)] <- sapply(df_NY_schools_corrected[, c(20:28)],
                                              as.numeric)

# Change county name format to a capital first letter and rest lowercase
df_NY_schools_corrected$COUNTY_NAME <- capitalize(tolower(df_NY_schools_corrected$COUNTY_NAME))

# Check to see if the number of reported dropouts makes sense
df_NY_schools_corrected$dropout_check <- df_NY_schools_corrected$ENROLL_CNT - 
  df_NY_schools_corrected$GRAD_CNT -
  df_NY_schools_corrected$STILL_ENR_CNT

# Remove unnecessary columns
df_NY_schools_corrected <- df_NY_schools_corrected %>% 
  select(-LEA_BEDS, -LEA_NAME, -NRC_DESC,
         -NYC_IND, -BOCES_CODE, -MEMBERSHIP_CODE, -MEMBERSHIP_KEY)

# Subset by statewide so have statewide reference for individual counties
# Remove one of the 4 year outcomes of the 2014 cohort (there is a june & august value)
# Choose August as it is more recent
df_state <- subset(df_NY_schools_corrected,
                   subset = AGGREGATION_TYPE == 'Statewide' &
                     MEMBERSHIP_DESC != '2014 Total Cohort - 4 Year Outcome')

# Subset data to a single aggregation index. In this case, we will choose county
df_county <- subset(x = df_NY_schools_corrected,
                    subset = AGGREGATION_TYPE == "County" &
                      MEMBERSHIP_DESC != '2014 Total Cohort - 4 Year Outcome')

# Create dataset of county-level appended to state-level
df_county_state <- rbind(df_county, df_state)

# Scatterplot dataset to see how graduation rates have changed over time, for 2012/13/14 cohorts
df_state_allcohort <- df_state %>% 
  filter(SUBGROUP_NAME == "All Students") %>% 
  mutate(Year = substr(MEMBERSHIP_DESC, 1, 4))

cohort_melted <- melt(df_state_allcohort, id.vars = "Year")
cohort_subset <- cohort_melted %>%
  filter(variable %in% c("GRAD_PCT", "DROPOUT_PCT", "STILL_ENR_PCT"))
cohort_subset$value <- as.numeric(cohort_subset$value)


ggplot(cohort_subset) +
  geom_point(aes(x = Year, y = value, color = variable)) +
  ylab("") +
  ggtitle("Graduation, Dropout, and Still-Enrolled Rates by Cohort") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(breaks=seq(0,1,.10))



# First look at data for all students
all_stud <- df_county_state %>% 
  filter(SUBGROUP_NAME == "All Students") %>% 
  arrange(desc(DROPOUT_CNT))

all_stud_long <- melt(all_stud,
                      id.vars = c("AGGREGATION_TYPE", "AGGREGATION_NAME", "AGGREGATION_CODE"))

# Subset by variables of interest for bar chart
all_stud_long_bar <- all_stud_long %>% 
  filter(variable %in% c('GRAD_PCT', 'DROPOUT_PCT', 'STILL_ENR_PCT', 'GED_PCT'))

all_stud_long_bar$value = sapply(all_stud_long_bar$value, as.numeric)

t1 <- df_county %>% 
  filter(AGGREGATION_NAME == "County: BRONX" & SUBGROUP_NAME == "All Students")
enrolled <- t1$ENROLL_CNT

# use this to test the bar chart
test<- all_stud_long_bar %>%  
  filter(AGGREGATION_NAME %in% c("All Districts and Charters", "County: BRONX") &
           year == '2014') 

# Bar chart displaying graduation, dropout, still enrolled, and GED percent
ggplot(data = test) +
  geom_bar(stat = "identity",
           aes(x = variable, y = value, fill = AGGREGATION_NAME),
           position = 'dodge') +
  xlab("") +
  ylab('Rate') + 
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank())


ggplot(data = all_stud) +
  geom_histogram(aes(x = ENROLL_CNT), fill = 'red', alpha = .4)


df_gender <- df_county %>% 
  filter(SUBGROUP_NAME %in% c('Male', 'Female'))

# Test the pie chart with the Bronx
bronx_test<- df_gender %>%  
  filter(AGGREGATION_NAME %in% c("County: BRONX")) %>% 
  arrange(desc(SUBGROUP_NAME)) %>% 
  mutate(y_label_pos = cumsum(ENROLL_CNT) - .5*ENROLL_CNT)


# Test: dropout rate by gender for a given county
ggplot(data = bronx_test) +
  geom_bar(stat = "identity",
           aes(x = SUBGROUP_NAME, y = DROPOUT_PCT))

# Test: pie chart by gender for a given county
ggplot(bronx_test, aes(x = "", y = ENROLL_CNT, fill = SUBGROUP_NAME)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = y_label_pos, label = ENROLL_CNT)) +
  theme_void() +
  theme(legend.title = element_blank()) +
  ggtitle("Gender Demographics")

df_race <- df_county %>% 
  filter(SUBGROUP_CODE %in% c(4,5,6,7,8,9))
         
ggplot(data = df_gender) + 
  geom_histogram(aes(x = DROPOUT_PCT, fill = SUBGROUP_NAME), alpha = .5)
# maybe add a vertical line for the means

ggplot(data = df_race) + 
  geom_bar(stat = "identity", aes(x = SUBGROUP_NAME, y = DROPOUT_PCT))


# Testing graphs
test<- all_stud_long_bar %>%  
  filter(AGGREGATION_NAME %in% c("All Districts and Charters", "County: BRONX") &
           Year == '2014') 

ggplot(data = test) +
  geom_bar(stat = "identity",
           aes(x = variable, y = value, fill = AGGREGATION_NAME),
           position = "dodge")


# Demographics
dem_test <- df_county %>% 
  filter(SUBGROUP_CODE %in% c(2,3)) %>% 
  filter(COUNTY_NAME == "BRONX" & Year == "2014")

dem_melt <- melt(df_county, id.vars = c("SUBGROUP_CODE", "SUBGROUP_NAME",
                                       "COUNTY_NAME", "Year")) %>% 
  filter(variable %in% c("GRAD_PCT", "DROPOUT_PCT", "STILL_ENR_PCT")) 

dem_melt$value <- as.numeric(dem_melt$value)

test <- df_county %>% 
  filter(SUBGROUP_CODE %in% c(4,5,6,7,9)) %>% 
  filter(COUNTY_NAME == "YATES" & Year == '2014') %>% 
  select(ENROLL_CNT)

ggplot(data = dem_melt_subset) +
  geom_bar(stat = "identity",
           aes(x = variable, y = value, fill = SUBGROUP_NAME),
           position = "dodge")
