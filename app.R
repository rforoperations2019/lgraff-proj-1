#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

#Prepare the dataset
# Read and examine fields of dataset
df_NY_schools <- read.csv(file = "GRAD_RATE_AND_OUTCOMES_2018.csv")

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

# Check to see if the number of reported dropouts makes sense
df_NY_schools_corrected$dropout_check <- df_NY_schools_corrected$ENROLL_CNT - 
  df_NY_schools_corrected$GRAD_CNT -
  df_NY_schools_corrected$STILL_ENR_CNT

# Remove unnecessary columns
df_NY_schools_corrected <- df_NY_schools_corrected %>% 
  select(-LEA_BEDS, -LEA_NAME, -NRC_DESC,
         -NYC_IND, -BOCES_CODE, -MEMBERSHIP_CODE, -MEMBERSHIP_KEY)

# Subset by statewide so have statewide reference for individual counties
# Let's also choose the 4 year outcome of the 2014 cohort (dataset is backwards looking)
df_state <- subset(df_NY_schools_corrected,
                   subset = AGGREGATION_TYPE == 'Statewide' &
                     MEMBERSHIP_DESC == '2014 Total Cohort - 4 Year Outcome - August 2018')

# Subset data to a single aggregation index. In this case, we will choose county
df_county <- subset(x = df_NY_schools_corrected,
                    subset = AGGREGATION_TYPE == "County" &
                      MEMBERSHIP_DESC == '2014 Total Cohort - 4 Year Outcome - August 2018')

# Create dataset of county-level appended to state-level
df_county_state <- rbind(df_county, df_state)


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

# Create list of counties
county_names <- all_stud_long_bar %>% 
  filter(AGGREGATION_TYPE != "Statewide")

county_names$AGGREGATION_TYPE <- as.character(county_names$AGGREGATION_TYPE)

# Create individual subgroup datasets
df_gender <- df_county %>% 
  filter(SUBGROUP_NAME %in% c('Male', 'Female'))

df_race <- df_county %>% 
  filter(SUBGROUP_CODE %in% c(4,5,6,7,8,9))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   
   #hello
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),

      selectInput(inputId = "county", "County",
                  choices = unique(county_names$AGGREGATION_NAME))
      ),
   
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selected_county <- reactive({
    all_stud_long_bar %>%
    filter(AGGREGATION_NAME == input$county)
  })
  
  output$distPlot <- renderPlot({
    
    ggplot(selected_county()) +
      geom_bar(stat = "identity",
               aes(x = variable, y = value, fill = AGGREGATION_NAME))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

