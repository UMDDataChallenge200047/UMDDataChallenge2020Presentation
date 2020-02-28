# How have the household characteristics (e.g., household composition, income, etc.) of HUD’s
# PH, MH and HCV programs changed from 2009 to 2018?

# load the data into separate dataframes
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(pastecs)
#hud09 <- read_xlsx("DataChallengeData/Data_Level2_HUD_HUDPrograms_2009.xlsx")
#hud14 <- read_xlsx("DataChallengeData/Data_Level2_HUD_HUDPrograms_2014.xlsx")
#hud18 <- read_xlsx("DataChallengeData/Data_Level2_HUD_HUDPrograms_2018.xlsx")

# Ideas: Explore the demographic changes between the years - maybe see how the gentrification of housing affects 
# Set up housing dashboard? What about allowing public worker to use the program effectively?
# Can they learn it quickly? Maybe more effective with Power BI
# Should the data be private? Is it widely available and should we use open source?
# Data exploration: Household(Family size) / Income / Housing programs
# Can't get the data on location, but can chalk it up as a potential improvement in the future
# Hypothesis that there are more adult children in household - learned 2018 that more adult children living with family since unable to get job or can't pay bills

# Need to remove occurrences of "." in

# Remove all of the "." in the dataset and assume that these represent individuals who applied to the programs and failed.
#hud09 <- hud09[hud09$gross_rent_amnt_rounded != ".",]
#hud09 <- hud09[hud09$total_fmly_crbtn_amnt_rounded != ".",]
#hud14 <- hud14[hud14$gross_rent_amnt_rounded != ".",]
#hud14 <- hud14[hud14$total_fmly_crbtn_amnt_rounded != ".",]
#hud18 <- hud18[hud18$gross_rent_amnt_rounded != ".",]
#hud18 <- hud18[hud18$total_fmly_crbtn_amnt_rounded != ".",]

##family09 <- hud09 %>% select(HSHD_MBR_TOTAL_CNT, TOTAL_DPNDNT_CNT, )
##head09 <- hud09 %>% select(HEAD_GNDR_CD, HEAD_AGE_YR_CNT, HEAD_ELDLY_INDR, HEAD_DSBLTY_INDR, HEAD_RACE_CD, HEAD_ETHNCY_CD, H6_CD)
#house09 <- hud09 %>% select(CHLDRN_AGE_0_3_CNT, CHLDRN_AGE_4_5_CNT, CHLDRN_AGE_6_12_CNT, CHLDRN_AGE_13_17_CNT, ADLT_AGE_18_21_CNT, ADLT_AGE_22_25_CNT, ADLT_AGE_26_35_CNT, ADLT_AGE_36_49_CNT, ADLT_AGE_50_61_CNT, ADLT_AGE_62_85_CNT, ADLT_AGE_ABOVE85_CNT)

#house14 <- hud14 %>% select(CHLDRN_AGE_0_3_CNT, CHLDRN_AGE_4_5_CNT, CHLDRN_AGE_6_12_CNT, CHLDRN_AGE_13_17_CNT, ADLT_AGE_18_21_CNT, ADLT_AGE_22_25_CNT, ADLT_AGE_26_35_CNT, ADLT_AGE_36_49_CNT, ADLT_AGE_50_61_CNT, ADLT_AGE_62_85_CNT, ADLT_AGE_ABOVE85_CNT)

#house18 <- hud18 %>% select(CHLDRN_AGE_0_3_CNT, CHLDRN_AGE_4_5_CNT, CHLDRN_AGE_6_12_CNT, CHLDRN_AGE_13_17_CNT, ADLT_AGE_18_21_CNT, ADLT_AGE_22_25_CNT, ADLT_AGE_26_35_CNT, ADLT_AGE_36_49_CNT, ADLT_AGE_50_61_CNT, ADLT_AGE_62_85_CNT, ADLT_AGE_ABOVE85_CNT)

# Plot the graphs for the number people in each household
library(ggplot2)

#familygraph09 = ggplot(house09, aes(age)) + geom_histogram(binwidth = 10) + facet_wrap(~)
#familygraph09

# take complete data set and remove dots and create another dataset with just the dots
hudall <- read_xlsx("DataChallengeData/Data_Level2_HUD_HUDPrograms_Fulldataset.xlsx")
#Editing hudall so that the Race is displayed as a string rather than an int:
hudall$HEAD_RACE_CD[hudall$HEAD_RACE_CD == 1] <- "White"
hudall$HEAD_RACE_CD[hudall$HEAD_RACE_CD == 2] <- "Black"
hudall$HEAD_RACE_CD[hudall$HEAD_RACE_CD == 3] <- "Native American"
hudall$HEAD_RACE_CD[hudall$HEAD_RACE_CD == 4] <- "Asian"
hudall$HEAD_RACE_CD[hudall$HEAD_RACE_CD == 5] <- "Hawaiian / Pacific Islander"
hudall$HEAD_RACE_CD[hudall$HEAD_RACE_CD == 6] <- "More than 1"
hudall$pgm_type_edited[hudall$pgm_type_edited == 1] <- "Public Housing"
hudall$pgm_type_edited[hudall$pgm_type_edited == 2] <- "HCVP"
hudall$pgm_type_edited[hudall$pgm_type_edited == 3] <- "Multifamily Housing"



# separates data into complete and incomplete data - data incomplete in the reporting of the program variables: gross home expense for Homeownership, HUD's payment, and total amount family contributes to housing including utility. 

##Changing so that hudnodot no longer includes gross rent
hudnodot <- hudall[(hudall$gross_rent_amnt_rounded != ".") | (hudall$total_fmly_crbtn_amnt_rounded != ".") ,]
#hudnodot <- hudall[hudall$total_fmly_crbtn_amnt_rounded != ".",]
#hudwithdot <- hudall[ (hudall$total_fmly_crbtn_amnt_rounded == ".") ,]
hudwithdot <- hudall[((hudall$gross_rent_amnt_rounded == ".") | (hudall$total_fmly_crbtn_amnt_rounded == ".")) ,]


# writing it to an csv file
write.csv(hudnodot, "DataChallengeData/Data_Level2_HUD_HUDPrograms_Fulldatasetwithoutdot.csv")
write.csv(hudwithdot, "DataChallengeData/Data_Level2_HUD_HUDPrograms_Fulldatasetwithdot.csv")



## Create R Shiny analyzing the data
#summarize dataset and apply the values in this link (https://www.statmethods.net/stats/descriptives.html) to BOTH the hudnodot and hudwithdot datasets along with the household variables/ income variables / program+external variables.
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("HUD Data Analysis"),
  
  # Sidebar with a slider input for number of bins in histogram and a radio button for analyzing the complete data vs incomplete data (ones with .)
  sidebarLayout(
    sidebarPanel(
      radioButtons("dots", label = "Complete or Incomplete data set:",choices = c("Complete","Incomplete")),
      radioButtons("year", label = "Year of the HUD Census:", choices = as.list(unique(hudall$Year)))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        plotOutput("Household"),
        plotOutput("FacetTest")
      ),
      fluidRow(
        verbatimTextOutput("info")
      )
    )
  )
  
)

server <- function(input, output) {
  
  plotInput <- reactive({
    if (input$dots == "Complete"){
      changingdat <- hudnodot %>% subset(hudnodot$Year == input$year)
    } 
    else{
      changingdat <- hudwithdot %>% subset(hudwithdot$Year == input$year)
    }
  })
  
  
  
  output$Household <- renderPlot({
    house <- plotInput() %>% select(CHLDRN_AGE_0_3_CNT, CHLDRN_AGE_4_5_CNT, CHLDRN_AGE_6_12_CNT, CHLDRN_AGE_13_17_CNT, ADLT_AGE_18_21_CNT, ADLT_AGE_22_25_CNT, ADLT_AGE_26_35_CNT, ADLT_AGE_36_49_CNT, ADLT_AGE_50_61_CNT, ADLT_AGE_62_85_CNT, ADLT_AGE_ABOVE85_CNT) 
    house <- house %>% setNames(c("00-03", "04-05", "06-12", "13-17", "18-21", "22-25", "26-35", "36-49", "50-61", "62-85", "86+"))
    housesum <- house %>% colSums() %>% as.list() %>% data.frame() %>% rownames_to_column("ID") %>% mutate(ID = as.numeric(ID)) %>% gather(agegroup, total, -ID)
      
    ggplot(housesum, aes(x = agegroup, y = total/sum(total), col = agegroup)) + geom_histogram(stat = "identity", show.legend = TRUE) 
  })
  
  output$info <- renderPrint ({
    #house <- plotInput() %>% select(CHLDRN_AGE_0_3_CNT, CHLDRN_AGE_4_5_CNT, CHLDRN_AGE_6_12_CNT, CHLDRN_AGE_13_17_CNT, ADLT_AGE_18_21_CNT, ADLT_AGE_22_25_CNT, ADLT_AGE_26_35_CNT, ADLT_AGE_36_49_CNT, ADLT_AGE_50_61_CNT, ADLT_AGE_62_85_CNT, ADLT_AGE_ABOVE85_CNT) 
    
    house <- plotInput() %>% select(HEAD_RACE_CD, TOTAL_ANNL_INCM_AMNT)
    #white <- subset(house, house$HEAD_RACE_CD == "White")
    #black <- subset(house, house$HEAD_RACE_CD == "Black")
    #asian <- subset(house, house$HEAD_RACE_CD == "Asian")
    #NativeAmerican <- subset(house, house$HEAD_RACE_CD == "Native American")
    #hpi <- subset(house, house$HEAD_RACE_CD == "Hawaiian / Pacific Islander")
    #oneplus <- subset(house, house$HEAD_RACE_CD == "More than 1")
    
    df <- aggregate(house, by = list(Race = house$HEAD_RACE_CD), FUN = stat.desc)
    
    Filter(function(x)!all(is.na(x)), df)
    #summary(house)
    #options(scipen=100)
    #options(digits=3)
    #summary(white)
    #summary(black)
    #summary(asian)
    #summary(NativeAmerican)
    #summary(hpi)
    #summary(oneplus)
    
    ###Try to get ethnicity vs household income instead
    
    
  })
  
  output$FacetTest <- renderPlot ({
    ggplot(plotInput(), aes(x=TOTAL_DPNDNT_CNT, y = TOTAL_ANNL_INCM_AMNT)) + geom_point(shape = 1) + facet_grid(pgm_type_edited ~ HEAD_RACE_CD)
    
    #Other variables to compare with annual income:
    #TOTAL_DPNDNT_CNT
    #HEAD_AGE_YR_CNT
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)