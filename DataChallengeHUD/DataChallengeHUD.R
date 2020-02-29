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

hudall %>% transform(ASSTN_PYMNT_AMNT = as.numeric(ASSTN_PYMNT_AMNT)) %>% transform(GROSS_RENT_AMNT = as.numeric(GROSS_RENT_AMNT))
# separates data into complete and incomplete data - data incomplete in the reporting of the program variables: gross home expense for Homeownership, HUD's payment, and total amount family contributes to housing including utility. 

###### Public housing is where gross_rent_amnt is absent, total_fmly_crbtn_amnt_rounded is where  
##Changing so that otherhouse no longer includes gross rent
#otherhouse <- hudall[(hudall$GROSS_RENT_AMNT != ".") & (hudall$TOTAL_FMLY_CRBTN_AMNT != ".") ,]
otherhouse <- hudall[hudall$GROSS_RENT_AMNT != ".",]
publichouse <- hudall[ (hudall$total_fmly_crbtn_amnt_rounded == ".") ,]
#publichouse <- hudall[((hudall$gross_rent_amnt_rounded == ".") | (hudall$total_fmly_crbtn_amnt_rounded == ".")) ,]


# writing it to an csv file
#write.csv(otherhouse, "DataChallengeData/Data_Level2_HUD_HUDPrograms_ASSTN_PYMNT.csv")
#write.csv(publichouse, "DataChallengeData/Data_Level2_HUD_HUDPrograms_PUBLIC_HOUSING.csv")



## Create R Shiny analyzing the data
#summarize dataset and apply the values in this link (https://www.statmethods.net/stats/descriptives.html) to BOTH the otherhouse and publichouse datasets along with the household variables/ income variables / program+external variables.
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("HUD Data Analysis"),
  
  # Sidebar with a slider input for number of bins in histogram and a radio button for analyzing the complete data vs incomplete data (ones with .)
  sidebarLayout(
    sidebarPanel(
      radioButtons("dots", label = "Type of Housing Program",choices = c("HCVP and Multifamily","Public")),
      radioButtons("year", label = "Year of the HUD Census:", choices = as.list(unique(hudall$Year)))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        plotOutput("Household"),
        plotOutput("FacetTest"),
        plotOutput("LinearRegression")
      ),
      fluidRow(
        verbatimTextOutput("info"),
        verbatimTextOutput("prediction")
        
      )
    )
  )
  
)

server <- function(input, output) {
  
  plotInput <- reactive({
    if (input$dots == "HCVP and Multifamily"){
      changingdat <- otherhouse %>% subset(otherhouse$Year == input$year) %>% transform(ASSTN_PYMNT_AMNT = as.numeric(ASSTN_PYMNT_AMNT)) %>% transform(GROSS_RENT_AMNT = as.numeric(GROSS_RENT_AMNT))
    } 
    else{
      changingdat <- publichouse %>% subset(publichouse$Year == input$year)
    }
  })
  
  
  
  output$Household <- renderPlot({
    house <- plotInput() %>% select(CHLDRN_AGE_0_3_CNT, CHLDRN_AGE_4_5_CNT, CHLDRN_AGE_6_12_CNT, CHLDRN_AGE_13_17_CNT, ADLT_AGE_18_21_CNT, ADLT_AGE_22_25_CNT, ADLT_AGE_26_35_CNT, ADLT_AGE_36_49_CNT, ADLT_AGE_50_61_CNT, ADLT_AGE_62_85_CNT, ADLT_AGE_ABOVE85_CNT) 
    house <- house %>% setNames(c("00-03", "04-05", "06-12", "13-17", "18-21", "22-25", "26-35", "36-49", "50-61", "62-85", "86+"))
    housesum <- house %>% colSums() %>% as.list() %>% data.frame() %>% rownames_to_column("ID") %>% mutate(ID = as.numeric(ID)) %>% gather(agegroup, total, -ID)
      
    ggplot(housesum, aes(x = agegroup, y = total/sum(total), col = agegroup)) + geom_histogram(stat = "identity", show.legend = TRUE) 
  })
  
  output$info <- renderPrint ({
    #house <- plotInput() %>% select(HEAD_RACE_CD, TOTAL_ANNL_INCM_AMNT)
    #df <- aggregate(house, by = list(Race = house$HEAD_RACE_CD), FUN = stat.desc)
    #Filter(function(x)!all(is.na(x)), df)
    
    house <- plotInput() %>% select(HEAD_RACE_CD, ASSTN_PYMNT_AMNT)
    df <- aggregate(house, by = list(Race = house$HEAD_RACE_CD), FUN = stat.desc)
    Filter(function(x)!all(is.na(x)), df)
    
    
  })
  
  output$prediction <- renderPrint ({
    predict_pymnt <- otherhouse %>% select(Year, HEAD_RACE_CD, GROSS_RENT_AMNT, ASSTN_PYMNT_AMNT) %>% transform(ASSTN_PYMNT_AMNT = as.numeric(ASSTN_PYMNT_AMNT)) %>% transform(GROSS_RENT_AMNT = as.numeric(GROSS_RENT_AMNT))
    
    df <- split(predict_pymnt, predict_pymnt$HEAD_RACE_CD)
    library(lattice)
    library(dplyr)
    library(broom)
    
    ##Another way of doing this, but not as easy to untangle
    #fitted_models = predict_pymnt %>% group_by(HEAD_RACE_CD) %>% do(model = lm(ASSTN_PYMNT_AMNT ~ Year, data = .))
    #LinearModel <- fitted_models %>% tidy(model)
    
  #Supposedly could compare the individual trends with the overall mixed trends, but keep getting non-numeric argument to mathematical function for the lme() function
    ##https://stackoverflow.com/questions/1169539/linear-regression-and-group-by-in-r
    #library(nlme)
    #lme(ASSTN_PYMNT_AMNT ~ Year, random = ~Year|HEAD_RACE_CD, correlation = corAR1(~Year))
    
    LinearModel <- lmList(ASSTN_PYMNT_AMNT ~ Year | HEAD_RACE_CD, data=predict_pymnt)
    future_asstm <- data.frame(HEAD_RACE_CD = c(unique(predict_pymnt$HEAD_RACE_CD)), Year = c(2023, 2023, 2023, 2023, 2023, 2023) )
    
    LinearModel
    predict(LinearModel, newdata = future_asstm)
    #LinearModel <- sapply(df, function(x) lm(predict_pymnt$ASSTN_PYMNT_AMNT ~ predict_pymnt$Year, x))
    
    
  })
  
  output$FacetTest <- renderPlot ({
    predict_pymnt <- otherhouse %>% select(Year, HEAD_RACE_CD, GROSS_RENT_AMNT, ASSTN_PYMNT_AMNT) %>% transform(ASSTN_PYMNT_AMNT = as.numeric(ASSTN_PYMNT_AMNT)) %>% transform(GROSS_RENT_AMNT = as.numeric(GROSS_RENT_AMNT))
    ggplot(predict_pymnt, aes(x=GROSS_RENT_AMNT, y = ASSTN_PYMNT_AMNT)) + geom_point(shape = 1) + stat_smooth(method = "lm", col = "dodgerblue3") +
facet_grid(Year ~ HEAD_RACE_CD)
    
    #ggplot(plotInput(), aes(x=gross_rent_amnt_rounded, y = asstn_pymnt_amnt_rounded)) + geom_point(shape = 1) + facet_grid(pgm_type_edited ~ HEAD_RACE_CD)
    
    
    #Other variables to compare with annual income:
    #TOTAL_DPNDNT_CNT
    #HEAD_AGE_YR_CNT
  })

  
  output$LinearRegression <- renderPlot ({
    predict_pymnt <- otherhouse %>% select(Year, HEAD_RACE_CD, GROSS_RENT_AMNT, ASSTN_PYMNT_AMNT) %>% transform(ASSTN_PYMNT_AMNT = as.numeric(ASSTN_PYMNT_AMNT)) %>% transform(GROSS_RENT_AMNT = as.numeric(GROSS_RENT_AMNT))
    
    ggplot(data = predict_pymnt, aes(x = GROSS_RENT_AMNT, y = ASSTN_PYMNT_AMNT, color = HEAD_RACE_CD))    + 
      geom_point() +
      stat_smooth(method = "lm", col = "dodgerblue3") +
      theme(panel.background = element_rect(fill = "white"),
            axis.line.x=element_line(),
            axis.line.y=element_line()) +
      ggtitle("Linear Model Fitted to Data")
    
    ###3D prediction
    
    #fit_1 <- lm(predict_pymnt$ASSTN_PYMNT_AMNT ~ predict_pymnt$HEAD_RACE_CD, data = predict_pymnt)
    #ggplot(data=predict_pymnt, aes(fit_1$residuals)) +
    #  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
    #  theme(panel.background = element_rect(fill = "white"),
    #        axis.line.x=element_line(),
    #        axis.line.y=element_line()) +
    #  ggtitle("Histogram for Model Residuals") 
    
    #fit_2 <- lm(ASSTN_PYMNT_AMNT ~ Year + GROSS_RENT_AMNT, data = predict_pymnt)
    #Time <- seq(2009, 2018, by = 1)
    #Rent <- seq(min(predict_pymnt$GROSS_RENT_AMNT), max(predict_pymnt$GROSS_RENT_AMNT), by = 778)
    #pred_grid <- expand.grid(Year = Time, Rent = Rent)
    #pred_grid$ASSTN_PYMNT_AMNT <-predict(fit_2, new = pred_grid, type = "class")
    #fit_2_sp <- scatterplot3d(pred_grid$Time, pred_grid$Rent, pred_grid$pymnt, angle = 60, color = "dodgerblue", pch = 1, ylab = "Rent", xlab = "Year", zlab = "ASSTN_PYMNT_AMNT" )
    #fit_2_sp$points3d(predict_pymnt$Year, predict_pymnt$GROSS_RENT_AMNT, predict_pymnt$ASSTN_PYMNT_AMNT, pch=16)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)