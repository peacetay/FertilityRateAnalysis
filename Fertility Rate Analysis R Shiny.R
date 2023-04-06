library(sqldf)
library(DT)
library(lubridate)
library(shiny)
library(tidyverse)
library(shinydashboard)
library(readr)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(reshape2)
library(shinyWidgets)
library(plotly)
library(ggrepel)
library(radarchart)
library(fmsb)
library(bslib)
library(shinythemes)

# ----- Importing data
getwd()
data_dir <- file.path('Data')

# ----- Sources:
# https://population.un.org/wpp/Download/Standard/CSV/
# https://unstats.un.org/unsd/methodology/m49/overview/

WB <- read.csv(file.path(data_dir,"Combibed.csv"))
IMF <- read.csv(file.path(data_dir,"IMF.csv"))
# ------ Merging Data
dfken <- merge(WB,IMF,"Country.Code")
dfken <- dfken[,c(1,2,3,4,5,6,49,7:47)]
# ----- Renaming Columns and removing columns that are not needed
names(dfken) <- c("Country.Code","Indicator","Indcode","Country","Region","Income","IMF", as.character(1980:2020))
dfken$Indcode <- NULL
# ----- Melt years
dfken <- melt(dfken, c("Indicator","Country","Region","Income","IMF"), 7:(ncol(dfken)-1), variable_name = "Year", value.name = "Value")
dfken$Value <- as.numeric(dfken$Value)
# ----- Cast data frame
dfken <- dcast(dfken, Country+Region+Income+IMF+variable~Indicator, value.var = "Value")
names(dfken)[names(dfken) == 'variable'] <- 'Year'
# ----- Rename columns
names(dfken) <- c("Country","Region","Income","IMF","Year","Fertility","GDP","LifeExp","Mortrate","Population","Edu")
# ----- Define regions vector
regions <- sort(as.vector(unique(dfken$Region)))
regions <- append(regions, "All", 0)
# ----- Define countries vector
countries <- sort(as.vector(unique(dfken$Country)))
# ----- Remove NA
dfken <- dfken[complete.cases(dfken), ] 
dfken <- dfken[!(is.na(dfken) | dfken==""), ]

##Data for Emerging and Developing Economies
require(plyr)
df.emerg<-dfken
df.emerg<-filter(df.emerg,IMF=='Emerging and Developing Economies')
func1 <- function(df.emerg)
{
  return(data.frame(Life_Expectancy= abs(cor(df.emerg$LifeExp, df.emerg$Fertility))))
}
r_LifeExp<-ddply(df.emerg, .(Country), func1)
func2 <- function(df.emerg)
{
  return(data.frame(GDP = abs(cor(df.emerg$GDP, df.emerg$Fertility))))
}
r_GDP<-ddply(df.emerg, .(Country), func2)
func3 <- function(df.emerg)
{
  return(data.frame(Education = abs(cor(df.emerg$Edu, df.emerg$Fertility))))
}
r_Edu<-ddply(df.emerg, .(Country), func3)
r1<-merge(r_LifeExp,r_GDP,by="Country")
r2<-merge(r1,r_Edu,by="Country")
corr.emer<-na.omit(r2)



# ----- Define emerging countries vector

emer.countries <- sort(as.vector(unique(corr.emer$Country)))
rownames(corr.emer)<-corr.emer$Country
corr.emer<-corr.emer[,!names(corr.emer) %in% c("Country")]

##Data for Advanced Economies
require(plyr)
df.adv<-dfken
df.adv<-filter(df.adv,IMF=='Advanced Economies')
funct1 <- function(df.adv)
{
  return(data.frame(Life_Expectancy= abs(cor(df.adv$LifeExp, df.adv$Fertility))))
}
cr_LifeExp<-ddply(df.adv, .(Country), funct1)
funct2 <- function(df.adv)
{
  return(data.frame(GDP = abs(cor(df.adv$GDP, df.adv$Fertility))))
}
cr_GDP<-ddply(df.adv, .(Country), funct2)
funct3 <- function(df.adv)
{
  return(data.frame(Education = abs(cor(df.adv$Edu, df.adv$Fertility))))
}
cr_Edu<-ddply(df.adv, .(Country), funct3)
cr1<-merge(cr_LifeExp,cr_GDP,by="Country")
cr2<-merge(cr1,cr_Edu,by="Country")
corr.adv<-na.omit(cr2)

# ----- Define advanced countries vector

adv.countries <- sort(as.vector(unique(corr.adv$Country)))
rownames(corr.adv)<-corr.adv$Country
corr.adv<-corr.adv[,!names(corr.adv) %in% c("Country")]

life_exp<-read.csv(file.path(data_dir,"life_expV2.csv"))
fertility<-read.csv(file.path(data_dir,"fertilityV2.csv"))
GDP<-read.csv(file.path(data_dir,"GDPV2.csv"))
Edu <-read.csv(file.path(data_dir,"EduV2.csv"))
names(life_exp) <- gsub("X", "Year_", names(life_exp))
names(fertility) <- gsub("X", "Year_", names(fertility))
names(GDP) <- gsub("X", "Year_", names(GDP))
names(Edu) <- gsub("X", "Year_", names(Edu))

Pop_Age <- read.csv(file.path(data_dir,"Population by Age, Sex.csv"))
UNMethod <- read.csv(file.path(data_dir,"UNSD - Methodology.csv"))
# ----- Pop_Age: Renaming Columns and removing columns that are not needed
Pop_Age <- select(Pop_Age, -c(3,4,6,8,9))
names(Pop_Age) <- c("LocID", "Country", "Year", "AgeGroup", "PopMale", "PopFemale", "PopTotal")
# ----- Cleaning Pop_Age LocID
temp_vec <- Pop_Age$LocID
Pop_Age$LocID <- sprintf("%03d", temp_vec)
# ----- UNMethod: Renaming Columns and removing columns that are not needed
UNMethod <- select(UNMethod,-c(1,2,3,5,7,8,9,13, 14,15))
names(UNMethod) <- c("Region", "SubRegion", "LocID", "ISO_Alpha2", "ISO_Alpha3") 
# ----- Cleaning Data of UNMethod
UNMethod$LocID[UNMethod$ISO_Alpha2 == 535] <- 535
UNMethod$LocID[UNMethod$ISO_Alpha2 == 344] <- 344
UNMethod$LocID[UNMethod$ISO_Alpha2 == 446] <- 446

UNMethod$ISO_Alpha2[UNMethod$ISO_Alpha3 == 'BQ'] <- 'BQ'
UNMethod$ISO_Alpha2[UNMethod$ISO_Alpha3 == 'HK'] <- 'HK'
UNMethod$ISO_Alpha2[UNMethod$ISO_Alpha3 == 'MO'] <- 'MO'

UNMethod$ISO_Alpha3[UNMethod$temp == 'BES'] <- 'BES'
UNMethod$ISO_Alpha3[UNMethod$temp == 'HKG'] <- 'HKG'
UNMethod$ISO_Alpha3[UNMethod$temp == 'MAC'] <- 'MAC'
# ------ Merging UNMethod with Pop_Age
temp_df <- unique(UNMethod[,c("LocID", "ISO_Alpha2", "ISO_Alpha3", "Region", "SubRegion")])
Pop_Age <- merge(x = Pop_Age, y = UNMethod, by = "LocID")
# ------ Reordering Columns in Pop_Age
Pop_Age <- Pop_Age[,c("LocID", "ISO_Alpha2", "ISO_Alpha3", "Country", "Region", "SubRegion", "Year", "AgeGroup", "PopMale", "PopFemale", "PopTotal")]
# ------ Removing lines without LocID, ISO_Alpha2, ISO_Alpha3 from Pop_Age 
Pop_Age <- subset(Pop_Age, LocID != "" & ISO_Alpha2 != "" & ISO_Alpha3 != "")
# ----- Removing Aprostrophe from Country Names
Pop_Age$Country<-gsub("'","",as.character(Pop_Age$Country))
# ----- Age Group Vector
AgeGroup_vec <- c("0-4", "5-9", "10-14", "15-19", "20-24",
                  "25-29", "30-34", "35-39", "40-44", "45-49",
                  "50-54", "55-59", "60-64", "65-69", "70-74",
                  "75-79", "80-84", "85-89", "90-94", "95-99", "100+")

WB1 <- read.csv(file.path(data_dir,"Final project.csv"))
IMF1 <- read.csv(file.path(data_dir,"IMF.csv"))
# ------ Merging Data
df <- merge(WB1,IMF1,"Country.Code")
df <- df[,c(1,2,3,4,5,6,49,7:47)]
# ----- Renaming Columns and removing columns that are not needed
names(df) <- c("Country.Code","Indicator","Indcode","Country","Region","Income","IMF", as.character(1980:2020))
df$Indcode <- NULL
# ----- Melt years
df <- melt(df, c("Indicator","Country","Region","Income","IMF"), 7:(ncol(df)-1), variable_name = "Year", value.name = "Value")
df$Value <- as.numeric(df$Value)
# ----- Cast data frame
df <- dcast(df, Country+Region+Income+IMF+variable~Indicator, value.var = "Value")
names(df)[names(df) == 'variable'] <- 'Year'
# ----- Rename columns
names(df) <- c("Country","Region","Income","IMF","Year","Fertility","GDP","LifeExp","Mortrate","Population")
# ----- Define regions vector
regions <- sort(as.vector(unique(df$Region)))
regions <- append(regions, "All", 0)
# ----- Define countries vector
countries <- sort(as.vector(unique(df$Country)))
# ----- Remove NA
df <- df[complete.cases(df), ] 
df <- df[!(is.na(df) | df==""), ]
#Years Range
projected_years <- seq(2020,2050)
projected_years_length <- length(projected_years)

########################### EMERGING COUNTRIES ###########################################
#Emerging and Developing Economies
emerging <-cbind(df) %>%
  filter(IMF %in% c('Emerging and Developing Economies')) %>%
  group_by(Year) %>%
  summarize_at(vars(Fertility), list(name = mean)) 
colnames(emerging)[2] <- 'Mean'
#emerging$IMF <- 'Emerging and Developing Economies'
# to find length of projected years
emerging_year <-as.numeric(as.character(emerging$Year))
emerging_year_number <- as.numeric(emerging$Year)
len_emerge_actual_year <- length(emerging_year_number)

##################        LINEAR        ########################
fit_linear <- lm(emerging$Mean ~ emerging_year_number)
mod_summary_linear <- summary(fit_linear)
# search for intercept and gradient
emerging_intercept_linear <- mod_summary_linear$coefficients[1,1]
emerging_gradient_linear <- mod_summary_linear$coefficients[2,1]

# to get projected values
projected_emerge_mean_linear <- vector(mode = "numeric", length = projected_years_length)
for(i in (len_emerge_actual_year+1):(len_emerge_actual_year+projected_years_length)){
  projected_emerge_mean_linear[i-len_emerge_actual_year] <- (emerging_gradient_linear * i) + emerging_intercept_linear
}

#place projected values into dataframe
projected_emerging_df_linear <- data.frame(projected_years,projected_emerge_mean_linear)

#change to similar column name
names(projected_emerging_df_linear)[1] <- "Year"
names(projected_emerging_df_linear)[2] <- "Mean"

#change to character so as to be able to append to original df
projected_emerging_df_linear$Year <- as.character(projected_emerging_df_linear$Year)
#append to orginal file
final_emerging_df_linear <- rbind(emerging,projected_emerging_df_linear)
final_emerging_df_linear$IMF <- 'Emerging and Developing Economies'
##################        LINEAR        ########################
##################        EXPONENTIAL   ########################
fit_expo <- lm(log(emerging$Mean) ~ emerging_year_number)
mod_summary_expo <- summary(fit_expo)
# search for intercept and gradient
emerging_intercept_expo <- mod_summary_expo$coefficients[1,1]
emerging_gradient_expo <- mod_summary_expo$coefficients[2,1]

# to get projected values
projected_emerge_mean_expo <- vector(mode = "numeric", length = projected_years_length)
for(i in (len_emerge_actual_year+1):(len_emerge_actual_year+projected_years_length)){
  projected_emerge_mean_expo[i-len_emerge_actual_year] <- exp((emerging_gradient_expo * i) + emerging_intercept_expo)
}

#place projected values into dataframe
projected_emerging_df_expo <- data.frame(projected_years,projected_emerge_mean_expo)

#change to similar column name
names(projected_emerging_df_expo)[1] <- "Year"
names(projected_emerging_df_expo)[2] <- "Mean"

#change to character so as to be able to append to original df
projected_emerging_df_expo$Year <- as.character(projected_emerging_df_expo$Year)
#append to orginal file
final_emerging_df_expo <- rbind(emerging,projected_emerging_df_expo)
final_emerging_df_expo$IMF <- 'Emerging and Developing Economies'

### EXPONENTIAL####
### EMERGING COUNTRIES ###
#### ADVANCED COUNTRIES ###

advanced <-cbind(df) %>%
  filter(IMF %in% c('Advanced Economies')) %>%
  group_by(Year) %>%
  summarize_at(vars(Fertility), list(name = mean)) 
colnames(advanced)[2] <- "Mean"
#advanced$IMF <- 'Advanced Economies'
# to find length of projected years
advanced_year <-as.numeric(as.character(advanced$Year))
advanced_year_number <- as.numeric(advanced$Year)
len_advanced_actual_year <- length(advanced_year_number)

##################        LINEAR        ########################
fit2_linear <- lm(advanced$Mean ~ advanced_year_number)
mod_summary2_linear <- summary(fit2_linear)
# search for intercept and gradient
advanced_intercept_linear <- mod_summary2_linear$coefficients[1,1]
advanced_gradient_linear <- mod_summary2_linear$coefficients[2,1]

# to get projected values
projected_advanced_mean_linear <- vector(mode = "numeric", length = projected_years_length)
for(i in (len_advanced_actual_year+1):(len_advanced_actual_year+projected_years_length)){
  projected_advanced_mean_linear[i-len_advanced_actual_year] <- (advanced_gradient_linear * i) + advanced_intercept_linear
}

#place projected values into dataframe
projected_advanced_df_linear <- data.frame(projected_years,projected_advanced_mean_linear)

#change to similar column name
names(projected_advanced_df_linear)[1] <- "Year"
names(projected_advanced_df_linear)[2] <- "Mean"

#change to character so as to be able to append to original df
projected_advanced_df_linear$Year <- as.character(projected_advanced_df_linear$Year)
#append to orginal file
final_advanced_df_linear <- rbind(advanced,projected_advanced_df_linear)
final_advanced_df_linear$IMF <- 'Advanced Economies'
##################        LINEAR        ########################
##################        EXPONENTIAL   ########################
fit2_expo <- lm(log(advanced$Mean) ~ advanced_year_number)
mod_summary2_expo <- summary(fit2_expo)
# search for intercept and gradient
advanced_intercept_expo <- mod_summary2_expo$coefficients[1,1]
advanced_gradient_expo <- mod_summary2_expo$coefficients[2,1]

# to get projected values
projected_advanced_mean_expo <- vector(mode = "numeric", length = projected_years_length)
for(i in (len_advanced_actual_year+1):(len_advanced_actual_year+projected_years_length)){
  projected_advanced_mean_expo[i-len_advanced_actual_year] <- exp((advanced_gradient_expo * i) + advanced_intercept_expo)
}

#place projected values into dataframe
projected_advanced_df_expo <- data.frame(projected_years,projected_advanced_mean_expo)

#change to similar column name
names(projected_advanced_df_expo)[1] <- "Year"
names(projected_advanced_df_expo)[2] <- "Mean"

#change to character so as to be able to append to original df
projected_advanced_df_expo$Year <- as.character(projected_advanced_df_expo$Year)
#append to orginal file
final_advanced_df_expo <- rbind(advanced,projected_advanced_df_expo)
final_advanced_df_expo$IMF <- 'Advanced Economies'

##################        EXPONENTIAL   ########################
########################### ADVANCED COUNTRIES ###########################################


########################### SELECTED COUNTRIES ###########################################
#Selected Country


#create data frame with 0 rows and 3 columns
final_country_df2_linear <- data.frame(matrix(ncol = 3, nrow = 0))
final_country_df2_expo <- data.frame(matrix(ncol = 3, nrow = 0))
#provide column names
colnames(final_country_df2_linear) <- c('Year', 'Fertility', 'Country') 
colnames(final_country_df2_expo) <- c('Year', 'Fertility', 'Country')



for (c in countries) {
  input_country <- c
  countrytoplot <- cbind(df) %>%
    filter(Country %in% c(input_country))
  
  if (input_country != 'Aruba' && input_country != 'Hong Kong SAR, China' && input_country != 'Kosovo' && input_country != 'Macao SAR, China'
      && input_country != 'Nauru' && input_country != 'Puerto Rico'&& input_country != 'Tuvalu' ){
    
    #frpxvalue1 <- countrytoplot$Year
    #frpyvalue1 <- countrytoplot$Fertility
    
    countrytoplot2 <- data.frame(countrytoplot$Year,countrytoplot$Fertility)
    #countrytoplot2$Country <- input_country
    
    names(countrytoplot2)[1] <- "Year"
    names(countrytoplot2)[2] <- "Fertility"
    
    # if 1 row cant do projection
    if (nrow(na.omit(countrytoplot2)) != 1) {
      # to find length of projected years
      country_year <-as.numeric(as.character(countrytoplot2$Year))
      country_year_number <- as.numeric(countrytoplot2$Year)
      #len_country_actual_year <- length(country_year_number)
      # To counter missing data in between
      len_country_actual_year <- 2019- as.numeric(as.character(countrytoplot2[1,1])) +1
      
      ##################        LINEAR        ########################
      fit3_linear <- lm(countrytoplot2$Fertility ~ country_year_number)
      mod_summary3_linear <- summary(fit3_linear)
      # search for intercept and gradient
      country_intercept_linear <- mod_summary3_linear$coefficients[1,1]
      country_gradient_linear <- mod_summary3_linear$coefficients[2,1]
      
      # to get projected values
      projected_country_mean_linear <- vector(mode = "numeric", length = projected_years_length)
      for(i in (len_country_actual_year+1):(len_country_actual_year+projected_years_length)){
        projected_country_mean_linear[i-len_country_actual_year] <- (country_gradient_linear * i) + country_intercept_linear
      }
      
      
      #place projected values into dataframe
      projected_country_df_linear <- data.frame(projected_years,projected_country_mean_linear)
      #projected_country_df$Country <- input_country
      
      #change to similar column name
      names(projected_country_df_linear)[1] <- "Year"
      names(projected_country_df_linear)[2] <- "Fertility"
      
      #change to character so as to be able to append to original df
      projected_country_df_linear$Year <- as.character(projected_country_df_linear$Year)
      #append to orginal file
      final_country_df_linear <- rbind(countrytoplot2,projected_country_df_linear)
      final_country_df_linear$Country <- input_country
      final_country_df2_linear <-rbind(final_country_df2_linear,final_country_df_linear)
      ##################        LINEAR        ########################
      ##################        EXPONENTIAL   ########################
      fit3_expo <- lm(log(countrytoplot2$Fertility) ~ country_year_number)
      mod_summary3_expo <- summary(fit3_expo)
      # search for intercept and gradient
      country_intercept_expo <- mod_summary3_expo$coefficients[1,1]
      country_gradient_expo <- mod_summary3_expo$coefficients[2,1]
      
      # to get projected values
      projected_country_mean_expo <- vector(mode = "numeric", length = projected_years_length)
      for(i in (len_country_actual_year+1):(len_country_actual_year+projected_years_length)){
        projected_country_mean_expo[i-len_country_actual_year] <- exp((country_gradient_expo * i) + country_intercept_expo)
      }
      
      
      #place projected values into dataframe
      projected_country_df_expo <- data.frame(projected_years,projected_country_mean_expo)
      #projected_country_df$Country <- input_country
      
      #change to similar column name
      names(projected_country_df_expo)[1] <- "Year"
      names(projected_country_df_expo)[2] <- "Fertility"
      
      #change to character so as to be able to append to original df
      projected_country_df_expo$Year <- as.character(projected_country_df_expo$Year)
      #append to orginal file
      final_country_df_expo <- rbind(countrytoplot2,projected_country_df_expo)
      final_country_df_expo$Country <- input_country
      final_country_df2_expo <-rbind(final_country_df2_expo,final_country_df_expo)
      ##################        EXPONENTIAL   ########################
    }
    
    
  }
}


# ----- Shiny UI

ui <- navbarPage("Population Study",
                 theme = "https://bootswatch.com/3/united/bootstrap.min.css",

                 tabPanel("World Map",
                          sidebarLayout(
                            sidebarPanel( width = 3,
                                          h3("Select Inputs"),
                                          helpText("Select a year to visualise the data"),
                                          h3("\n"),
                                          #tags$hr(),
                                          selectInput('year', 'Pick a Year', c(1980:2019), selected=2019),
                                          #tags$hr(),
                                          tags$b("World Average Fertility Rate"),
                                          verbatimTextOutput("txt_f"),
                                          #tags$hr(),
                                          tags$b("World Average Life Expectancy"),
                                          verbatimTextOutput("txt_lf"),
                                          #tags$hr(),
                                          tags$b("World Average GDP per capita (USD)"),
                                          verbatimTextOutput("txt_gdp"),
                                          #tags$hr(),
                                          tags$b("World Average Secondary School Enrollment"),
                                          verbatimTextOutput("txt_edu"),
                                          #tags$hr(),
                                          helpText("Source: http://data.worldbank.org.
                                        Note that some values for some years are missing.")
                            ),
                            
                            
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Fertility", br(), plotlyOutput("plot_f")),
                                          tabPanel("GDP per capita", br(), plotlyOutput("plot_gdp")),
                                          tabPanel("Secondary School Enrollment ", br(), plotlyOutput("plot_edu")),
                                          tabPanel("Life Expectancy ", br(), plotlyOutput("plot_lf"))
                                          
                                          
                              )
                              
                              
                              
                            )
                          )
                 ),
                 
                 tabPanel("Fertility Rate Projections",
                          
                          sidebarLayout(
                            
                            sidebarPanel(width = 3,
                                         
                                         h3("Select Inputs"),
                                         helpText("Please select Countries"),
                                         h3("\n"),
                                         selectInput("variable_frp1", "Country:",choices = unique(df$Country)),
                                         sliderInput("frpyear", "Year:", min = 1980, max = 2050, value = c(1980,2050), sep = "", width = '100%')
                            ),
                            
                            mainPanel(
                              
                              fluidPage(
                                
                                fluidRow(
                                  
                                  tabsetPanel(
                                    
                                    tabPanel("Overview",
                                             
                                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("fertilitypto_plot1"), plotOutput("fertilitypto_plot2"))
                                             
                                    )
                                    
                                  )
                                  
                                )
                              )
                              
                            )
                            
                          )
                          
                 ),
                 
                 
                 tabPanel("Population Projections",
                          
                          sidebarLayout(
                            
                            sidebarPanel(width = 3,
                                         
                                         h3("Select Inputs"),
                                         helpText("Please select Region, Sub Region and Country"),
                                         h3("\n"),
                                         uiOutput("Bar_Region"),
                                         uiOutput("Bar_SubRegion"),
                                         uiOutput("Bar_Country"),
                                         
                                         sliderInput(inputId = "Bar_Year",
                                                     label = "Select Year",
                                                     min = 1980,
                                                     max = 2050,
                                                     value = min(Pop_Age$Year),
                                                     step = 1,
                                                     width = "100%",
                                                     animate = animationOptions(interval = 200)),
                                         
                                         helpText("Source: https://population.un.org/ ")
                            ),
                            
                            mainPanel(
                              
                              fluidPage(
                                
                                fluidRow(
                                  
                                  tabsetPanel(
                                    tabPanel("Bar Chart",
                                             
                                             box(width = 12, 
                                                 h4("Bar Chart"),
                                                 plotOutput(outputId = "barplot"))
                                             
                                    ),
                                    
                                    tabPanel("Table",
                                             
                                             box(width = 12, 
                                                 h4("Table"),
                                                 DT::dataTableOutput("Pop_Age_subset")
                                             )
                                             
                                             
                                    )
                                  )
                                )
                                
                              )
                              
                            )
                            
                          )
                          
                 ),
                 
                 
                 tabPanel("Development Indicators vs Fertility Rate",
                          
                          sidebarLayout(
                            
                            sidebarPanel(width = 3,
                                         
                                         h3("Select Inputs"),
                                         helpText("Please select Developed or Developing Economies"),
                                         h3("\n"),
                                         selectInput("IMF1", label = ("Select"), 
                                                     choices = list("All" = "All", "Emerging and Developing Economies" = "Emerging and Developing Economies", "Advanced Economies" = "Advanced Economies"), selected = 1),
                                         
                                         sliderInput("yearken", label = "Select Year",
                                                     min = 1980, 
                                                     max = 2018, 
                                                     value = 1980, 
                                                     sep = "", 
                                                     width = "100%",
                                                     animate = animationOptions(interval = 750))             
                            ),
                            
                            mainPanel(
                              
                              fluidPage(
                                
                                fluidRow(
                                  
                                  tabsetPanel(
                                    
                                    tabPanel("GDP Per Capita",
                                             
                                             box(width = 12,
                                                 h4("GDP per capita vs Fertility Rate"),
                                                 plotOutput(outputId = "GDP_plot", hover = hoverOpts("plot_hover", delay = 100 , delayType = "debounce")),
                                                 uiOutput("hover_info"))
                                    ),
                                    
                                    tabPanel("Secondary School Enrollment",
                                             
                                             box(width = 12,
                                                 h4("Secondary School Enrollment (Gross %) vs Fertility Rate"),
                                                 plotOutput(outputId = "Edu_plot", hover = hoverOpts("plot_hover4", delay = 100 , delayType = "debounce")),
                                                 uiOutput("hover_info4"))
                                    ),
                                    
                                    tabPanel("Life Expectancy",
                                             
                                             box(width = 12, 
                                                 h4("Life Expectancy vs Fertility Rate"),
                                                 plotOutput(outputId = "LE_plot", hover = hoverOpts("plot_hover2", delay = 100 , delayType = "debounce")),
                                                 uiOutput("hover_info2"))
                                    ),
                                    
                                    tabPanel("Mortality Rate",
                                             
                                             box(width = 12,
                                                 h4("Mortality Rate vs Fertility Rate"),
                                                 plotOutput(outputId = "MR_plot", hover = hoverOpts("plot_hover3", delay = 100 , delayType = "debounce")),
                                                 uiOutput("hover_info3"))
                                    )
                                    
                                  )
                                )
                                
                              )
                              
                            )
                            
                          )
                          
                 ),
                 

                 tabPanel("Correlation",
                          
                          sidebarLayout(
                            
                            sidebarPanel(width = 3,
                                         
                                         h3("Select Inputs"),
                                         helpText("Please select Advanced Countries"),
                                         h3("\n"),
                                         selectInput("var_adv", "Advanced Country:",choices = adv.countries),
                                         helpText("Please select Developing Countries"),
                                         h3("\n"),
                                         selectInput("var_emer", "Emerging & Developing Country:",choices = emer.countries),
                            ),
                            
                            mainPanel(
                              fluidPage(
                                fluidRow(
                                  tabsetPanel(
                                    tabPanel("Absolute Correlation with Fertility Rate",
                                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("Radar"), plotOutput("Radar1"))
                                             
                                    )
                                    
                                  )
                                )
                              )
                              
                            )
                            
                          )
                          
                 )

)

server <- function(input, output) {
  
  
  data <- reactive({
    
    file = Pop_Age
    
  })
  
  # Creating filters
  output$Bar_Region <- renderUI({
    
    selectInput(inputId = "Bar_Region", "Select Region", choices = var_Bar_Region(), selected = "Asia")
    
  })
  
  output$Bar_SubRegion <- renderUI({
    
    selectInput(inputId = "Bar_SubRegion", "Select Sub-Region", choices = var_Bar_SubRegion(), selected = "Southern Asia")
    
  })
  
  output$Bar_Country <- renderUI({
    
    selectInput(inputId = "Bar_Country", "Select Country", choices = var_Bar_Country(), selected = "Afghanistan")
    
  })
  
  # Cascasing filters
  
  var_Bar_Region <- reactive({
    file1 <- data()
    if(is.null(data())){return()}
    as.list(unique(file1$Region))
  })
  
  # Creating reactive function to subset data
  Bar_Region_function <- reactive({
    file1 <- data()
    Region <- input$Bar_Region
    file2 <- sqldf(sprintf("select * from file1 where Region = '%s' ", Region))
    return (file2)
    
  })
  
  var_Bar_SubRegion <- reactive({
    file1 <- Bar_Region_function()
    if(is.null(file1)){return()}
    as.list(unique(file1$SubRegion))
    
  })
  
  Bar_Country_function <- reactive({
    file1 <- Bar_Region_function()
    SubRegion <- input$Bar_SubRegion
    file2 <- sqldf(sprintf("select * from file1 where SubRegion = '%s' ", SubRegion))
    return (file2)
    
  })
  
  var_Bar_Country <- reactive({
    file1 <- Bar_Country_function()
    as.list(unique(file1$Country))
    
  })
  
  
  data_table <- reactive ({
    
    file1 <- Bar_Country_function()
    
    Country <- input$Bar_Country
    file2 <- sqldf(sprintf("select * from file1 where Country = '%s' ", Country))
    file2 <- file2 %>% filter(Year == input$Bar_Year)
    
    file2$AgeGroup <- factor(file2$AgeGroup, levels = AgeGroup_vec)
    
    file2
    
  })
  
  
  output$Pop_Age_subset <- DT::renderDataTable({
    
    
    DT::datatable(data_table(), 
                  options = list(order = list(list(8, 'asc')), 
                                 scrollX = T, 
                                 searching = FALSE,
                                 pageLength = 21,
                                 lengthChange = FALSE,
                                 paging = FALSE))
  })
  
  
  output$barplot <- renderPlot({
    
    plot.title = input$Bar_Year
    
    ggplot(data_table()) + 
      geom_bar(aes(x = -PopMale, y = AgeGroup), stat = 'identity', fill = '#8ECEFD') +
      geom_bar(aes(x = PopFemale, y = AgeGroup), stat = 'identity', fill = '#F88B9D') + 
      geom_vline(aes(xintercept = 0), show.legend = TRUE) +
      scale_x_continuous(labels=abs) +
      labs(subtitle = plot.title, x = "Population Size of Males | Females ('000)", y = 'Age Group') + 
      theme(axis.title = element_text(face = 'bold'))
    
  })
  
  
  df_subset <- reactive({
    
    sub_df <- subset(dfken, Year == input$yearken, drop=T)
    if (input$IMF1 == "All"){sub_df <- sub_df %>% filter(IMF=="Emerging and Developing Economies" | IMF== "Advanced Economies")} 
    else {sub_df <- sub_df %>% filter(IMF==input$IMF1)}
    
  })
  
  
  output$GDP_plot <- renderPlot({
    
    ggplot(df_subset(), aes(x = Fertility, y = GDP, label = Country,)) +
      geom_jitter(aes(color = Region, shape = IMF, size = Population, alpha = 0.7))+
      scale_x_continuous(breaks = c(1,2,3,4,5,6,7), limits = c(0,8)) +
      scale_y_continuous(breaks = c(10000,20000,30000,40000,50000,60000,70000,80000,90000), limits = c(0,100000)) +
      scale_size(range = c(2, 15)) + 
      guides(alpha = "none", size = "none") +
      labs(y = "GDP Per Capita ($)", x = "Fertility Rate")+
      theme( 
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
      )
    
  })
  
  
  output$LE_plot <- renderPlot({
    
    ggplot(df_subset(), aes(x = Fertility, y = LifeExp))+
      geom_jitter(aes(color = Region, shape = IMF, size = Population, alpha = 0.7))+
      scale_x_continuous(breaks = c(1,2,3,4,5,6,7), limits = c(0,8)) +
      scale_y_continuous(breaks = c(40,50,60,70,80,90,100), limits = c(35,120)) +
      scale_size(range = c(2, 15)) + 
      guides(alpha = "none", size = "none") +
      labs(y = "Life Expectancy (Years)", x = "Fertility Rate")+
      theme( 
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))
    
  })
  
  output$MR_plot <- renderPlot({
    
    ggplot(df_subset(), aes(x = Fertility, y = Mortrate))+
      geom_jitter(aes(color = Region, shape = IMF, size = Population, alpha = 0.7))+
      scale_x_continuous(breaks = c(1,2,3,4,5,6,7), limits = c(0,8)) +
      scale_y_continuous(breaks = c(0,50,100,150,200,250,300), limits = c(0,300)) +
      scale_size(range = c(2, 15)) + 
      guides(alpha = "none", size = "none") +
      labs(y = "Mortality Rate, Infant (per 1000 births)", x = "Fertility Rate")+
      theme( 
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))
    
  })
  
  output$Edu_plot <- renderPlot({
    
    ggplot(df_subset(), aes(x = Fertility, y = Edu))+
      geom_jitter(aes(color = Region, shape = IMF, size = Population, alpha = 0.7))+
      scale_x_continuous(breaks = c(1,2,3,4,5,6,7), limits = c(0,8)) +
      scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100,110), limits = c(0,120)) +
      scale_size(range = c(2, 15)) + 
      guides(alpha = "none", size = "none") +
      labs(y = "Secondary School Enrollment (Gross %)", x = "Fertility Rate")+
      theme( 
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))
    
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(df_subset(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- -250
    top_pct <- 300
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- -250
    top_px <- 300
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$Country, "<br/>",
                    "<b> Region: </b>", point$Region, "<br/>",
                    "<b> Population: </b>", point$Population, "<br/>"
      )))
    )
  })
  
  output$hover_info2 <- renderUI({
    hover <- input$plot_hover2
    point <- nearPoints(df_subset(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- -250
    top_pct <- 300
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- -250
    top_px <- 300
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$Country, "<br/>",
                    "<b> Region: </b>", point$Region, "<br/>",
                    "<b> Population: </b>", point$Population, "<br/>"
      )))
    )
  })
  
  output$hover_info3 <- renderUI({
    hover <- input$plot_hover3
    point <- nearPoints(df_subset(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- -250
    top_pct <- 300
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- -250
    top_px <- 300
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$Country, "<br/>",
                    "<b> Region: </b>", point$Region, "<br/>",
                    "<b> Population: </b>", point$Population, "<br/>"
      )))
    )
  })
  
  output$hover_info4 <- renderUI({
    hover <- input$plot_hover4
    point <- nearPoints(df_subset(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- -250
    top_pct <- 300
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- -250
    top_px <- 300
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$Country, "<br/>",
                    "<b> Region: </b>", point$Region, "<br/>",
                    "<b> Population: </b>", point$Population, "<br/>"
      )))
    )
  })
  

  df_subset_emerge <- reactive({
    YearsRangeFRP <- seq(input$frpyear[1], input$frpyear[2])
    sub_df <- subset(final_emerging_df_linear, Year %in% YearsRangeFRP)
    
  })
  
  df_subset_advanced <- reactive({
    YearsRangeFRP <- seq(input$frpyear[1], input$frpyear[2])
    sub_df <- subset(final_advanced_df_linear, Year %in% YearsRangeFRP)
    
    
    
  })
  
  df_subset_country <- reactive({
    YearsRangeFRP <- seq(input$frpyear[1], input$frpyear[2])
    input_country <- input$variable_frp1
    sub_df <- subset(final_country_df2_linear, Year %in% YearsRangeFRP) %>%
      filter(Country %in% c(input_country))
    
  })
  
  df_subset_emerge_expo <- reactive({
    YearsRangeFRP <- seq(input$frpyear[1], input$frpyear[2])
    sub_df <- subset(final_emerging_df_expo, Year %in% YearsRangeFRP)
    
  })
  
  df_subset_advanced_expo <- reactive({
    YearsRangeFRP <- seq(input$frpyear[1], input$frpyear[2])
    sub_df <- subset(final_advanced_df_expo, Year %in% YearsRangeFRP)
    
    
  })
  
  df_subset_country_expo <- reactive({
    YearsRangeFRP <- seq(input$frpyear[1], input$frpyear[2])
    input_country <- input$variable_frp1
    sub_df <- subset(final_country_df2_expo, Year %in% YearsRangeFRP) %>%
      filter(Country %in% c(input_country))
    
  })
  #n <- ifelse(length(YearsRangeFRP) ==1,1,0)
  
  #renderPlot functions inside
  output$fertilitypto_plot2 <- renderPlot({
    
    #Plot
    ggplot() +
      geom_line(data = df_subset_country_expo(), aes(x=Year, y = Fertility, group = Country, color = Country) , size = 1.5, linetype = "dashed") +
      geom_line(data = df_subset_emerge_expo(), aes(x=Year, y = Mean, group = IMF, color = IMF ),size = 1.5) +
      geom_line(data = df_subset_advanced_expo(), aes(x=Year, y = Mean, group = IMF, color = IMF ),size = 1.5) +
      #geom_point() +
      #scale_color_manual(values = c(Country="green", IMF="blue", IMF2="red")) +
      labs(color="Economy", title = "Projection using Exponential Regression") +
      xlab('Year') +
      ylab('Fertility Rate')+
      scale_x_discrete(breaks = seq(1980,2050, by = 10))+
      scale_y_continuous(breaks = c(0.5,1,2,3,4,5,6,7,8,9), limits = c(0,9))+
      theme(legend.position = c(0.75, 0.85))
    
    
    
  })
  
  #renderPlot functions inside
  output$fertilitypto_plot1 <- renderPlot({
    
    #Plot
    ggplot() +
      geom_line(data = df_subset_country(), aes(x=Year, y = Fertility, group = Country, color = Country) ,size = 1.5, linetype = "dashed") +
      geom_line(data = df_subset_emerge(), aes(x=Year, y = Mean, group = IMF, color = IMF ),size = 1.5) +
      geom_line(data = df_subset_advanced(), aes(x=Year, y = Mean, group = IMF, color = IMF ),size = 1.5) +
      #geom_point() +
      #scale_color_manual(values = c(Country="green", IMF="blue", IMF2="red")) +
      labs(color="Economy", title = "Projection using Linear Regression") +
      xlab('Year') +
      ylab('Fertility Rate')+
      scale_x_discrete(breaks = seq(1980,2050, by = 10))+
      scale_y_continuous(breaks = c(0.5,1,2,3,4,5,6,7,8,9), limits = c(0,9))+
      theme(legend.position = "None")
    
    
    
  })

  
  output$plot_f <- renderPlotly({
    
    YEAR<-paste("Year_", input$year, sep="")
    df<-data.frame(life_exp[1:4], life_exp[YEAR], fertility[YEAR], GDP[YEAR], Edu[YEAR])
    colnames(df) <- c("Code", "Region", "Income", "Country_Name","life_exp","fertility", "GDP","Edu" )
    
    l <- list(color = toRGB("grey"), width = 0.5)
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(df) %>%
      add_trace(
        z = ~fertility, color = ~fertility, colors = 'Blues',
        text = ~Country_Name, locations = ~Code, marker = list(line = l)) %>%
      colorbar(title = 'Fertility rate')%>%
      layout(title = paste('Fertility rate in',input$year))%>%
      
      layout(geo=g)
  })
  
  
  output$plot_lf <- renderPlotly({
    
    YEAR<-paste("Year_", input$year, sep="")
    df<-data.frame(life_exp[1:4], life_exp[YEAR], fertility[YEAR], GDP[YEAR],Edu[YEAR])
    colnames(df) <- c("Code", "Region", "Income", "Country_Name","life_exp","fertility", "GDP","Edu" )
    
    l <- list(color = toRGB("grey"), width = 0.5)
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(df) %>%
      add_trace(
        z = ~life_exp, color = ~life_exp, colors = 'Greens',
        text = ~Country_Name, locations = ~Code, marker = list(line = l)) %>%
      colorbar(title = 'Years')%>%
      layout(title = paste('Life Expectancy in',input$year))%>%
      layout(geo=g)
  })
  
  
  output$plot_gdp <- renderPlotly({
    
    YEAR<-paste("Year_", input$year, sep="")
    df<-data.frame(life_exp[1:4], life_exp[YEAR], fertility[YEAR], GDP[YEAR],Edu[YEAR])
    colnames(df) <- c("Code", "Region", "Income", "Country_Name","life_exp","fertility", "GDP","Edu" )
    
    l <- list(color = toRGB("grey"), width = 0.5)
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(df) %>%
      add_trace(
        z = ~GDP, color = ~GDP, colors = 'Reds',
        text = ~Country_Name, locations = ~Code, marker = list(line = l)) %>%
      colorbar(title = 'GDP (USD)')%>%
      layout(title = paste('GDP per Capita in',input$year))%>%
      layout(geo=g)
  })
  
  output$plot_edu <- renderPlotly({
    
    YEAR<-paste("Year_", input$year, sep="")
    df<-data.frame(life_exp[1:4], life_exp[YEAR], fertility[YEAR], GDP[YEAR],Edu[YEAR])
    colnames(df) <- c("Code", "Region", "Income", "Country_Name","life_exp","fertility", "GDP","Edu" )
    
    l <- list(color = toRGB("grey"), width = 0.5)
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(df) %>%
      add_trace(
        z = ~Edu, color = ~Edu, colors = 'Purples',
        text = ~Country_Name, locations = ~Code, marker = list(line = l)) %>%
      colorbar(title = 'School Enrollment')%>%
      layout(title = paste('% School Enrollment in',input$year))%>%
      layout(geo=g)
  })
  
  
  output$txtout <- renderText({
    
    YEAR<-paste("YEAR_", input$year, sep="")
    paste(YEAR)
  })
  
  
  output$txt_f <- renderText({
    a<-as.character(input$year)
    YEAR<-paste("Year_", a, sep="")
    df<-data.frame(life_exp[1:4], life_exp[YEAR], fertility[YEAR], GDP[YEAR],Edu[YEAR])
    colnames(df) <- c("Code", "Region", "Income", "Country_Name","life_exp","fertility", "GDP","Edu" )
    
    paste(signif(mean(df$fertility, na.rm=TRUE, digits=3)))
  })
  
  
  
  output$txt_lf <- renderText({
    a<-as.character(input$year)
    YEAR<-paste("Year_", a, sep="")
    df<-data.frame(life_exp[1:4], life_exp[YEAR], fertility[YEAR], GDP[YEAR],Edu[YEAR])
    colnames(df) <- c("Code", "Region", "Income", "Country_Name","life_exp","fertility", "GDP","Edu")
    
    paste( signif(mean(df$life_exp, na.rm=TRUE, digits=3)))
  })
  
  
  
  output$txt_gdp <- renderText({
    a<-as.character(input$year)
    YEAR<-paste("Year_", a, sep="")
    df<-data.frame(life_exp[1:4], life_exp[YEAR], fertility[YEAR], GDP[YEAR],Edu[YEAR])
    colnames(df) <- c("Code", "Region", "Income", "Country_Name","life_exp","fertility", "GDP","Edu" )
    
    
    paste("$", signif(mean(df$GDP, na.rm=TRUE, digits=3)))
  })
  
  output$txt_edu <- renderText({
    a<-as.character(input$year)
    YEAR<-paste("Year_", a, sep="")
    df<-data.frame(life_exp[1:4], life_exp[YEAR], fertility[YEAR], GDP[YEAR],Edu[YEAR])
    colnames(df) <- c("Code", "Region", "Income", "Country_Name","life_exp","fertility", "GDP","Edu" )
    
    
    paste("%", signif(mean(df$Edu, na.rm=TRUE, digits=3)))
  })
  
  df_subset_country_olivia <- reactive({
    input_adv <- input$var_adv
    input_emer <- input$var_emer
    
  })
  
  #renderPlot functions inside
  output$Radar<- renderPlot({
    
    
    #Plot
    corr.adv<-rbind(rep(0,10),rep(1,10),corr.adv)
    plotdata<-corr.adv[c("2","1",input$var_adv),]
    rownames(plotdata)<-plotdata$IMF
    plotdata<-plotdata[,!names(plotdata) %in% c("IMF")]
    radarchart(plotdata,axistype=1,title=input$var_adv,pfcol=rgb(0.2,0.8,0.8,0.6))
    
  })
  
  output$Radar1<- renderPlot({
    
    
    #Plot
    corr.emer<-rbind(rep(0,10),rep(1,10),corr.emer)
    plotdata<-corr.emer[c("2","1",input$var_emer),]
    rownames(plotdata)<-plotdata$IMF
    plotdata<-plotdata[,!names(plotdata) %in% c("IMF")]
    radarchart(plotdata,axistype=1,title=input$var_emer,pfcol=rgb(0.9,0.2,0.2,0.4))
    
  })
  
}

shinyApp(ui, server)
