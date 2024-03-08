#Macro Paper

library(dplyr)
library(ggplot2)
library(stargazer)
library(lmtest)
library(sandwich)
library(plm)
library(readr)
library(rstatix)
library(readxl)


  ### Import and clean Data

rm(list = ls())
  Data = read_excel("/Users/leander/Desktop/SSE master/Macro/4330_FUND_DATA.xlsx", sheet = "Import")
#Clear Duplicates
  Data <- Data %>%
    distinct(ISIN, Year, .keep_all = TRUE)

  ## Construct Varibles:

#Lag Return
Data= Data%>% group_by(ISIN) %>% mutate(Lagreturn = lag(`Total Return`))
  
#Construct Performance Variable in T=0
  Data$Performance = Data$`Total Return`/Data$Lagreturn-1
  
#Construct Lagged Performance (for PastPerformance)
  Data= Data%>% group_by(ISIN) %>% mutate(PastPerformance = lag(`Performance`))
  
#Lag NAV
  Data= Data%>% group_by(ISIN) %>% mutate(LAGNAV = lag(`NAV`))
  
#Construct Fundflow: NAV/NAV(t-1) - Performance
  Data$FundFlow = Data$NAV /Data$LAGNAV - Data$Performance -1


  ###Outllier detection - Analysis of independent variable 
  
  summary(Data$PastPerformance)
  boxplot(Data$PastPerformance)
  ggplot(Data, aes(x = Performance, y = ..density..)) +
    geom_histogram(binwidth = 0.02,colour = "darkblue" ,fill = "blue", alpha = 0.75)

#Independent variable "Past Performance" seems to have no problem with outliers

  ###Outllier detection - Analysis of dependent variable 
  
  summary(Data$FundFlow)
  boxplot(Data$FundFlow)
  ggplot(Data, aes(x=FundFlow, y = ..density..)) + geom_histogram(binwidth = 0.02,colour = "orange" ,fill = "blue", alpha = 0.75)

#Dependent variable has strong outlier problem -> Further cleaning necessary:
lower_bound <- quantile(Data$FundFlow, 0.05, na.rm = TRUE)
lower_bound
upper_bound = quantile(Data$FundFlow, 0.95, na.rm = TRUE)
upper_bound

clean_data <- Data[Data$FundFlow >= lower_bound & Data$FundFlow <= upper_bound, ]

summary(clean_data$FundFlow)
boxplot(clean_data$FundFlow)
ggplot(clean_data, aes(x=FundFlow, y = ..density..)) + geom_histogram(binwidth = 0.02,colour = "orange" ,fill = "blue", alpha = 0.75)

#After removal of outliers the distribution looks better


### Model Calcultion 
Model1= lm(FundFlow ~ PastPerformance , data = clean_data)
  
  summary(Model1)
  
  stargazer(Model1, type = "text")
  
  ggplot(data = clean_data, aes(x = Performance, y = FundFlow))+ geom_point(shape=5)+ geom_smooth(method = "lm", se= FALSE, col = "green", linetype = "dashed")
                                       
#First model yields positive relationship at 90% significance level-
  

### NAV Analysis -> Different fundsizes: LARGE, MEDIUM, SMALL
summary(clean_data$NAV)
boxplot(clean_data$NAV) 

#Create a subset where NAV is larger than the 75th percentile -> LARGE FUNDS

percentile_75 <- quantile(clean_data$NAV, 0.75, na.rm = TRUE)
Big_Funds_Data <- subset(clean_data, NAV > percentile_75)

Model_LARGE_FUNDS= lm(FundFlow ~ PastPerformance , data = Big_Funds_Data)
summary(Model_LARGE_FUNDS)

#Subset for SMALL FUNDS
percentile_25 <- quantile(clean_data$NAV, 0.25, na.rm = TRUE)

Small_Funds_Data <- subset(clean_data, NAV < percentile_25)

Model_PETITE_FUNDS= lm(FundFlow ~ PastPerformance  , data = Small_Funds_Data)
summary(Model_PETITE_FUNDS)

#Subset for Medium 

Medium_Funds_Data <- subset(clean_data, NAV > percentile_25 & NAV < percentile_75)

summary(Medium_Funds_Data$NAV)

Model_MEDIUM_FUNDS= lm(FundFlow ~ PastPerformance  , data = Medium_Funds_Data)
summary(Model_MEDIUM_FUNDS)

stargazer(Model1,Model_PETITE_FUNDS, Model_MEDIUM_FUNDS,Model_LARGE_FUNDS, type = "text", 
          column.labels = c("All", "Small", "Medium", "Large"))

#Small and Medium sized Funds highly positive, Large funds no positive effect

### Robustness Tests: Include Fund_Tyoe as factor in Regressions: 

R1 = lm(FundFlow ~ PastPerformance + factor(`Fund Type`) , data = clean_data)
R2 = lm(FundFlow ~ PastPerformance + factor(`Fund Type`) , data = Small_Funds_Data)
R3 = lm(FundFlow ~ PastPerformance + factor(`Fund Type`) , data = Big_Funds_Data)
R4 = lm(FundFlow ~ PastPerformance + factor(`Fund Type`) , data = Medium_Funds_Data)

stargazer(R1,R2, R3,R4, title = "Robustness", type = "text", 
          column.labels = c("All", "Small", "Medium", "Large"))

#After COntrolling for Fund Type the effects holds also for Large funds



