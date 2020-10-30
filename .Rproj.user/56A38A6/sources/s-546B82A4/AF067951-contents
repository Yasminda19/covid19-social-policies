#install.packages("pacman")
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(fpc)
library(arules)
library(mice)
library(arulesCBA)

policy <- read.csv("C:/Users/Ayana/Desktop/TA_Daming/Social interventions pada 5 negara - Sheet1.csv")
datacovid <- read.csv("C:/Users/Ayana/Desktop/TA_Daming/owid-covid-data (1).csv")
datacovid$date <- as.Date(datacovid$date)

str(datacovid)
str(policy)
summary(datacovid)
summary(policy)


# -------------------------- praproses dan transformasi ----------------------------------------#

#praproses menjadi 5 negara
#ada 5 negara indonesia,germany,spain,italy,usa

datacovid <- datacovid[with(datacovid,which(location %in% c("Indonesia","Italy","Germany","United States","Spain"))),]
nrow(datacovid)

datacovid$tests_units <- NULL
datacovid$new_tests <- NULL
datacovid$new_tests_per_thousand <- NULL
datacovid$tests_units <- NULL
datacovid$total_tests <- NULL
datacovid$total_tests_per_thousand <- NULL
datacovid$iso_code <- NULL


md.pattern(datacovid)

#german first case is 2020-01-28
datagerman <- datacovid[with(datacovid,which(location %in% c("Germany"))),]
datagerman$location <- NULL
datagerman <- datagerman[which(dataindo$date >= as.Date("2020-01-28")),]

#plotting german cases
ggplot(datagerman, aes(x=date, y=total_cases_per_million)) +
  geom_line() + 
  xlab("")

#indonesia first case is 2020-03-2
dataindo <- datacovid[with(datacovid,which(location %in% c("Indonesia"))),]
dataindo$location <- NULL
dataindo <- dataindo[which(dataindo$date >= as.Date("2020-03-02")),]

#plotting indonesia cases
ggplot(dataindo, aes(x=date, y=total_cases_per_million)) +
  geom_line() + 
  xlab("")

#US first case is 2020-01-21
dataus <- datacovid[with(datacovid,which(location %in% c("United States"))),]
dataus$location <- NULL
dataus <- dataus[which(dataus$date >= as.Date("2020-01-21")),]

#plotting us cases
ggplot(dataindo, aes(x=date, y=total_cases_per_million)) +
  geom_line() + 
  xlab("")

#italy first case is 2020-01-31
datait <- datacovid[with(datacovid,which(location %in% c("Italy"))),]
datait$location <- NULL
datait <- datait[which(datait$date >= as.Date("2020-01-31")),]

#plotting italy cases
ggplot(dataindo, aes(x=date, y=total_cases_per_million)) +
  geom_line() + 
  xlab("")

#spain first case is 2020-02-01
dataspain <- datacovid[with(datacovid,which(location %in% c("Spain"))),]
dataspain$location <- NULL
dataspain <- dataspain[which(dataspain$date >= as.Date("2020-02-01")),]

#plotting spain cases
ggplot(dataindo, aes(x=date, y=total_cases_per_million)) +
  geom_line() + 
  xlab("")


# -------------------------- PROSES KATEGORI ----------------------------------------#

increasing_count <- function(df){
  cname <- c("total_cases_inc", "new_cases_inc",
             "total_deaths_inc", "new_deaths_inc",
             "total_cases_per_million_inc", "new_cases_per_million_inc",
             "total_deaths_per_million_inc", "new_deaths_per_million_inc")
  inc_data <- df
  for(i in 2:9){
    for(j in 2:nrow(df)){
      inc_data[j,i] <- df[j,i] - df[j-1, i]
      
    }
  }
}

categorize <- function(df){
  df$new_cases_cat <- "naik"
  df$new_deaths_cat <- "naik"
  df$new_cases_per_million_cat<-"naik"
  df$new_deaths_per_million_cat<-"naik"
  k = 1
  for(i in c(3,5,7,9)){
    if(df[1,i] <= 0){
      df[1,9+i%/%2] <- "tidak naik"
    }
  }
  for(i in 3:9){
    for(j in 2:nrow(df)){
      if(i%%2 == 1){
        print(i)
        if(df[j,i] > df[j-1,i]){
          df[j, 9+(i%/%2)] <- "naik"
        }
        else{
          df[j, 9+(i%/%2)] <- "tidak naik"
        }
      }
    }
  }
  print(k)
  View(df)
  return(df)
}

#categorize everything
dataindocat <- categorize(dataindo)
datagermancat <- categorize(datagerman)
dataitcat <- categorize(datait)
datauscat <- categorize(dataus)
dataspaincat <- categorize(dataspain)

# -------------------------- Transformasi Data ----------------------------------------#

#memetakan tanggal implementasi tanggal social interventions
#Data Indonesia
dataindocat$mass_gathering_implemented <- 1
dataindocat$education_closed <- 1
dataindocat$education_closed[1:5] <- 0
dataindocat$stay_home <- 1
dataindocat$stay_home[1:26] <- 0
dataindocat$non_essential_closed <- 1
dataindocat$non_essential_closed[1:26] <- 0
dataindocat$closing_business <- 1
dataindocat$closing_business[1:26] <- 0
dataindocat$limiting_travel <- 1
dataindocat$limiting_travel[1:26] <- 0

#Data Germany
datagermancat$mass_gathering_implemented <- 1
datagermancat$mass_gathering_implemented[1:70] <- 0
datagermancat$education_closed <- 1
datagermancat$education_closed[1:66] <- 0
datagermancat$stay_home <- 1
datagermancat$stay_home[1:66] <- 0
datagermancat$non_essential_closed <- 1
datagermancat$non_essential_closed[1:76] <- 0
datagermancat$closing_business <- 1
datagermancat$closing_business[1:82] <- 0
datagermancat$limiting_travel <- 1
datagermancat$limiting_travel[1:75] <- 0

#Data Italy
dataitcat$mass_gathering_implemented <- 1
dataitcat$mass_gathering_implemented[1:21] <- 0
dataitcat$education_closed <- 1
dataitcat$education_closed[1:21] <- 0
dataitcat$stay_home <- 1
dataitcat$stay_home[1:38] <- 0
dataitcat$non_essential_closed <- 1
dataitcat$non_essential_closed[1:38] <- 0
dataitcat$closing_business <- 1
dataitcat$closing_business[1:38] <- 0
dataitcat$limiting_travel <- 0

#Data Spain
dataspaincat$mass_gathering_implemented <- 1
dataspaincat$mass_gathering_implemented[1:43] <- 0
dataspaincat$education_closed <- 1
dataspaincat$education_closed[1:37] <- 0
dataspaincat$stay_home <- 1
dataspaincat$stay_home[1:43] <- 0
dataspaincat$non_essential_closed <- 1
dataspaincat$non_essential_closed[1:43] <- 0
dataspaincat$closing_business <- 1
dataspaincat$closing_business[1:43] <- 0
dataspaincat$limiting_travel <- 1
dataspaincat$limiting_travel[1:38] <- 0

#Data USA
datauscat$mass_gathering_implemented <- 1
datauscat$mass_gathering_implemented[1:54] <- 0
datauscat$education_closed <- 1
datauscat$education_closed[1:80] <- 0
datauscat$stay_home <- 1
datauscat$stay_home[1:54] <- 0
datauscat$non_essential_closed <- 1
datauscat$non_essential_closed[1:60] <- 0
datauscat$closing_business <- 1
datauscat$closing_business[1:60] <- 0
datauscat$limiting_travel <- 1
datauscat$limiting_travel[1:12] <- 0


# -------------------------- ARM IMPLEMENTATION ----------------------------------------#

#parameter
params = list(minlen = 2,
              support = 0.005,
              confidence = 0.8)

#################               INDONESIA ARM                            ###############


#turn class into factor
dataindocat$new_cases_cat <- as.factor(dataindocat$new_cases_cat)
dataindocat$new_deaths_cat <- as.factor(dataindocat$new_deaths_cat)
dataindocat$mass_gathering_implemented <- as.factor(dataindocat$mass_gathering_implemented) 
dataindocat$education_closed <- as.factor(dataindocat$education_closed)
dataindocat$stay_home <- as.factor(dataindocat$stay_home)
dataindocat$non_essential_closed <- as.factor(dataindocat$non_essential_closed)
dataindocat$closing_business <- as.factor(dataindocat$closing_business)
dataindocat$limiting_travel <- as.factor(dataindocat$limiting_travel)

dataindocat$date <- NULL
dataindocat$total_cases <- NULL
dataindocat$total_cases_per_million <- NULL
dataindocat$total_deaths <- NULL
dataindocat$total_deaths_per_million <- NULL
dataindocat$new_cases <- NULL
dataindocat$new_deaths <- NULL
dataindocat$new_deaths_per_million <- NULL
dataindocat$new_deaths_per_million_cat <- NULL
dataindocat$new_cases_per_million <- NULL
dataindocat$new_cases_per_million_cat <- NULL


rules <- apriori(dataindocat,
                 parameter = params,list(rhs = c('new_deaths_cat=tidak naik')))

inspect(rules[1:10])

subset_matrix <- is.subset(rules, rules)
subset_matrix[lower.tri(subset_matrix)] <- FALSE
redundant <- colSums(subset_matrix) > 1
rules_pruned <- rules[!redundant]
inspect(rules_pruned)

dataindocat$new_deaths_cat <- NULL

rules <- apriori(dataindocat,
                 parameter = params,list(lhs = c('new_cases_cat=naik')))

inspect(rules)

subset_matrix <- is.subset(rules, rules)
subset_matrix[lower.tri(subset_matrix)] <- FALSE
redundant <- colSums(subset_matrix) > 1
rules_pruned <- rules[!redundant]
inspect(rules_pruned)

#################               German ARM                            ###############
datagermancat$new_cases_cat <- as.factor(datagermancat$new_cases_cat)
datagermancat$new_deaths_cat <- as.factor(datagermancat$new_deaths_cat)
datagermancat$mass_gathering_implemented <- as.factor(datagermancat$mass_gathering_implemented) 
datagermancat$education_closed <- as.factor(datagermancat$education_closed)
datagermancat$stay_home <- as.factor(datagermancat$stay_home)
datagermancat$non_essential_closed <- as.factor(datagermancat$non_essential_closed)
datagermancat$closing_business <- as.factor(datagermancat$closing_business)
datagermancat$limiting_travel <- as.factor(datagermancat$limiting_travel)

datagermancat$date <- NULL
datagermancat$total_cases <- NULL
datagermancat$total_cases_per_million <- NULL
datagermancat$total_deaths <- NULL
datagermancat$total_deaths_per_million <- NULL
datagermancat$new_cases <- NULL
datagermancat$new_deaths <- NULL
datagermancat$new_deaths_per_million <- NULL
datagermancat$new_deaths_per_million_cat <- NULL
datagermancat$new_cases_per_million <- NULL
datagermancat$new_cases_per_million_cat <- NULL

rules <- apriori(datagermancat,
                 parameter = params,list(rhs = c('new_deaths_cat=tidak naik')))

inspect(rules[1:10])

subset_matrix <- is.subset(rules, rules)
subset_matrix[lower.tri(subset_matrix)] <- FALSE
redundant <- colSums(subset_matrix) > 1
rules_pruned <- rules[!redundant]
inspect(rules_pruned)

datagermancat$new_deaths_cat <- NULL

rules <- apriori(datagermancat,
                 parameter = params,list(rhs = c('new_cases_cat=tidak naik')))

inspect(rules[1:5])


#################               itali ARM                            ###############
dataitcat$new_cases_cat <- as.factor(dataitcat$new_cases_cat)
dataitcat$new_deaths_cat <- as.factor(dataitcat$new_deaths_cat)
dataitcat$mass_gathering_implemented <- as.factor(dataitcat$mass_gathering_implemented) 
dataitcat$education_closed <- as.factor(dataitcat$education_closed)
dataitcat$stay_home <- as.factor(dataitcat$stay_home)
dataitcat$non_essential_closed <- as.factor(dataitcat$non_essential_closed)
dataitcat$closing_business <- as.factor(dataitcat$closing_business)
dataitcat$limiting_travel <- as.factor(dataitcat$limiting_travel)

dataitcat$date <- NULL
dataitcat$total_cases <- NULL
dataitcat$total_cases_per_million <- NULL
dataitcat$total_deaths <- NULL
dataitcat$total_deaths_per_million <- NULL
dataitcat$new_cases <- NULL
dataitcat$new_deaths <- NULL
dataitcat$new_deaths_per_million <- NULL
dataitcat$new_deaths_per_million_cat <- NULL
dataitcat$new_cases_per_million <- NULL
dataitcat$new_cases_per_million_cat <- NULL

rules <- apriori(dataitcat,
                 parameter = params,list(rhs = c('new_deaths_cat=tidak naik')))

inspect(rules[1:5])

subset_matrix <- is.subset(rules, rules)
subset_matrix[lower.tri(subset_matrix)] <- FALSE
redundant <- colSums(subset_matrix) > 1
rules_pruned <- rules[!redundant]
inspect(rules_pruned)

rules <- apriori(dataitcat,
                 parameter = params,list(rhs = c('new_cases_cat=tidak naik')))

inspect(rules[1:5])

subset_matrix <- is.subset(rules, rules)
subset_matrix[lower.tri(subset_matrix)] <- FALSE
redundant <- colSums(subset_matrix) > 1
rules_pruned <- rules[!redundant]
inspect(rules_pruned)

#################               united state ARM                            ###############
datauscat$new_cases_cat <- as.factor(datauscat$new_cases_cat)
datauscat$new_deaths_cat <- as.factor(datauscat$new_deaths_cat)
datauscat$mass_gathering_implemented <- as.factor(datauscat$mass_gathering_implemented) 
datauscat$education_closed <- as.factor(datauscat$education_closed)
datauscat$stay_home <- as.factor(datauscat$stay_home)
datauscat$non_essential_closed <- as.factor(datauscat$non_essential_closed)
datauscat$closing_business <- as.factor(datauscat$closing_business)
datauscat$limiting_travel <- as.factor(datauscat$limiting_travel)

datauscat$date <- NULL
datauscat$total_cases <- NULL
datauscat$total_cases_per_million <- NULL
datauscat$total_deaths <- NULL
datauscat$total_deaths_per_million <- NULL
datauscat$new_cases <- NULL
datauscat$new_deaths <- NULL
datauscat$new_deaths_per_million <- NULL
datauscat$new_deaths_per_million_cat <- NULL
datauscat$new_cases_per_million <- NULL
datauscat$new_cases_per_million_cat <- NULL

rules <- apriori(datauscat,
                 parameter = params,list(rhs = c('new_deaths_cat=tidak naik')))

inspect(rules[1:5])

subset_matrix <- is.subset(rules, rules)
subset_matrix[lower.tri(subset_matrix)] <- FALSE
redundant <- colSums(subset_matrix) > 1
rules_pruned <- rules[!redundant]
inspect(rules_pruned)

datauscat$new_deaths_cat <- NULL

rules <- apriori(datauscat,
                 parameter = params,list(lhs = c('new_cases_cat=tidak naik')))

inspect(rules)

subset_matrix <- is.subset(rules, rules)
subset_matrix[lower.tri(subset_matrix)] <- FALSE
redundant <- colSums(subset_matrix) > 1
rules_pruned <- rules[!redundant]
inspect(rules_pruned)

#################              spain ARM                            ###############

dataspaincat$new_cases_cat <- as.factor(dataspaincat$new_cases_cat)
dataspaincat$new_deaths_cat <- as.factor(datauscat$new_deaths_cat)
dataspaincat$mass_gathering_implemented <- as.factor(dataspaincat$mass_gathering_implemented) 
dataspaincat$education_closed <- as.factor(dataspaincat$education_closed)
dataspaincat$stay_home <- as.factor(dataspaincat$stay_home)
dataspaincat$non_essential_closed <- as.factor(dataspaincat$non_essential_closed)
dataspaincat$closing_business <- as.factor(dataspaincat$closing_business)
dataspaincat$limiting_travel <- as.factor(dataspaincat$limiting_travel)

dataspaincat$date <- NULL
dataspaincat$total_cases <- NULL
dataspaincat$total_cases_per_million <- NULL
dataspaincat$total_deaths <- NULL
dataspaincat$total_deaths_per_million <- NULL
dataspaincat$new_cases <- NULL
dataspaincat$new_deaths <- NULL
dataspaincat$new_deaths_cat <- NULL
dataspaincat$new_deaths_per_million <- NULL
dataspaincat$new_deaths_per_million_cat <- NULL
dataspaincat$new_cases_per_million <- NULL
dataspaincat$new_cases_per_million_cat <- NULL

rules <- apriori(dataspaincat,
                 parameter = params,list(rhs = c('new_cases_cat=tidak naik')))

inspect(rules)

rules <- apriori(dataspaincat,
                 parameter = params,list(rhs = c('new_deaths_cat=tidak naik')))

inspect(rules[1:5])

subset_matrix <- is.subset(rules, rules)
subset_matrix[lower.tri(subset_matrix)] <- FALSE
redundant <- colSums(subset_matrix) > 1
rules_pruned <- rules[!redundant]
inspect(rules_pruned)
