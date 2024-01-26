#The purpose of this script is to organize and structure the data for analysis

library(tidyverse)
library(lubridate)
library(survey)
library(plyr)
library(ggplot2)
library(ggpubr)
library(broom)

# Load the stacked file.
plusai = read_csv("")

# There are a very few rows with unknown demographics and zero Weight.
# They are useless, so remove them.
nrow(plusai)
plusai[plusai$Weight == 0,] # Examine.
plusai = plusai[!(plusai$Weight == 0),] # Remove.
nrow(plusai)

# Sanity checks and get to know the data.
str(plusai)
x <- data.frame(table(plusai$Full_Prompt))

table(plusai$Survey_Date)
table(plusai$Response)
table(plusai$Gender)
table(plusai$Age)
hist(plusai$Weight)
summary(plusai$Weight)

#hist(plusai$RT)
summary(plusai$RT)

# Add a column for months since first survey.
plusai = plusai %>%
  mutate(Months_Since_First_Survey = round(as.double(difftime(Survey_Date, as.Date("2020-07-01"), units = "weeks")) / 4.34) )
table(plusai$Months_Since_First_Survey)
# 4.34 weeks per month.  Round to get integer value.

# Add a column for Institution.
plusai = plusai %>% 
  mutate(Institution = NA) %>%
  mutate(Institution = ifelse(grepl("academic research teams", Full_Prompt), "Academic Research Teams", Institution)) %>%
  mutate(Institution = ifelse(grepl("American companies", Full_Prompt), "American Companies", Institution)) %>%
  mutate(Institution = ifelse(grepl("American government agencies", Full_Prompt), "American Government Agencies", Institution)) %>%
  mutate(Institution = ifelse(grepl("American hospitals", Full_Prompt), "American Hospitals", Institution)) %>%
  mutate(Institution = ifelse(grepl("American police precincts", Full_Prompt), "American Police Precincts", Institution)) %>%
  mutate(Institution = ifelse(grepl("American research labs", Full_Prompt), "American Research Labs", Institution)) %>%
  mutate(Institution = ifelse(grepl("the average American", Full_Prompt), "The Average American", Institution))

# Inspect
unique(plusai$Institution)
table(plusai$Institution)
sum(is.na(plusai$Institution))

# Add a column for Plus_Tech
plusai = plusai %>% 
  mutate(Plus_Tech = "None") %>%
  mutate(Plus_Tech = ifelse(grepl("implicit bias training", Full_Prompt), "Implicit Bias Training", Plus_Tech)) %>%
  mutate(Plus_Tech = ifelse(grepl("mindfulness training", Full_Prompt), "Mindfulness Training", Plus_Tech)) %>%
  mutate(Plus_Tech = ifelse(grepl("smartphone apps", Full_Prompt), "Smartphone Apps", Plus_Tech)) %>%
  mutate(Plus_Tech = ifelse(grepl("machine learning algorithms", Full_Prompt), "Machine Learning Algorithms", Plus_Tech)) %>%
  mutate(Plus_Tech = ifelse(grepl("Artificial Intelligence systems", Full_Prompt), "Artificial Intelligence Systems", Plus_Tech))

# Inspect
unique(plusai$Plus_Tech)
table(plusai$Plus_Tech)
sum(is.na(plusai$Plus_Tech))

xtabs(~ Institution + Plus_Tech, plusai)

# Add a column named Verb for Create, Develop, Use
plusai = plusai %>% 
  mutate(Verb = "None") %>%
  mutate(Verb = ifelse(grepl("\\bcreate\\b", Full_Prompt), "Create", Verb)) %>%
  mutate(Verb = ifelse(grepl("\\bdevelop\\b", Full_Prompt), "Develop", Verb)) %>%
  mutate(Verb = ifelse(grepl("\\buse\\b", Full_Prompt), "Use", Verb))

# Inspect
unique(plusai$Verb)
table(plusai$Verb)
sum(is.na(plusai$Verb))

unique(paste(plusai$Full_Prompt, plusai$Verb))
xtabs(~ Institution + Verb, plusai)
xtabs(~ Plus_Tech + Verb, plusai)
xtabs(~Verb + Institution, plusai)

# Explicitly set types and factor levels.
plusai$Gender = as.factor(plusai$Gender)
plusai$Age = as.ordered(plusai$Age)

# Use survey package to represent results.

# This will help us Weight rows correctly.
plusaiSurvey = svydesign(data = plusai, ids = ~User_ID, weights = ~Weight)
summary(plusaiSurvey)

# Check summary statistics.
svymean(~Response, plusaiSurvey) # same as mean(plusai$Response * plusai$Weight)
#checkResults = svyby(~Response, ~Full_Prompt, plusaiSurvey, svymean, keep.var=TRUE)

###########################################
# Create the tibbles for AI / USE
###########################################

#Use Artificial Intelligence Systems
useAI = plusai %>% 
  # Filter out everything but use AI contrast rows.
  filter(Verb == "Use" | Verb == "None") %>%
  filter(Plus_Tech == "Artificial Intelligence Systems" | Plus_Tech == "None") %>%
  filter(Institution %in% c("American Companies", "American Government Agencies",
                            "American Hospitals", "American Police Precincts"))

xtabs(~ Institution + Plus_Tech + Verb, useAI)

#Use Artificial Intelligence Systems by institution
useAIcop = useAI %>%
  filter(Institution %in% "American Police Precincts")
useAIcomp = useAI %>%
  filter(Institution %in% "American Companies")
useAIgov = useAI %>%
  filter(Institution %in% "American Government Agencies")
useAIhos = useAI %>%
  filter(Institution %in% "American Hospitals")

###########################################
# Create the tibbles for ML / USE
###########################################

#Use Machine Learning Algorithms
useML = plusai %>% 
  filter(Verb == "Use" | Verb == "None") %>%
  filter(Plus_Tech == "Machine Learning Algorithms" | Plus_Tech == "None") %>%
  filter(Institution %in% c("American Companies", "American Police Precincts"))

xtabs(~ Institution + Plus_Tech + Verb, useML)

#Use Machine Learning by institution
useMLcop = useML %>%
  filter(Institution %in% "American Police Precincts")
useMLcomp = useML %>%
  filter(Institution %in% "American Companies")

###########################################
# Create the tibbles for APPS / USE
###########################################

#Use Smartphone Apps
useAPP = plusai %>% 
  filter(Verb == "Use" | Verb == "None") %>%
  filter(Plus_Tech == "Smartphone Apps" | Plus_Tech == "None") %>%
  filter(Institution %in% c("American Companies", "American Police Precincts"))

xtabs(~ Institution + Plus_Tech + Verb, useAPP)

#Use apps by institution
useAPPcop = useAPP %>%
  filter(Institution %in% "American Police Precincts")
useAPPcomp = useAPP %>%
  filter(Institution %in% "American Companies")

######################################################
# Create the tibbles for implicit bias training / USE
#####################################################

#Use implicit bias training
useBT = plusai %>% 
  filter(Verb == "Use" | Verb == "None") %>%
  filter(Plus_Tech == "Implicit Bias Training" | Plus_Tech == "None") %>%
  filter(Institution %in% c("American Companies", "American Police Precincts"))

#Use BT by institution
useBTcop = useBT %>%
  filter(Institution %in% "American Police Precincts")
useBTcomp = useBT %>%
  filter(Institution %in% "American Companies")

xtabs(~ Institution + Plus_Tech + Verb, useBT)

#####################################################
# Create the tibbles for mindfulness training / USE
#####################################################

#Use mindfulness training
useMT = plusai %>% 
  filter(Verb == "Use" | Verb == "None") %>%
  filter(Plus_Tech == "Mindfulness Training" | Plus_Tech == "None") %>%
  filter(Institution %in% c("American Companies", "American Police Precincts"))

xtabs(~ Institution + Plus_Tech + Verb, useMT)

#Use MT by institution
useMTcop = useMT %>%
  filter(Institution %in% "American Police Precincts")
useMTcomp = useMT %>%
  filter(Institution %in% "American Companies")

xtabs(~ Institution + Plus_Tech + Verb, useMT)


###########################################################
# Create separate tibble for American research labs/CREATE
###########################################################

createlab <- plusai %>% 
  filter(Verb == "Create" | Verb == "None") %>%
  filter(Institution %in% c("American Research Labs"))

xtabs(~ Institution + Plus_Tech + Verb, createlab)
xtabs(~ Institution + Verb, createlab)

#Research labs by tech
createAIlab = createlab %>%
  filter(Plus_Tech == "Artificial Intelligence Systems" | Verb == "None")
createMLlab = createlab %>%
  filter(Plus_Tech == "Machine Learning Algorithms"| Verb == "None")
createAPPlab = createlab %>%
  filter(Plus_Tech == "Smartphone Apps"| Verb == "None")
createMTlab = createlab %>%
  filter(Plus_Tech == "Mindfulness Training"| Verb == "None")
createBTlab = createlab %>%
  filter(Plus_Tech == "Implicit Bias Training"| Verb == "None")

####################################################
# Create separate tibble for research labs/DEVELOP
####################################################

developlab <- plusai %>% 
  filter(Verb == "Develop" | Verb == "None") %>%
  filter(Institution %in% c("American Research Labs"))

xtabs(~ Institution + Plus_Tech + Verb, developlab)
xtabs(~ Institution + Verb, developlab)

#Research labs by tech
developAIlab = developlab %>%
  filter(Plus_Tech == "Artificial Intelligence Systems" | Verb == "None")

####################################################
# Create separate tibble for academic research teams 
####################################################

developacad <- plusai %>% 
  filter(Verb == "Develop" | Verb == "None") %>%
  filter(Institution %in% c("Academic Research Teams"))

#Academic Research Teams by tech
developAIacad = developacad %>%
  filter(Plus_Tech == "Artificial Intelligence Systems" | Verb == "None")

xtabs(~ Institution + Plus_Tech + Verb, developacad)
xtabs(~ Institution + Verb, developacad)

avgamerican <- plusai %>% 
  filter(Institution %in% c("The Average American"))

################################################
#Making a survey object
################################################

useAISurvey = svydesign(data = useAI, ids = ~User_ID, weights = ~Weight)

# Run analysis on survey object.
model = svyglm(Response ~ Institution + Verb, useAISurvey)

# Examine coefficients.
summary(model)

# Examine Psuedo-Rsquared. Ref: https://stats.stackexchange.com/questions/46345/how-to-calculate-goodness-of-fit-in-glm-r/46358
with(summary(model), 1 - deviance/null.deviance)

