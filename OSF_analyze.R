#The purpose of this script is to analyze the data from the OSF_structure script

################################################################################
#Get survey weighted means for Artificial Intelligence x Institution
################################################################################

#American Police Precincts
meanU <- useAIcop %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
police_use <- data.frame(svymean(~Response, svy))

meanN <- useAIcop %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
police_none <- data.frame(svymean(~Response, svy))

#American Companies
meanU <- useAIcomp %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
comp_use <- data.frame(svymean(~Response, svy))

meanN <- useAIcomp %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
comp_none <- data.frame(svymean(~Response, svy))

#American Government Agencies
meanU <- useAIgov %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
gov_use <- data.frame(svymean(~Response, svy))

meanN <- useAIgov %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
gov_none <- data.frame(svymean(~Response, svy))

#American Hospitals
meanU <- useAIhos %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
hos_use <- data.frame(svymean(~Response, svy))

meanN <- useAIhos %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
hos_none <- data.frame(svymean(~Response, svy))

#Academic Research Teams
meanD <- developAIacad %>%
  filter(Verb == "Develop")
svy <- svydesign(data = meanD, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
acad_develop <- data.frame(svymean(~Response, svy))

meanN <- developAIacad %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
acad_none <- data.frame(svymean(~Response, svy))

#American research labs
meanD <- developAIlab %>%
  filter(Verb == "Develop")
svy <- svydesign(data = meanD, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
lab_develop <- data.frame(svymean(~Response, svy))

meanN <- developAIlab %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
lab_none <- data.frame(svymean(~Response, svy))

#Storing results in dataframe
meansAI <- list(comp_none,comp_use,gov_none,gov_use,hos_none,hos_use,
                police_none,police_use,acad_none,acad_develop,lab_none,lab_develop)
meansAI <- do.call(rbind, meansAI)
rownames(meansAI) <- c("company_none","company_use","gov_none","gov_use",
                       "hos_none","hos_use","police_none","police_use",
                       "acad_none","acad_use","lab_none","lab_develop")

############################################################################
#Test for a statistically significant difference with svyttest function.
############################################################################

svy <- svydesign(data = useAIcop, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = useAIcomp, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = useAIgov, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = useAIhos, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = developAIacad, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = developAIlab, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = createBTlab, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = createMTlab, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

################################################################################
################################################################################
#Get survey weighted means for Machine Learning x Institution
################################################################################
################################################################################

#American Police Precincts
meanU <- useMLcop %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
police_use <- data.frame(svymean(~Response, svy))

meanN <- useMLcop %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
police_none <- data.frame(svymean(~Response, svy))

#American Companies
meanU <- useMLcomp %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
comp_use <- data.frame(svymean(~Response, svy))

meanN <- useMLcomp %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
comp_none <- data.frame(svymean(~Response, svy))

#Storing results in dataframe

meansML <- list(comp_none,comp_use,police_none,police_use)
meansML <- do.call(rbind, meansML)
rownames(meansML) <- c("company_none","company_use", "police_none","police_use")

#Test for a statistically significant difference with svyttest function.

svy <- svydesign(data = useMLcop, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = useMLcomp, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

################################################################################
################################################################################
#Get survey weighted means for Smartphone Apps x Institution
################################################################################
################################################################################

#American Police Precincts
meanU <- useAPPcop %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
police_use <- data.frame(svymean(~Response, svy))

meanN <- useAPPcop %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
police_none <- data.frame(svymean(~Response, svy))

#American Companies
meanU <- useAPPcomp %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
comp_use <- data.frame(svymean(~Response, svy))

meanN <- useAPPcomp %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
comp_none <- data.frame(svymean(~Response, svy))

#Storing results in dataframe

meansAPP <- list(comp_none,comp_use,police_none,police_use)
meansAPP <- do.call(rbind, meansAPP)
rownames(meansAPP) <- c("company_none","company_use", "police_none","police_use")

#Test for a statistically significant difference with svyttest function.

svy <- svydesign(data = useAPPcop, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = useAPPcomp, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

################################################################################
################################################################################
#Get survey weighted means for Implicit bias training x Institution
################################################################################
################################################################################

#American Police Precincts
meanU <- useBTcop %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
police_use <- data.frame(svymean(~Response, svy))

meanN <- useBTcop %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
police_none <- data.frame(svymean(~Response, svy))

#American Companies
meanU <- useBTcomp %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
comp_use <- data.frame(svymean(~Response, svy))

meanN <- useBTcomp %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
comp_none <- data.frame(svymean(~Response, svy))

#Storing results in dataframe

meansBT <- list(comp_none,comp_use,police_none,police_use)
meansBT <- do.call(rbind, meansBT)
rownames(meansBT) <- c("company_none","company_use", "police_none","police_use")

#Test for a statistically significant difference with svyttest function.

svy <- svydesign(data = useBTcop, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = useBTcomp, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

################################################################################
################################################################################
#Get survey weighted means for Mindfullness training x Institution
################################################################################
################################################################################

#American Police Precincts
meanU <- useMTcop %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
police_use <- data.frame(svymean(~Response, svy))

meanN <- useMTcop %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
police_none <- data.frame(svymean(~Response, svy))

#American Companies
meanU <- useMTcomp %>%
  filter(Verb == "Use")
svy <- svydesign(data = meanU, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
comp_use <- data.frame(svymean(~Response, svy))

meanN <- useMTcomp %>%
  filter(Verb == "None")
svy <- svydesign(data = meanN, ids = ~User_ID, weights = ~Weight)
svymean(~Response, svy)
comp_none <- data.frame(svymean(~Response, svy))

#Storing results in dataframe

meansMT <- list(comp_none,comp_use,police_none,police_use)
meansMT <- do.call(rbind, meansMT)
rownames(meansMT) <- c("company_none","company_use", "police_none","police_use")

#Test for a statistically significant difference with svyttest function.

svy <- svydesign(data = useMTcop, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)

svy <- svydesign(data = useMTcomp, ids = ~User_ID, weights = ~Weight)
svyttest(Response ~ Verb, svy)
