#####################################
#########Packages
library(foreign)
library(tidyverse)
library(foreign)
library(psych)
library(reshape2)
library(ggplot2)
library(car)
library(stats)
library(Hmisc)
library(vcd)
library(broom)
library(Amelia)
library(pscl)
library(rcompanion)
library(DMwR)
library(haven)
library(expss)
library(here)


#####Multivariate Analyse:
load("main_df.RData")
main_df_logReg <- main_df
main_df_logReg <- main_df_logReg[, c(1:21, 23, 25, 27:33, 37)]
main_df_logReg <- droplevels(main_df_logReg)


###Variablenliste
# ardZdf: TV: WATCH NEWS ON PUBLIC CHANNELS?

# eastwest: REGION OF INTERVIEW: WEST - EAST
# german: GERMAN CITIZENSHIP?
# finSit: RESP. OWN CURRENT FINANCIAL SITUATION
# tvWeek: WATCH TV: HOW MANY DAYS A WEEK
# freqNewsPubl: TV: HOW OFTEN NEWS ON PUBLIC CHANNELS?
# newsPriv: TV: WATCH NEWS ON PRIVATE CHANNELS?
# freqNewsPriv: TV: HOW OFTEN NEWS ON PRIVATE CHANNELS?
# freqNewsp: NEWSPAPER: HOW MANY DAYS A WEEK
# intPol: INTERNET: FOR INFORMATION ON POLITICS
# freqIntPol: HOW OFTEN INTERNET FOR INFO ON POLITICS?
# trustTv: TRUST: TELEVISION
# trustNewsp: TRUST: NEWSPAPERS
# infMedia: MEDIA: INFLUENCE ON PUBLIC OPINION
# polCompl: POLITICS IS TOO COMPLICATED FOR ME
# knowlPol: I DO NOT KNOW MUCH ABOUT POLITICS
# shldInfPol: PEOPLE SHLD STAY INFORMED ABOUT POLITICS
# leftRight: SELF-PLACEMENT ON LEFT-RIGHT CONTINUUM
# class: SELF-ASSESSMENT OF SOCIAL CLASS, RESP
# sex: RESPONDENT: SEX
# agec: RESPONDENT: AGE, CATEGORIZED
# educ: RESP.: GENERAL SCHOOL LEAVING CERTIFICATE
# uniDeg: RESP.: UNIVERSITY DEGREE
# work: RESPONDENT: CURRENT EMPLOYMENT STATUS
# subvOther: RESP.: SUPERVISING THE WORK OF OTHERS?
# unempl: RESP.: UNEMPLOYMENT IN THE LAST 10 YEARS
# hhincc: HH.: MONTHLY NET INC.-OPEN+CLOSED, CAT.
# freqPolFam: HOW OFTEN TALK ABOUT POLITICS: IN FAMILY
# freqPolFre: HOW OFTEN TALK ABOUT POLITICS: FRIENDS
# placeLive: SELF-ASSESSMENT OF PLACE WHERE R LIVES
# vote: DID YOU VOTE IN LAST FEDERAL ELECTION?
# intClassH: INT.: SOCIAL CLASS OF HOUSEHOLD

# wghtpew: WEIGHT: EAST-WEST, PERSON-LEVEL


###Variablen nach NA filtern:
main_df_logReg %>% missmap(main = "Missing values vs observed")
main_df_logReg <- main_df_logReg[, c(1:5, 7, 9:10, 12:23, 26:30)]
main_df_logReg <- na.omit(main_df_logReg)
main_df_logReg %>% missmap(main = "Missing values vs observed")


###Regressionsmodell:
##Modellrechnung:
#Null-Modell:
model.null = glm(ardZdf ~ 1,
                 data = main_df_logReg,
                 family = binomial(link = "logit")
)


#Total-Modell
model.full = glm(ardZdf ~ .,
                 data=main_df_logReg,
                 family = binomial(link = "logit")
)


#Step-procedure:
step(model.null,
     scope = list(upper=model.full),
     direction = "both",
     test = "Chisq",
     data  =main_df_logReg)


#Final-Modell:
model.final = glm(ardZdf ~ agec + freqPolFam + knowlPol + class + 
                  tvWeek + uniDeg + trustTv + freqNewsp + vote + eastwest + 
                  freqPolFre,
                  data=  main_df_logReg,
                  family = binomial(link = "logit")
)


#Koeffizienten:
summary(model.final)
nagelkerke(model.final)
Anova(model.final, type="II", test="Wald")


##Modellvoraussetzungen:
#Multikollinearität:
vif(model.final)

#####Ende