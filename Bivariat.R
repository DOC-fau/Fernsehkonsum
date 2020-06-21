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
main_df_desc2 <- main_df


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
# typeUniDeg: RESP.: TYPE OF UNIVERSITY DEGREE
# work: RESPONDENT: CURRENT EMPLOYMENT STATUS
# currOcc: RESPONDENT: CURRENT OCCUPATION
# subvOther: RESP.: SUPERVISING THE WORK OF OTHERS?
# unempl: RESP.: UNEMPLOYMENT IN THE LAST 10 YEARS
# incc: RESP.: MONTHLY NET INC.-OPEN+CLOSED Q.,CAT
# hhincc: HH.: MONTHLY NET INC.-OPEN+CLOSED, CAT.
# freqPolFam: HOW OFTEN TALK ABOUT POLITICS: IN FAMILY
# freqPolFre: HOW OFTEN TALK ABOUT POLITICS: FRIENDS
# placeLive: SELF-ASSESSMENT OF PLACE WHERE R LIVES
# vote: DID YOU VOTE IN LAST FEDERAL ELECTION?
# partyVote: PARTY VOTE IN LAST FEDERAL ELECTION
# intClassH: INT.: SOCIAL CLASS OF HOUSEHOLD
# land: FEDERAL STATE THAT RESPONDENT LIVES IN

# wghtpew: WEIGHT: EAST-WEST, PERSON-LEVEL


###Funktionen:
chisqFunc <- function(crossTab){chisq.test(crossTab)}
cramersVFunc <- function(chisq, crossTab){sqrt((chisq)/(margin.table(crossTab)*(min(nrow(crossTab) - 1,ncol(crossTab) - 1))))}
korrKFunc <- function(chisq, crossTab){(sqrt((chisq)/((margin.table(crossTab)+(chisq)))))/(sqrt((min(nrow(crossTab), ncol(crossTab)) - 1)/(min(nrow(crossTab), ncol(crossTab)))))}
E_2Row <- function(crossTab){
  x <- vector("numeric", ncol(crossTab))
  for (i in 1:ncol(crossTab)) {
    x[i] <- sum(crossTab[,i]) - max(crossTab[,i])
  }
  sum(x)}
E_2Col <- function(crossTab){
  x <- vector("numeric", nrow(crossTab))
  for (i in 1:nrow(crossTab)) {
    x[i] <- sum(crossTab[i,]) - max(crossTab[i,])
  }
  sum(x)}
E_1Row <- function(crossTab){sum(crossTab) - max(sum(crossTab[ncol(crossTab), ]))}
E_1Col <- function(crossTab){sum(crossTab) - max(sum(crossTab[, nrow(crossTab)]))}
lambdaRow <- function(crossTab){  (E_1Row(crossTab) - E_2Row(crossTab))/(E_1Row(crossTab))}
lambdaCol <- function(crossTab){  (E_1Col(crossTab) - E_2Col(crossTab))/(E_1Col(crossTab))}
ZsmNom <- function(crossTab){
  chisqZsm <- (chisqFunc(crossTab)[[1]])[[1]]
  cramersVZsm <- (cramersVFunc(chisqZsm[[1]], crossTab))[[1]]
  korrKZsm <- (korrKFunc(chisqZsm[[1]], crossTab))[[1]]
  lambdaCol <- lambdaCol(crossTab)
  lambdaRow <- lambdaRow(crossTab)
  
  df <- data.frame(chisqZsm, cramersVZsm, korrKZsm, lambdaCol, lambdaRow)
  df <- rename(df, CramersV = cramersVZsm, normKontigenzkoeffizient = korrKZsm, Chisq = chisqZsm)
  }
  

#Zusammenhang zwischen ardZdf und eastWest:
(descValEastWest <- main_df_desc2 %>%
                    with(table(main_df_desc2$eastwest, main_df_desc2$ardZdf)) %>%
                    ZsmNom())


#Zusammenhang zwischen ardZdf und unempl:
(descValunempl <- main_df_desc2 %>%
    with(table(main_df_desc2$unempl, main_df_desc2$ardZdf)) %>%
    ZsmNom())


#Zusammenhang zwischen ardZdf und vote:
(descValEastWest <- main_df_desc2 %>%
    with(table(main_df_desc2$vote, main_df_desc2$ardZdf)) %>%
    ZsmNom())


#Zusammenhang zwischen ardZdf und sex:
(descValSex <- main_df_desc2 %>%
    with(table(main_df_desc2$sex, main_df_desc2$ardZdf)) %>%
    ZsmNom())


#Zusammenhang zwischen ardZdf und intPol:
(descValSex <- main_df_desc2 %>%
    with(table(main_df_desc2$intPol, main_df_desc2$ardZdf)) %>%
    ZsmNom())

#####Ende