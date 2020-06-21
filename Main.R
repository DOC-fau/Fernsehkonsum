#####################################
#########Packages
install.packages("tidyverse")
install.packages("foreign")
install.packages("vcd")
install.packages("psych")
install.packages("reshape")
install.packages("stats19")
install.packages("Hmisc")
install.packages("polycor")
install.packages("car")
install.packages("broom")
install.packages("Amelia")
install.packages("pscl")
install.packages("rcompanion")
install.packages("janitor")
install.packages("DMwR")
install.packages("expss")
install.packages("here")

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
library(expss)
library(here)


#########Vorbereitung des Datensatzes
main_df <- read.spss(file = "H:/01 Studium/01 Bachelor of Arts/03 Schluesselqualifikationen/Einfuehrung in R/Seminararbeit/Datensatz/allbus-daten_2018.sav", to.data.frame=TRUE)
main_df <- as_tibble(main_df)

#####Variablenliste:
# lm19 TV: WATCH NEWS ON PUBLIC CHANNELS?

# eastwest: REGION OF INTERVIEW: WEST - EAST
# german: GERMAN CITIZENSHIP?
# ep03: RESP. OWN CURRENT FINANCIAL SITUATION
# lm01: WATCH TV: HOW MANY DAYS A WEEK
# lm20: TV: HOW OFTEN NEWS ON PUBLIC CHANNELS?
# lm21: TV: WATCH NEWS ON PRIVATE CHANNELS?
# lm22: TV: HOW OFTEN NEWS ON PRIVATE CHANNELS?
# lm14: NEWSPAPER: HOW MANY DAYS A WEEK
# lm23: INTERNET: FOR INFORMATION ON POLITICS
# lm24: HOW OFTEN INTERNET FOR INFO ON POLITICS?
# pt09: TRUST: TELEVISION
# pt10: TRUST: NEWSPAPERS
# pa21: MEDIA: INFLUENCE ON PUBLIC OPINION
# pe04: POLITICS IS TOO COMPLICATED FOR ME
# pe06: I DO NOT KNOW MUCH ABOUT POLITICS
# pe13: PEOPLE SHLD STAY INFORMED ABOUT POLITICS
# pa01: SELF-PLACEMENT ON LEFT-RIGHT CONTINUUM
# id02: SELF-ASSESSMENT OF SOCIAL CLASS, RESP
# sex:  RESPONDENT: SEX
# agec: RESPONDENT: AGE, CATEGORIZED
# educ: RESP.: GENERAL SCHOOL LEAVING CERTIFICATE
# de15: RESP.: UNIVERSITY DEGREE
# de18: RESP.: TYPE OF UNIVERSITY DEGREE
# work: RESPONDENT: CURRENT EMPLOYMENT STATUS
# dw01: RESPONDENT: CURRENT OCCUPATION
# dw10: RESP.: SUPERVISING THE WORK OF OTHERS?
# dw18: RESP.: UNEMPLOYMENT IN THE LAST 10 YEARS
# incc: RESP.: MONTHLY NET INC.-OPEN+CLOSED Q.,CAT
# hhincc: HH.: MONTHLY NET INC.-OPEN+CLOSED, CAT.
# pp81: HOW OFTEN TALK ABOUT POLITICS: IN FAMILY
# pp82: HOW OFTEN TALK ABOUT POLITICS: FRIENDS
# gs01: SELF-ASSESSMENT OF PLACE WHERE R LIVES
# pv03: DID YOU VOTE IN LAST FEDERAL ELECTION?
# pv04: PARTY VOTE IN LAST FEDERAL ELECTION
# xr27: INT.: SOCIAL CLASS OF HOUSEHOLD
# land: FEDERAL STATE THAT RESPONDENT LIVES IN

# wghtpew: WEIGHT: EAST-WEST, PERSON-LEVEL

#eastwest [5], german [6], sex [159], agec [163], educ [181], work [198], incc [387], hhincc [395], land [701], wghtpew [705]


#####Aufbereitung der Variablen:
###Recodieren der Variablen:
names(main_df)[13] <- "ardZdf"

names(main_df)[8] <- "finSit"
names(main_df)[11] <- "tvWeek"
names(main_df)[14] <- "freqNewsPubl"
names(main_df)[15] <- "newsPriv"
names(main_df)[16] <- "freqNewsPriv"
names(main_df)[17] <- "freqNewsp"
names(main_df)[18] <- "intPol"
names(main_df)[19] <- "freqIntPol"
names(main_df)[70] <- "trustTv"
names(main_df)[71] <- "trustNewsp"
names(main_df)[78] <- "infMedia"
names(main_df)[82] <- "polCompl"
names(main_df)[84] <- "knowlPol"
names(main_df)[91] <- "shldInfPol"
names(main_df)[105] <- "leftRight"
names(main_df)[128] <- "class"
names(main_df)[191] <- "uniDeg"
names(main_df)[194] <- "typeUniDeg"
names(main_df)[199] <- "currOcc"
names(main_df)[210] <- "subvOther"
names(main_df)[213] <- "unempl"
names(main_df)[426] <- "freqPolFam"
names(main_df)[427] <- "freqPolFre"
names(main_df)[435] <- "placeLive"
names(main_df)[472] <- "vote"
names(main_df)[473] <- "partyVote"
names(main_df)[685] <- "intClassH"

levels(main_df$trustTv) <- c("KEIN VERTRAUEN", "2", "3", "4", "5", "6", "GROßES VERTRAUEN")
levels(main_df$trustNewsp) <- c("KEIN VERTRAUEN", "2", "3", "4", "5", "6", "GROßES VERTRAUEN")

###Eingrenzen des Datensatzes:
main_df <- main_df[, c(5:6, 8, 11, 13:19, 70:71, 78, 82, 84, 91, 105, 128, 159, 163, 181, 191, 194, 198, 199, 210, 213, 
                       338, 395, 426:427, 435, 472:473, 685, 701, 705)]


#####Speichern:
save(main_df, file = "main_df.RData")

#####Ende