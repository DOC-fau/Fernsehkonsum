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
library(haven)
library(expss)
library(here)


#########Vorbereitung des Datensatzes
main_df <- read_sav(file = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Datensatz/allbus-daten_2018.sav")
main_df <- as.tibble(main_df)

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
# sex: RESPONDENT: SEX
# agec: RESPONDENT: AGE, CATEGORIZED
# educ: RESP.: GENERAL SCHOOL LEAVING CERTIFICATE
# de15: RESP.: UNIVERSITY DEGREE
# de18: RESP.: TYPE OF UNIVERSITY DEGREE
# work: RESPONDENT: CURRENT EMPLOYMENT STATUS
# dw01: RESPONDENT: CURRENT OCCUPATION
# dw10: RESP.: SUPERVISING THE WORK OF OTHERS?
# dw18: RESP.: UNEMPLOYMENT IN THE LAST 10 YEARS
# hhincc: HH.: MONTHLY NET INC.-OPEN+CLOSED, CAT.
# pp81: HOW OFTEN TALK ABOUT POLITICS: IN FAMILY
# pp82: HOW OFTEN TALK ABOUT POLITICS: FRIENDS
# gs01: SELF-ASSESSMENT OF PLACE WHERE R LIVES
# pv03: DID YOU VOTE IN LAST FEDERAL ELECTION?
# pv04: PARTY VOTE IN LAST FEDERAL ELECTION
# xr27: INT.: SOCIAL CLASS OF HOUSEHOLD
# land: FEDERAL STATE THAT RESPONDENT LIVES IN

# wghtpew: WEIGHT: EAST-WEST, PERSON-LEVEL

#eastwest [5], german [6], sex [159], agec [163], educ [181], work [198], hhincc [395], land [701], wghtpew [705]


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

main_df <- main_df[, c(5:6, 8, 11, 13:19, 70:71, 78, 82, 84, 91, 105, 128, 159, 163, 181, 191, 194, 198, 199, 210, 213, 395, 426:427, 435, 472:473, 685, 701, 705)]

main_df$ardZdf <- as_factor(main_df$ardZdf)
main_df$eastwest <- as_factor(main_df$eastwest)
main_df$german <- as_factor(main_df$german)
main_df$finSit <- as_factor(main_df$finSit)
main_df$tvWeek <- as_factor(main_df$tvWeek)
main_df$freqNewsPubl <- as_factor(main_df$freqNewsPubl)
main_df$newsPriv <- as_factor(main_df$newsPriv)
main_df$freqNewsPriv <- as_factor(main_df$freqNewsPriv)
main_df$freqNewsp <- as_factor(main_df$freqNewsp)
main_df$intPol <- as_factor(main_df$intPol)
main_df$freqIntPol <- as_factor(main_df$freqIntPol)
main_df$trustTv <- as_factor(main_df$trustTv)
main_df$trustNewsp <- as_factor(main_df$trustNewsp)
main_df$infMedia <- as_factor(main_df$infMedia)
main_df$polCompl <- as_factor(main_df$polCompl)
main_df$knowlPol <- as_factor(main_df$knowlPol)
main_df$shldInfPol <- as_factor(main_df$shldInfPol)
main_df$leftRight <- as_factor(main_df$leftRight)
main_df$class <- as_factor(main_df$class)
main_df$sex <- as_factor(main_df$sex)
main_df$agec <- as_factor(main_df$agec)
main_df$educ <- as_factor(main_df$educ)
main_df$uniDeg <- as_factor(main_df$uniDeg)
main_df$typeUniDeg <- as_factor(main_df$typeUniDeg)
main_df$work <- as_factor(main_df$work)
main_df$currOcc <- as_factor(main_df$currOcc)
main_df$subvOther <- as_factor(main_df$subvOther)
main_df$unempl <- as_factor(main_df$unempl)
main_df$hhincc <- as_factor(main_df$hhincc)
main_df$freqPolFam <- as_factor(main_df$freqPolFam)
main_df$freqPolFre <- as_factor(main_df$freqPolFre)
main_df$placeLive <- as_factor(main_df$placeLive)
main_df$vote <- as_factor(main_df$vote)
main_df$partyVote <- as_factor(main_df$partyVote)
main_df$intClassH <- as_factor(main_df$intClassH)
main_df$land <- as_factor(main_df$land)
main_df$wghtpew <- as_factor(main_df$wghtpew)

#####Speichern:
save(main_df, file = "main_df.RData")

#####Ende