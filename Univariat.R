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


#####Laden des Datensatzes:
setwd("H:/01 Studium/01 Bachelor of Arts/03 Schluesselqualifikationen/Einfuehrung in R/")
load("./Seminararbeit/Fernsehkonsum/main_df.RData")
main_df_desc <- main_df

dir.create("./Seminararbeit/Bericht/Graphiken")
unlink("./Seminararbeit/Bericht/Graphiken/*.uni.pdf")


#####Variablenliste:
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


#####Analyse:
###TV: WATCH NEWS ON PUBLIC CHANNELS?
str(main_df_desc$ardZdf)
summary(main_df_desc$ardZdf)

#Häufigkeiten:
tableArdZdf <- with(main_df_desc, table(main_df_desc$ardZdf))
percfreqTabArdZdf <- round(prop.table(tableArdZdf)*100, 2)
cumFreqTabArdZdf <- cumsum(percfreqTabArdZdf)
freqTabArdZdf <- cbind(tableArdZdf, percfreqTabArdZdf, cumFreqTabArdZdf)
remove(tableArdZdf, percfreqTabArdZdf, cumFreqTabArdZdf)
colnames(freqTabArdZdf)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabArdZdf

#Maße der zentralen Tendenz:
main_df_desc$ardZdf %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$ardZdf %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(ardZdf)) + 
  scale_x_discrete(limits = c("JA", "NEIN", NA)) + 
  labs(y = "Anzahl", x = "Öffentliches TV schauen", title = "TV") +
  ggsave("freqArdZdf.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###REGION OF INTERVIEW: WEST - EAST
str(main_df_desc$eastwest)
summary(main_df_desc$eastwest)

#Häufigkeiten:
tableEastWest <- with(main_df_desc, table(main_df_desc$eastwest))
percfreqTabEastWest <- round(prop.table(tableEastWest)*100, 2)
cumFreqTabEastWest <- cumsum(percfreqTabEastWest)
freqTabEastWest <- cbind(tableEastWest, percfreqTabEastWest, cumFreqTabEastWest)
remove(tableEastWest, percfreqTabEastWest, cumFreqTabEastWest)
colnames(freqTabEastWest)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabEastWest

#Maße der zentralen Tendenz:
main_df_desc$eastwest %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$eastwest %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(eastwest)) + 
  scale_x_discrete(limits = c("ALTE BUNDESLAENDER", "NEUE BUNDESLAENDER")) + 
  labs(y = "Anzahl", x = "Osten oder Westen", title = "Wohngebiet") +
  ggsave("freqEastWest.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###GERMAN CITIZENSHIP?
str(main_df_desc$german)
summary(main_df_desc$german)

#Häufigkeiten:
tableGerman <- with(main_df_desc, table(main_df_desc$german))
percfreqTabGerman <- round(prop.table(tableGerman)*100, 2)
cumFreqTabGerman <- cumsum(percfreqTabGerman)
freqTabGerman <- cbind(tableGerman, percfreqTabGerman, cumFreqTabGerman)
remove(tableGerman, percfreqTabGerman, cumFreqTabGerman)
colnames(freqTabGerman)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabGerman

#Maße der zentralen Tendenz:
main_df_desc$german %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$german %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(german)) + 
  scale_x_discrete(limits = c("JA", "JA,NEBEN ZWEITER", "NEIN", NA)) + 
  labs(y = "Anzahl", x = "Deutsche Staatsangehörigkeit", title = "Staatsangehörigkeit") +
  ggsave("freqGerman.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESP. OWN CURRENT FINANCIAL SITUATION
str(main_df_desc$finSit)
summary(main_df_desc$finSit)

#Häufigkeiten:
tableFinSit <- with(main_df_desc, table(main_df_desc$finSit))
percfreqTabFinSit <- round(prop.table(tableFinSit)*100, 2)
cumFreqTabFinSit <- cumsum(percfreqTabFinSit)
freqTabFinSit <- cbind(tableFinSit, percfreqTabFinSit, cumFreqTabFinSit)
remove(tableFinSit, percfreqTabFinSit, cumFreqTabFinSit)
colnames(freqTabFinSit)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabFinSit

#Maße der zentralen Tendenz:
main_df_desc$finSit %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$finSit %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(finSit)) + 
  scale_x_discrete(limits = c("SEHR GUT", "GUT", "TEILS TEILS", "SCHLECHT", "SEHR SCHLECHT", NA)) + 
  labs(y = "Anzahl", x = "Einschätzung der finanziellen Situation", title = "Finanz. Situation") +
  ggsave("freqFinSit.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###WATCH TV: HOW MANY DAYS A WEEK
str(main_df_desc$tvWeek)
summary(main_df_desc$tvWeek)

#Häufigkeiten:
tableTvWeek <- with(main_df_desc, table(main_df_desc$tvWeek))
percfreqTabTvWeek <- round(prop.table(tableTvWeek)*100, 2)
cumFreqTabTvWeek <- cumsum(percfreqTabTvWeek)
freqTabTvWeek <- cbind(tableTvWeek, percfreqTabTvWeek, cumFreqTabTvWeek)
remove(tableTvWeek, percfreqTabTvWeek, cumFreqTabTvWeek)
colnames(freqTabTvWeek)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabTvWeek

#Maße der zentralen Tendenz:
main_df_desc$tvWeek %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$tvWeek %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(tvWeek)) + 
  scale_x_discrete(limits = c("NIE", "SELTENER", "AN EINEM TAG", "AN 2 TAGEN", "AN 3 TAGEN", "AN 4 TAGEN", "AN 5 TAGEN", "AN 6 TAGEN", "AN ALLEN 7 TAGEN", NA)) + 
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "Anzahl", x = "Anzahl an Tagen an denen TV geschaut wird", title = "TV") +
  ggsave("freqTvWeek.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###TV: HOW OFTEN NEWS ON PUBLIC CHANNELS?
str(main_df_desc$freqNewsPubl)
summary(main_df_desc$freqNewsPubl)

#Häufigkeiten:
tableFreqNewsPubl <- with(main_df_desc, table(main_df_desc$freqNewsPubl))
percfreqTabFreqNewsPubl <- round(prop.table(tableFreqNewsPubl)*100, 2)
cumFreqTabFreqNewsPubl <- cumsum(percfreqTabFreqNewsPubl)
freqTabFreqNewsPubl <- cbind(tableFreqNewsPubl, percfreqTabFreqNewsPubl, cumFreqTabFreqNewsPubl)
remove(tableFreqNewsPubl, percfreqTabFreqNewsPubl, cumFreqTabFreqNewsPubl)
colnames(freqTabFreqNewsPubl)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabFreqNewsPubl

#Maße der zentralen Tendenz:
main_df_desc$freqNewsPubl %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqNewsPubl %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(freqNewsPubl)) + 
  scale_x_discrete(limits = c("NIE", "SELTENER", "AN EINEM TAG", "AN 2 TAGEN", "AN 3 TAGEN", "AN 4 TAGEN", "AN 5 TAGEN", "AN 6 TAGEN", "AN ALLEN 7 TAGEN", NA)) + 
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "Anzahl", x = "Anzahl an Tagen an denen Nachrichten auf öffentlich-rechtlichen Sendern geschaut wird", title = "TV") +
  ggsave("freqFreqNewsPubl.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###TV: WATCH NEWS ON PRIVATE CHANNELS?
str(main_df_desc$newsPriv)
summary(main_df_desc$newsPriv)

#Häufigkeiten:
tableNewsPriv <- with(main_df_desc, table(main_df_desc$newsPriv))
percfreqTabNewsPriv <- round(prop.table(tableNewsPriv)*100, 2)
cumFreqTabNewsPriv <- cumsum(percfreqTabNewsPriv)
freqTabNewsPriv <- cbind(tableNewsPriv, percfreqTabNewsPriv, cumFreqTabNewsPriv)
remove(tableNewsPriv, percfreqTabNewsPriv, cumFreqTabNewsPriv)
colnames(freqTabNewsPriv)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabNewsPriv

#Maße der zentralen Tendenz:
main_df_desc$newsPriv %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$newsPriv %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(newsPriv)) + 
  scale_x_discrete(limits = c("JA", "NEIN", NA)) + 
  labs(y = "Anzahl", x = "Nachrichten schauen auf Privatkanälen", title = "TV") +
  ggsave("freqNewsPriv.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)

###TV: HOW OFTEN NEWS ON PRIVATE CHANNELS?
str(main_df_desc$freqNewsPriv)
summary(main_df_desc$freqNewsPriv)

#Häufigkeiten:
tableFreqNewsPriv <- with(main_df_desc, table(main_df_desc$freqNewsPriv))
percfreqTabFreqNewsPriv <- round(prop.table(tableFreqNewsPriv)*100, 2)
cumFreqTabFreqNewsPriv <- cumsum(percfreqTabFreqNewsPriv)
freqTabFreqNewsPriv <- cbind(tableFreqNewsPriv, percfreqTabFreqNewsPriv, cumFreqTabFreqNewsPriv)
remove(tableFreqNewsPriv, percfreqTabFreqNewsPriv, cumFreqTabFreqNewsPriv)
colnames(freqTabFreqNewsPriv)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabFreqNewsPriv

#Maße der zentralen Tendenz:
main_df_desc$freqNewsPriv %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqNewsPriv %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(freqNewsPriv)) + 
  scale_x_discrete(limits = c("NIE", "SELTENER", "AN EINEM TAG", "AN 2 TAGEN", "AN 3 TAGEN", "AN 4 TAGEN", "AN 5 TAGEN", "AN 6 TAGEN", "AN ALLEN 7 TAGEN", NA)) + 
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "Anzahl", x = "Anzahl an Tagen an denen TV geschaut wird", title = "TV") +  
  ggsave("freqFreqNewsPriv.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###NEWSPAPER: HOW MANY DAYS A WEEK
str(main_df_desc$freqNewsp)
summary(main_df_desc$freqNewsp)

#Häufigkeiten:
tableFreqNewsp <- with(main_df_desc, table(main_df_desc$freqNewsp))
percfreqTabFreqNewsp <- round(prop.table(tableFreqNewsp)*100, 2)
cumFreqTabFreqNewsp <- cumsum(percfreqTabFreqNewsp)
freqTabFreqNewsp <- cbind(tableFreqNewsp, percfreqTabFreqNewsp, cumFreqTabFreqNewsp)
remove(tableFreqNewsp, percfreqTabFreqNewsp, cumFreqTabFreqNewsp)
colnames(freqTabFreqNewsp)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabFreqNewsp

#Maße der zentralen Tendenz:
main_df_desc$freqNewsp %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqNewsp %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(freqNewsp)) + 
  scale_x_discrete(limits = c("NIE", "SELTENER", "AN EINEM TAG", "AN 2 TAGEN", "AN 3 TAGEN", "AN 4 TAGEN", "AN 5 TAGEN", "AN 6 TAGEN", "AN ALLEN 7 TAGEN", NA)) + 
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "Anzahl", x = "Anzahl an Tagen an denen Zeitung gelesen wird", title = "Zeitung") +
  ggsave("freqFreqNewsp.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###INTERNET: FOR INFORMATION ON POLITICS
str(main_df_desc$intPol)
summary(main_df_desc$intPol)

#Häufigkeiten:
tableIntPol <- with(main_df_desc, table(main_df_desc$intPol))
percfreqTabIntPol <- round(prop.table(tableIntPol)*100, 2)
cumFreqTabIntPol <- cumsum(percfreqTabIntPol)
freqTabIntPol <- cbind(tableIntPol, percfreqTabIntPol, cumFreqTabIntPol)
remove(tableIntPol, percfreqTabIntPol, cumFreqTabIntPol)
colnames(freqTabIntPol)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabIntPol

#Maße der zentralen Tendenz:
main_df_desc$intPol %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$intPol %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(intPol)) + 
  scale_x_discrete(limits = c("JA", "NEIN", NA)) + 
  labs(y = "Anzahl", x = "Internetnutzung für Informationen zu Politik", title = "Internetnutzung") +
  ggsave("freqIntPol.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###HOW OFTEN INTERNET FOR INFO ON POLITICS?
str(main_df_desc$freqIntPol)
summary(main_df_desc$freqIntPol)

#Häufigkeiten:
tableFreqIntPol <- with(main_df_desc, table(main_df_desc$freqIntPol))
percfreqTabFreqIntPol <- round(prop.table(tableFreqIntPol)*100, 2)
cumFreqTabFreqIntPol <- cumsum(percfreqTabFreqIntPol)
freqTabFreqIntPol <- cbind(tableFreqIntPol, percfreqTabFreqIntPol, cumFreqTabFreqIntPol)
remove(tableFreqIntPol, percfreqTabFreqIntPol, cumFreqTabFreqIntPol)
colnames(freqTabFreqIntPol)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabFreqIntPol

#Maße der zentralen Tendenz:
main_df_desc$freqIntPol %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqIntPol %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(freqIntPol)) + 
  scale_x_discrete(limits = c("TAEGLICH", "EINMAL JEDE WOCHE", "EINMAL JEDEN MONAT", "SELTENER", "NIE", NA)) + 
  labs(y = "Anzahl", x = "Häufigkeit der Internetnutzung für Informationen zu Politik", title = "Internetnutzung") +
  ggsave("freqFreqIntPol.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###TRUST: TELEVISION
str(main_df_desc$trustTv)
summary(main_df_desc$trustTv)

#Häufigkeiten:
tableTrustTv <- with(main_df_desc, table(main_df_desc$trustTv))
percfreqTabTrustTv <- round(prop.table(tableTrustTv)*100, 2)
cumFreqTabTrustTv <- cumsum(percfreqTabTrustTv)
freqTabTrustTv <- cbind(tableTrustTv, percfreqTabTrustTv, cumFreqTabTrustTv)
remove(tableTrustTv, percfreqTabTrustTv, cumFreqTabTrustTv)
colnames(freqTabTrustTv)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabTrustTv

#Maße der zentralen Tendenz:
main_df_desc$trustTv %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$trustTv %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(trustTv)) + 
  scale_x_discrete(limits = c("KEIN VERTRAUEN", "2", "3", "4", "5", "6", "GROßES VERTRAUEN", NA)) +
  labs(y = "Anzahl", x = "Vertrauen in TV", title = "Vertrauen") +
  ggsave("freqTrustTv.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###TRUST: NEWSPAPERS
str(main_df_desc$trustNewsp)
summary(main_df_desc$trustNewsp)

#Häufigkeiten:
tableTrustNewsp <- with(main_df_desc, table(main_df_desc$trustNewsp))
percfreqTabTrustNewsp <- round(prop.table(tableTrustNewsp)*100, 2)
cumFreqTabTrustNewsp <- cumsum(percfreqTabTrustNewsp)
freqTabTrustNewsp <- cbind(tableTrustNewsp, percfreqTabTrustNewsp, cumFreqTabTrustNewsp)
remove(tableTrustNewsp, percfreqTabTrustNewsp, cumFreqTabTrustNewsp)
colnames(freqTabTrustNewsp)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabTrustNewsp

#Maße der zentralen Tendenz:
main_df_desc$trustNewsp %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$trustNewsp %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(trustNewsp)) + 
  scale_x_discrete(limits = c("KEIN VERTRAUEN", "2", "3", "4", "5", "6", "GROßES VERTRAUEN", NA)) +
  labs(y = "Anzahl", x = "Vertrauen in Zeitungen", title = "Vertrauen") +
  ggsave("freqTrustNewsp.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###MEDIA: INFLUENCE ON PUBLIC OPINION
str(main_df_desc$infMedia)
summary(main_df_desc$infMedia)

#Häufigkeiten:
tableInfMedia <- with(main_df_desc, table(main_df_desc$infMedia))
percfreqTabInfMedia <- round(prop.table(tableInfMedia)*100, 2)
cumFreqTabInfMedia <- cumsum(percfreqTabInfMedia)
freqTabInfMedia <- cbind(tableInfMedia, percfreqTabInfMedia, cumFreqTabInfMedia)
remove(tableInfMedia, percfreqTabInfMedia, cumFreqTabInfMedia)
colnames(freqTabInfMedia)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabInfMedia

#Maße der zentralen Tendenz:
main_df_desc$infMedia %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$infMedia %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(infMedia)) + 
  scale_x_discrete(limits = c("ZU GERINGER EINFLUSS", "GERADE RICHTIG", "ZU GROSSER EINFLUSS", NA)) + 
  labs(y = "Anzahl", x = "Einfluss der Medien auf die öffentliche Meinung", title = "Zustimmung") +
  ggsave("freqInfMedia.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###POLITICS IS TOO COMPLICATED FOR ME
str(main_df_desc$polCompl)
summary(main_df_desc$polCompl)

#Häufigkeiten:
tablePolCompl <- with(main_df_desc, table(main_df_desc$polCompl))
percfreqTabPolCompl <- round(prop.table(tablePolCompl)*100, 2)
cumFreqTabPolCompl <- cumsum(percfreqTabPolCompl)
freqTabPolCompl <- cbind(tablePolCompl, percfreqTabPolCompl, cumFreqTabPolCompl)
remove(tablePolCompl, percfreqTabPolCompl, cumFreqTabPolCompl)
colnames(freqTabPolCompl)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabPolCompl

#Maße der zentralen Tendenz:
main_df_desc$polCompl %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$polCompl %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(polCompl)) + 
  scale_x_discrete(limits = c("STIMME VOLL ZU", "STIMME EHER ZU", "STIMME EHER NICHT ZU", "STIMME GAR NICHT ZU", NA)) + 
  labs(y = "Anzahl", x = "'Politik ist zu kompliziert'", title = "Zustimmung") +
  ggsave("freqPolCompl.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###I DO NOT KNOW MUCH ABOUT POLITICS
str(main_df_desc$knowlPol)
summary(main_df_desc$knowlPol)

#Häufigkeiten:
tableKnowlPol <- with(main_df_desc, table(main_df_desc$knowlPol))
percfreqTabKnowlPol <- round(prop.table(tableKnowlPol)*100, 2)
cumFreqTabKnowlPol <- cumsum(percfreqTabKnowlPol)
freqTabKnowlPol <- cbind(tableKnowlPol, percfreqTabKnowlPol, cumFreqTabKnowlPol)
remove(tableKnowlPol, percfreqTabKnowlPol, cumFreqTabKnowlPol)
colnames(freqTabKnowlPol)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabKnowlPol

#Maße der zentralen Tendenz:
main_df_desc$knowlPol %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$knowlPol %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(knowlPol)) + 
  scale_x_discrete(limits = c("STIMME VOLL ZU", "STIMME EHER ZU", "STIMME EHER NICHT ZU", "STIMME GAR NICHT ZU", NA)) + 
  labs(y = "Anzahl", x = "Wenig über Politik wissen", title = "Zustimmung") +
  ggsave("freqKnowlPol.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###PEOPLE SHLD STAY INFORMED ABOUT POLITICS
str(main_df_desc$shldInfPol)
summary(main_df_desc$shldInfPol)

#Häufigkeiten:
tableShldInfPol <- with(main_df_desc, table(main_df_desc$shldInfPol))
percfreqTabShldInfPol <- round(prop.table(tableShldInfPol)*100, 2)
cumFreqTabShldInfPol <- cumsum(percfreqTabShldInfPol)
freqTabShldInfPol <- cbind(tableShldInfPol, percfreqTabShldInfPol, cumFreqTabShldInfPol)
remove(tableShldInfPol, percfreqTabShldInfPol, cumFreqTabShldInfPol)
colnames(freqTabShldInfPol)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabShldInfPol

#Maße der zentralen Tendenz:
main_df_desc$shldInfPol %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$shldInfPol %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(shldInfPol)) + 
  scale_x_discrete(limits = c("STIMME VOLL ZU", "STIMME EHER ZU", "STIMME EHER NICHT ZU", "STIMME GAR NICHT ZU", NA)) + 
  labs(y = "Anzahl", x = "Informiert sein über Politik", title = "Zustimmung") +
  ggsave("freqShldInfPol.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###SELF-PLACEMENT ON LEFT-RIGHT CONTINUUM
str(main_df_desc$leftRight)
summary(main_df_desc$leftRight)

#Häufigkeiten:
tableLeftRight <- with(main_df_desc, table(main_df_desc$leftRight))
percfreqTabLeftRight <- round(prop.table(tableLeftRight)*100, 2)
cumFreqTabLeftRight <- cumsum(percfreqTabLeftRight)
freqTabLeftRight <- cbind(tableLeftRight, percfreqTabLeftRight, cumFreqTabLeftRight)
remove(tableLeftRight, percfreqTabLeftRight, cumFreqTabLeftRight)
colnames(freqTabLeftRight)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabLeftRight

#Maße der zentralen Tendenz:
main_df_desc$leftRight %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$leftRight %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(leftRight)) + 
  scale_x_discrete(limits = c("F - LINKS", "A", "M", "O", "G", "Z", "E", "Y", "I", "P - RECHTS", NA)) +
  labs(y = "Anzahl", x = "Rechts-links Selbsteinschätzung", title = "Selbsteinschätzung") +
  ggsave("freqLeftRight.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###SELF-ASSESSMENT OF SOCIAL CLASS, RESP
str(main_df_desc$class)
summary(main_df_desc$class)

#Häufigkeiten:
tableClass <- with(main_df_desc, table(main_df_desc$class))
percfreqTabClass <- round(prop.table(tableClass)*100, 2)
cumFreqTabClass <- cumsum(percfreqTabClass)
freqTabClass <- cbind(tableClass, percfreqTabClass, cumFreqTabClass)
remove(tableClass, percfreqTabClass, cumFreqTabClass)
colnames(freqTabClass)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabClass

#Maße der zentralen Tendenz:
main_df_desc$class %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$class %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(class)) + 
  scale_x_discrete(limits = c("UNTERSCHICHT", "ARBEITERSCHICHT", "MITTELSCHICHT", "OBERE MITTELSCHICHT", "OBERSCHICHT", NA)) + 
  labs(y = "Anzahl", x = "Schicht Selbsteinschätzung", title = "Selbsteinschätzung") +
  ggsave("freqClass.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESPONDENT: SEX
str(main_df_desc$sex)
summary(main_df_desc$sex)

#Häufigkeiten:
tableSex <- with(main_df_desc, table(main_df_desc$sex))
percfreqTabSex <- round(prop.table(tableSex)*100, 2)
cumFreqTabSex <- cumsum(percfreqTabSex)
freqTabSex <- cbind(tableSex, percfreqTabSex, cumFreqTabSex)
remove(tableSex, percfreqTabSex, cumFreqTabSex)
colnames(freqTabSex)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabSex

#Maße der zentralen Tendenz:
main_df_desc$sex %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$sex %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(sex)) + 
  scale_x_discrete(limits = c("MANN", "FRAU")) + 
  labs(y = "Anzahl", x = "Geschlecht der Befragten", title = "Geschlecht") +
  ggsave("freqSex.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESPONDENT: AGE, CATEGORIZED
str(main_df_desc$agec)
summary(main_df_desc$agec)

#Häufigkeiten:
tableAgec <- with(main_df_desc, table(main_df_desc$agec))
percfreqTabAgec <- round(prop.table(tableAgec)*100, 2)
cumFreqTabAgec <- cumsum(percfreqTabAgec)
freqTabAgec <- cbind(tableAgec, percfreqTabAgec, cumFreqTabAgec)
remove(tableAgec, percfreqTabAgec, cumFreqTabAgec)
colnames(freqTabAgec)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabAgec

#Maße der zentralen Tendenz:
main_df_desc$agec %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$agec %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(agec)) + 
  scale_x_discrete(limits = c("18-29 JAHRE", "30-44 JAHRE", "45-59 JAHRE", "60-74 JAHRE", "75-89 JAHRE", "UEBER 89 JAHRE", NA)) + 
  labs(y = "Anzahl", x = "Alter der Befragten", title = "Alter") +
  ggsave("freqAgec.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESP.: GENERAL SCHOOL LEAVING CERTIFICATE
str(main_df_desc$educ)
summary(main_df_desc$educ)

#Häufigkeiten:
tableEduc <- with(main_df_desc, table(main_df_desc$educ))
percfreqTabEduc <- round(prop.table(tableEduc)*100, 2)
cumFreqTabEduc <- cumsum(percfreqTabEduc)
freqTabEduc <- cbind(tableEduc, percfreqTabEduc, cumFreqTabEduc)
remove(tableEduc, percfreqTabEduc, cumFreqTabEduc)
colnames(freqTabEduc)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabEduc

#Maße der zentralen Tendenz:
main_df_desc$educ %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$educ %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(educ)) + 
  scale_x_discrete(limits = c("OHNE ABSCHLUSS", "VOLKS-,HAUPTSCHULE", "MITTLERE REIFE", "FACHHOCHSCHULREIFE", "HOCHSCHULREIFE", "ANDERER ABSCHLUSS", "NOCH SCHUELER", NA)) + 
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "Anzahl", x = "Schulabschluss der Befragten", title = "Abschluss") +
  ggsave("freqEduc.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESP.: UNIVERSITY DEGREE
str(main_df_desc$uniDeg)
summary(main_df_desc$uniDeg)

#Häufigkeiten:
tableUniDeg <- with(main_df_desc, table(main_df_desc$uniDeg))
percfreqTabUniDeg <- round(prop.table(tableUniDeg)*100, 2)
cumFreqTabUniDeg <- cumsum(percfreqTabUniDeg)
freqTabUniDeg <- cbind(tableUniDeg, percfreqTabUniDeg, cumFreqTabUniDeg)
remove(tableUniDeg, percfreqTabUniDeg, cumFreqTabUniDeg)
colnames(freqTabUniDeg)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabUniDeg

#Maße der zentralen Tendenz:
main_df_desc$uniDeg %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$uniDeg %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(uniDeg)) + 
  scale_x_discrete(limits = c("NICHT GENANNT", "GENANNT", NA)) + 
  labs(y = "Anzahl", x = "Angabe zu Uniabschluss", title = "Abschluss") +
  ggsave("freqUniDeg.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESP.: TYPE OF UNIVERSITY DEGREE
str(main_df_desc$typeUniDeg)
summary(main_df_desc$typeUniDeg)

#Häufigkeiten:
tableTypeUniDeg <- with(main_df_desc, table(main_df_desc$typeUniDeg))
percfreqTabTypeUniDeg <- round(prop.table(tableTypeUniDeg)*100, 2)
cumFreqTabTypeUniDeg <- cumsum(percfreqTabTypeUniDeg)
freqTabTypeUniDeg <- cbind(tableTypeUniDeg, percfreqTabTypeUniDeg, cumFreqTabTypeUniDeg)
remove(tableTypeUniDeg, percfreqTabTypeUniDeg, cumFreqTabTypeUniDeg)
colnames(freqTabTypeUniDeg)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabTypeUniDeg

#Maße der zentralen Tendenz:
main_df_desc$typeUniDeg %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$typeUniDeg %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(typeUniDeg)) + 
  scale_x_discrete(limits = c("BACHELOR", "MASTER", "DIPLOM", "MAGISTER", "STAATSEXAMEN", "PROMOTION", "SONSTIGES", NA)) + 
  labs(y = "Anzahl", x = "Art des Uniabschluss", title = "Abschluss") +
  ggsave("freqTypeUniDeg.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESPONDENT: CURRENT EMPLOYMENT STATUS
str(main_df_desc$work)
summary(main_df_desc$work)

#Häufigkeiten:
tableWork <- with(main_df_desc, table(main_df_desc$work))
percfreqTabWork <- round(prop.table(tableWork)*100, 2)
cumFreqTabWork <- cumsum(percfreqTabWork)
freqTabWork <- cbind(tableWork, percfreqTabWork, cumFreqTabWork)
remove(tableWork, percfreqTabWork, cumFreqTabWork)
colnames(freqTabWork)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabWork

#Maße der zentralen Tendenz:
main_df_desc$work %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$work %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(work)) + 
  scale_x_discrete(limits = c("HAUPTBERUFL.GANZTAGS", "HAUPTBERUFL.HALBTAGS", "NEBENHER BERUFSTAE.", "NICHT ERWERBSTAETIG")) + 
  labs(y = "Anzahl", x = "Art der Erwerbstätigkeit", title = "Erwerbstätigkeit") +
  ggsave("freqWork.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESPONDENT: CURRENT OCCUPATION
str(main_df_desc$currOcc)
summary(main_df_desc$currOcc)

#Häufigkeiten:
tableCurrOcc <- with(main_df_desc, table(main_df_desc$currOcc))
percfreqTabCurrOcc <- round(prop.table(tableCurrOcc)*100, 2)
cumFreqTabCurrOcc <- cumsum(percfreqTabCurrOcc)
freqTabCurrOcc <- cbind(tableCurrOcc, percfreqTabCurrOcc, cumFreqTabCurrOcc)
remove(tableCurrOcc, percfreqTabCurrOcc, cumFreqTabCurrOcc)
colnames(freqTabCurrOcc)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabCurrOcc

#Maße der zentralen Tendenz:
main_df_desc$currOcc %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$currOcc %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(currOcc)) + 
  scale_x_discrete(limits = c("LANDWIRT", "AKADEM.FREIER BERUF", "SONST.SELBSTAENDIGE", "BEAMT,RICHTER,SOLDAT", "ANGESTELLTER", "ARBEITER", "IN AUSBILDUNG", "MITHELF.FAMILIENANG.", NA)) + 
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "Anzahl", x = "Derzeitige Erwerbstätigkeit", title = "Erwerbstätigkeit") +
  ggsave("freqCurrOcc.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESP.: SUPERVISING THE WORK OF OTHERS?
str(main_df_desc$subvOther)
summary(main_df_desc$subvOther)

#Häufigkeiten:
tableSubvOther <- with(main_df_desc, table(main_df_desc$subvOther))
percfreqTabSubvOther <- round(prop.table(tableSubvOther)*100, 2)
cumFreqTabSubvOther <- cumsum(percfreqTabSubvOther)
freqTabSubvOther <- cbind(tableSubvOther, percfreqTabSubvOther, cumFreqTabSubvOther)
remove(tableSubvOther, percfreqTabSubvOther, cumFreqTabSubvOther)
colnames(freqTabSubvOther)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabSubvOther

#Maße der zentralen Tendenz:
main_df_desc$subvOther %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$subvOther %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(subvOther)) + 
  scale_x_discrete(limits = c("JA", "NEIN", NA)) + 
  labs(y = "Anzahl", x = "Beaufsichtigung von Angestellten", title = "Erwerbstätigkeit") +
  ggsave("freqSubvOther.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESP.: UNEMPLOYMENT IN THE LAST 10 YEARS
str(main_df_desc$unempl)
summary(main_df_desc$unempl)

#Häufigkeiten:
tableUnempl <- with(main_df_desc, table(main_df_desc$unempl))
percfreqTabUnempl <- round(prop.table(tableUnempl)*100, 2)
cumFreqTabUnempl <- cumsum(percfreqTabUnempl)
freqTabUnempl <- cbind(tableUnempl, percfreqTabUnempl, cumFreqTabUnempl)
remove(tableUnempl, percfreqTabUnempl, cumFreqTabUnempl)
colnames(freqTabUnempl)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabUnempl

#Maße der zentralen Tendenz:
main_df_desc$unempl %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$unempl %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(unempl)) + 
  scale_x_discrete(limits = c("JA", "NEIN", NA)) + 
  labs(y = "Anzahl", x = "Arbeitslosigkeit in den letzten 10 Jahren", title = "Arbeitslosigkeit") +
  ggsave("freqUnempl.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###RESP.: MONTHLY NET INC.-OPEN+CLOSED, CAT.
str(main_df_desc$incc)
summary(main_df_desc$incc)

#Häufigkeiten:
tableIncc <- with(main_df_desc, table(main_df_desc$incc))
percfreqTabIncc <- round(prop.table(tableIncc)*100, 2)
cumFreqTabIncc <- cumsum(percfreqTabIncc)
freqTabIncc <- cbind(tableIncc, percfreqTabIncc, cumFreqTabIncc)
remove(tableIncc, percfreqTabIncc, cumFreqTabIncc)
colnames(freqTabIncc)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabIncc

#Maße der zentralen Tendenz:
main_df_desc$incc %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$incc %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_histogram(aes(incc), stat = "count") + 
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "Anzahl", x = "Monatliches Nettoeinkommen des Befragten", title = "Einkommen") +
  ggsave("freqIncc.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###HH.: MONTHLY NET INC.-OPEN+CLOSED, CAT.
str(main_df_desc$hhincc)
summary(main_df_desc$hhincc)

#Häufigkeiten:
tableHhincc <- with(main_df_desc, table(main_df_desc$hhincc))
percfreqTabHhincc <- round(prop.table(tableHhincc)*100, 2)
cumFreqTabHhincc <- cumsum(percfreqTabHhincc)
freqTabHhincc <- cbind(tableHhincc, percfreqTabHhincc, cumFreqTabHhincc)
remove(tableHhincc, percfreqTabHhincc, cumFreqTabHhincc)
colnames(freqTabHhincc)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabHhincc

#Maße der zentralen Tendenz:
main_df_desc$hhincc %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$hhincc %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_histogram(aes(hhincc), stat = "count") + 
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "Anzahl", x = "Monatliches Nettoeinkommen pro Haushalt", title = "Einkommen") +
  ggsave("freqHhincc.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###HOW OFTEN TALK ABOUT POLITICS: IN FAMILY
str(main_df_desc$freqPolFam)
summary(main_df_desc$freqPolFam)

#Häufigkeiten:
tableFreqPolFam <- with(main_df_desc, table(main_df_desc$freqPolFam))
percfreqTabFreqPolFam <- round(prop.table(tableFreqPolFam)*100, 2)
cumFreqTabFreqPolFam <- cumsum(percfreqTabFreqPolFam)
freqTabFreqPolFam <- cbind(tableFreqPolFam, percfreqTabFreqPolFam, cumFreqTabFreqPolFam)
remove(tableFreqPolFam, percfreqTabFreqPolFam, cumFreqTabFreqPolFam)
colnames(freqTabFreqPolFam)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabFreqPolFam

#Maße der zentralen Tendenz:
main_df_desc$freqPolFam %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqPolFam %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(freqPolFam)) + 
  scale_x_discrete(limits = c("SEHR OFT", "OFT", "MANCHMAL", "SELTEN", "NIE", NA)) + 
  labs(y = "Anzahl", x = "Austausch über Politik in der Familie", title = "Austausch über Politik") +
  ggsave("freqFreqPolFam.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###HOW OFTEN TALK ABOUT POLITICS: FRIENDS
str(main_df_desc$freqPolFre)
summary(main_df_desc$freqPolFre)

#Häufigkeiten:
tableFreqPolFre <- with(main_df_desc, table(main_df_desc$freqPolFre))
percfreqTabFreqPolFre <- round(prop.table(tableFreqPolFre)*100, 2)
cumFreqTabFreqPolFre <- cumsum(percfreqTabFreqPolFre)
freqTabFreqPolFre <- cbind(tableFreqPolFre, percfreqTabFreqPolFre, cumFreqTabFreqPolFre)
remove(tableFreqPolFre, percfreqTabFreqPolFre, cumFreqTabFreqPolFre)
colnames(freqTabFreqPolFre)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabFreqPolFre

#Maße der zentralen Tendenz:
main_df_desc$freqPolFre %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqPolFre %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(freqPolFre)) + 
  scale_x_discrete(limits = c("SEHR OFT", "OFT", "MANCHMAL", "SELTEN", "NIE", NA)) + 
  labs(y = "Anzahl", x = "Austausch über Politik im Freundeskreis", title = "Austausch über Politik") +
  ggsave("freqFreqPolFre.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###SELF-ASSESSMENT OF PLACE WHERE R LIVES
str(main_df_desc$placeLive)
summary(main_df_desc$placeLive)

#Häufigkeiten:
tablePlaceLive <- with(main_df_desc, table(main_df_desc$placeLive))
percfreqTabPlaceLive <- round(prop.table(tablePlaceLive)*100, 2)
cumFreqTabPlaceLive <- cumsum(percfreqTabPlaceLive)
freqTabPlaceLive <- cbind(tablePlaceLive, percfreqTabPlaceLive, cumFreqTabPlaceLive)
remove(tablePlaceLive, percfreqTabPlaceLive, cumFreqTabPlaceLive)
colnames(freqTabPlaceLive)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabPlaceLive

#Maße der zentralen Tendenz:
main_df_desc$placeLive %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$placeLive %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(placeLive)) + 
  scale_x_discrete(limits = c("GROSSSTADT", "VORORT GROSSSTADT", "MITTEL-, KLEINSTADT", "LAENDL. DORF", "EINZELHAUS, LAND")) + 
    labs(y = "Anzahl", x = "Wohnumgebung des Befragten", title = "Wohnumgebung") +
  ggsave("freqPlaceLive.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###DID YOU VOTE IN LAST FEDERAL ELECTION?
str(main_df_desc$vote)
summary(main_df_desc$vote)

#Häufigkeiten:
tableVote <- with(main_df_desc, table(main_df_desc$vote))
percfreqTabVote <- round(prop.table(tableVote)*100, 2)
cumFreqTabVote <- cumsum(percfreqTabVote)
freqTabVote <- cbind(tableVote, percfreqTabVote, cumFreqTabVote)
remove(tableVote, percfreqTabVote, cumFreqTabVote)
colnames(freqTabVote)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabVote

#Maße der zentralen Tendenz:
main_df_desc$vote %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$vote %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(vote)) + 
  scale_x_discrete(limits = c("JA", "NEIN", NA)) + 
  labs(y = "Anzahl", x = "Wahlbeteiligung bei der letzten Bundestagswahl", title = "Bundestagswahl") +
  ggsave("freqVote.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###PARTY VOTE IN LAST FEDERAL ELECTION
str(main_df_desc$partyVote)
summary(main_df_desc$partyVote)

#Häufigkeiten:
tablePartyVote <- with(main_df_desc, table(main_df_desc$partyVote))
percfreqTabPartyVote <- round(prop.table(tablePartyVote)*100, 2)
cumFreqTabPartyVote <- cumsum(percfreqTabPartyVote)
freqTabPartyVote <- cbind(tablePartyVote, percfreqTabPartyVote, cumFreqTabPartyVote)
remove(tablePartyVote, percfreqTabPartyVote, cumFreqTabPartyVote)
colnames(freqTabPartyVote)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabPartyVote

#Maße der zentralen Tendenz:
main_df_desc$partyVote %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$partyVote %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(partyVote)) + 
  scale_x_discrete(limits = c("CDU-CSU", "SPD", "FDP", "DIE GRUENEN", "DIE LINKE", "AFD", "ANDERE PARTEI", NA)) + 
  labs(y = "Anzahl", x = "Wahlentscheidung bei der letzten Bundestagswahl", title = "Bundestagswahl") +
  ggsave("freqPartyVote.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###INT.: SOCIAL CLASS OF HOUSEHOLD
str(main_df_desc$intClassH)
summary(main_df_desc$intClassH)

#Häufigkeiten:
tableIntClassH <- with(main_df_desc, table(main_df_desc$intClassH))
percfreqTabIntClassH <- round(prop.table(tableIntClassH)*100, 2)
cumFreqTabIntClassH <- cumsum(percfreqTabIntClassH)
freqTabIntClassH <- cbind(tableIntClassH, percfreqTabIntClassH, cumFreqTabIntClassH)
remove(tableIntClassH, percfreqTabIntClassH, cumFreqTabIntClassH)
colnames(freqTabIntClassH)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabIntClassH

#Maße der zentralen Tendenz:
main_df_desc$intClassH %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$intClassH %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(intClassH)) + 
  scale_x_discrete(limits = c("UNTERSCHICHT", "ARBEITERSCHICHT", "MITTELSCHICHT", "OBERE MITTELSCHICHT", "OBERSCHICHT", "NICHT ERKENNBAR")) + 
  labs(y = "Anzahl", x = "Schichteinstufung des Haushalts durch den Interviewer", title = "Schichteinstufung") +
  ggsave("freqIntClassH.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


###FEDERAL STATE THAT RESPONDENT LIVES IN
str(main_df_desc$land)
summary(main_df_desc$land)

#Häufigkeiten:
tableLand<- with(main_df_desc, table(main_df_desc$land))
percfreqTabLand <- round(prop.table(tableLand)*100, 2)
cumFreqTabLand <- cumsum(percfreqTabLand)
freqTabLand <- cbind(tableLand, percfreqTabLand, cumFreqTabLand)
remove(tableLand, percfreqTabLand, cumFreqTabLand)
colnames(freqTabLand)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
freqTabLand

#Maße der zentralen Tendenz:
main_df_desc$land %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$land %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(land)) + 
  scale_x_discrete(limits = c("SCHLESWIG-HOLSTEIN", "HAMBURG", "NIEDERSACHSEN", "BREMEN", "NORDRHEIN-WESTFALEN", "HESSEN", "RHEINLAND-PFALZ", "BADEN-WUERTTEMBERG", "BAYERN", "SAARLAND", "EHEM. BERLIN-WEST", "EHEM. BERLIN-OST", "BRANDENBURG", "MECKLENB.-VORPOMMERN", "SACHSEN", "SACHSEN-ANHALT", "THUERINGEN"), labels = c("SCHLESWIG-\nHOLSTEIN", "HAMBURG", "NIEDERSACHSEN", "BREMEN", "NORDRHEIN-\nWESTFALEN", "HESSEN", "RHEINLAND-\nPFALZ", "BADEN-\nWUERTTEMBERG", "BAYERN", "SAARLAND", "EHEM. \nBERLIN-WEST", "EHEM. \nBERLIN-OST", "BRANDENBURG", "MECKLENBURG-\nVORPOMMERN", "SACHSEN", "SACHSEN-\nANHALT", "THUERINGEN")) + 
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(y = "Anzahl", x = "Bundesland des Befragten", title = "Bundesland") +
  ggsave("freqLand.uni.pdf", path = "./Seminararbeit/Bericht/Graphiken", width = 10, height = 10)


#####Ende