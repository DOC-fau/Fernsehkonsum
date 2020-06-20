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


#####Univariate Analyse:
load("main_df.RData")
main_df_desc <- main_df
main_df_desc <- droplevels(main_df_desc)


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
# hhincc: HH.: MONTHLY NET INC.-OPEN+CLOSED, CAT.
# freqPolFam: HOW OFTEN TALK ABOUT POLITICS: IN FAMILY
# freqPolFre: HOW OFTEN TALK ABOUT POLITICS: FRIENDS
# placeLive: SELF-ASSESSMENT OF PLACE WHERE R LIVES
# vote: DID YOU VOTE IN LAST FEDERAL ELECTION?
# partyVote: PARTY VOTE IN LAST FEDERAL ELECTION
# intClassH: INT.: SOCIAL CLASS OF HOUSEHOLD
# land: FEDERAL STATE THAT RESPONDENT LIVES IN

# wghtpew: WEIGHT: EAST-WEST, PERSON-LEVEL


###TV: WATCH NEWS ON PUBLIC CHANNELS?
str(main_df_desc$ardZdf)

#Häufigkeiten:
tableArdZdf <- with(main_df_desc, table(main_df_desc$ardZdf))
percfreqTabArdZdf <- round(prop.table(tableArdZdf)*100, 2)
cumFreqTabArdZdf <- cumsum(percfreqTabArdZdf)
freqTabArdZdf <- rbind(tableArdZdf, percfreqTabArdZdf, cumFreqTabArdZdf)
remove(tableArdZdf, percfreqTabArdZdf, cumFreqTabArdZdf)
rownames(freqTabArdZdf)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabArdZdf <- t(freqTabArdZdf))

#Maße der zentralen Tendenz:
main_df_desc$ardZdf %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$ardZdf %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:

set_here(path = "path/to/directory")

ggplot(main_df_desc) + 
  geom_bar(aes(ardZdf)) + 
  scale_x_discrete(limits = c("JA", "NEIN")) + 
  labs(y = "Anzahl", x = "Öffentliches TV schauen", title = "Öffentliches TV") +
  ggsave("freqArdZdf.pdf", path = here("Seminararbeit/Graphiken"), width = 10, height = 10)

here()


###REGION OF INTERVIEW: WEST - EAST
str(main_df_desc$eastwest)

#Häufigkeiten:
tableEastWest <- with(main_df_desc, table(main_df_desc$eastwest))
percfreqTabEastWest <- round(prop.table(tableEastWest)*100, 2)
cumFreqTabEastWest <- cumsum(percfreqTabEastWest)
freqTabEastWest <- rbind(tableEastWest, percfreqTabEastWest, cumFreqTabEastWest)
remove(tableEastWest, percfreqTabEastWest, cumFreqTabEastWest)
rownames(freqTabEastWest)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabEastWest <- t(freqTabEastWest))

#Maße der zentralen Tendenz:
main_df_desc$eastwest %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$eastwest %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(eastwest)) + 
  scale_x_discrete(limits = c("ALTE BUNDESLAENDER", "NEUE BUNDESLAENDER")) + 
  labs(y = "Anzahl", x = "Osten oder Westen", title = "Wohngebiet") +
  ggsave("freqEastWest.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###GERMAN CITIZENSHIP?
str(main_df_desc$german)

#Häufigkeiten:
tableGerman <- with(main_df_desc, table(main_df_desc$german))
percfreqTabGerman <- round(prop.table(tableGerman)*100, 2)
cumFreqTabGerman <- cumsum(percfreqTabGerman)
freqTabGerman <- rbind(tableGerman, percfreqTabGerman, cumFreqTabGerman)
remove(tableGerman, percfreqTabGerman, cumFreqTabGerman)
rownames(freqTabGerman)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabGerman <- t(freqTabGerman))

#Maße der zentralen Tendenz:
main_df_desc$german %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$german %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(german)) + 
  scale_x_discrete(limits = c("JA", "JA,NEBEN ZWEITER", "NEIN")) + 
  labs(y = "Anzahl", x = "Deutsche Staatsangehörigkeit", title = "Staatsangehörigkeit") +
  ggsave("freqGerman.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###RESP. OWN CURRENT FINANCIAL SITUATION
str(main_df_desc$finSit)

#Häufigkeiten:
tableFinSit <- with(main_df_desc, table(main_df_desc$finSit))
percfreqTabFinSit <- round(prop.table(tableFinSit)*100, 2)
cumFreqTabFinSit <- cumsum(percfreqTabFinSit)
freqTabFinSit <- rbind(tableFinSit, percfreqTabFinSit, cumFreqTabFinSit)
remove(tableFinSit, percfreqTabFinSit, cumFreqTabFinSit)
rownames(freqTabFinSit)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabFinSit <- t(freqTabFinSit))

#Maße der zentralen Tendenz:
main_df_desc$finSit %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$finSit %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(finSit)) + 
  scale_x_discrete(limits = c("SEHR GUT", "GUT", "TEILS TEILS", "SCHLECHT", "SEHR SCHLECHT")) + 
  labs(y = "Anzahl", x = "Einschätzung der finanziellen Situation", title = "Finanz. Situation") +
  ggsave("freqFinSit.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###WATCH TV: HOW MANY DAYS A WEEK
str(main_df_desc$tvWeek)

#Häufigkeiten:
tableTvWeek <- with(main_df_desc, table(main_df_desc$tvWeek))
percfreqTabTvWeek <- round(prop.table(tableTvWeek)*100, 2)
cumFreqTabTvWeek <- cumsum(percfreqTabTvWeek)
freqTabTvWeek <- rbind(tableTvWeek, percfreqTabTvWeek, cumFreqTabTvWeek)
remove(tableTvWeek, percfreqTabTvWeek, cumFreqTabTvWeek)
rownames(freqTabTvWeek)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabTvWeek <- t(freqTabTvWeek))

#Maße der zentralen Tendenz:
main_df_desc$tvWeek %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$tvWeek %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_histogram(aes(tvWeek), stat = "count") + 
  labs(y = "Anzahl", x = "Anzahl an Tagen an denen TV geschaut wird", title = "TV pro Woche") +
  ggsave("freqTvWeek.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###TV: HOW OFTEN NEWS ON PUBLIC CHANNELS?
str(main_df_desc$freqNewsPubl)

#Häufigkeiten:
tableFreqNewsPubl <- with(main_df_desc, table(main_df_desc$freqNewsPubl))
percfreqTabFreqNewsPubl <- round(prop.table(tableFreqNewsPubl)*100, 2)
cumFreqTabFreqNewsPubl <- cumsum(percfreqTabFreqNewsPubl)
freqTabFreqNewsPubl <- rbind(tableFreqNewsPubl, percfreqTabFreqNewsPubl, cumFreqTabFreqNewsPubl)
remove(tableFreqNewsPubl, percfreqTabFreqNewsPubl, cumFreqTabFreqNewsPubl)
rownames(freqTabFreqNewsPubl)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabFreqNewsPubl <- t(freqTabFreqNewsPubl))

#Maße der zentralen Tendenz:
main_df_desc$freqNewsPubl %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqNewsPubl %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_histogram(aes(freqNewsPubl), stat = "count") + 
  labs(y = "Anzahl", x = "Anzahl an Tagen an denen TV geschaut wird", title = "TV pro Woche") +
  ggsave("freqFreqNewsPubl.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###TV: WATCH NEWS ON PRIVATE CHANNELS?
str(main_df_desc$newsPriv)

#Häufigkeiten:
tableNewsPriv <- with(main_df_desc, table(main_df_desc$newsPriv))
percfreqTabNewsPriv <- round(prop.table(tableNewsPriv)*100, 2)
cumFreqTabNewsPriv <- cumsum(percfreqTabNewsPriv)
freqTabNewsPriv <- rbind(tableNewsPriv, percfreqTabNewsPriv, cumFreqTabNewsPriv)
remove(tableNewsPriv, percfreqTabNewsPriv, cumFreqTabNewsPriv)
rownames(freqTabNewsPriv)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabNewsPriv <- t(freqTabNewsPriv))

#Maße der zentralen Tendenz:
main_df_desc$newsPriv %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$newsPriv %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(newsPriv)) + 
  scale_x_discrete(limits = c("JA", "NEIN")) + 
  labs(y = "Anzahl", x = "Nachrichten schauen auf Privatkanälen", title = "Nachrichten") +
  ggsave("freqNewsPriv.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)

###TV: HOW OFTEN NEWS ON PRIVATE CHANNELS?
str(main_df_desc$freqNewsPriv)

#Häufigkeiten:
tableFreqNewsPriv <- with(main_df_desc, table(main_df_desc$freqNewsPriv))
percfreqTabFreqNewsPriv <- round(prop.table(tableFreqNewsPriv)*100, 2)
cumFreqTabFreqNewsPriv <- cumsum(percfreqTabFreqNewsPriv)
freqTabFreqNewsPriv <- rbind(tableFreqNewsPriv, percfreqTabFreqNewsPriv, cumFreqTabFreqNewsPriv)
remove(tableFreqNewsPriv, percfreqTabFreqNewsPriv, cumFreqTabFreqNewsPriv)
rownames(freqTabFreqNewsPriv)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabFreqNewsPriv <- t(freqTabFreqNewsPriv))

#Maße der zentralen Tendenz:
main_df_desc$freqNewsPriv %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqNewsPriv %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_histogram(aes(freqNewsPriv), stat = "count") + 
  labs(y = "Anzahl", x = "Anzahl an Tagen an denen Nachrichten auf Privatkanälen geschaut wird", title = "Nachrichten auf Privatikanälen pro Woche") +
  ggsave("freqFreqNewsPriv.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###NEWSPAPER: HOW MANY DAYS A WEEK
str(main_df_desc$freqNewsp)

#Häufigkeiten:
tableFreqNewsp <- with(main_df_desc, table(main_df_desc$freqNewsp))
percfreqTabFreqNewsp <- round(prop.table(tableFreqNewsp)*100, 2)
cumFreqTabFreqNewsp <- cumsum(percfreqTabFreqNewsp)
freqTabFreqNewsp <- rbind(tableFreqNewsp, percfreqTabFreqNewsp, cumFreqTabFreqNewsp)
remove(tableFreqNewsp, percfreqTabFreqNewsp, cumFreqTabFreqNewsp)
rownames(freqTabFreqNewsp)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabFreqNewsp <- t(freqTabFreqNewsp))

#Maße der zentralen Tendenz:
main_df_desc$freqNewsp %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqNewsp %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_histogram(aes(freqNewsp), stat = "count") + 
  labs(y = "Anzahl", x = "Zeitung lesen", title = "Zeitung") +
  ggsave("freqFreqNewsp.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###INTERNET: FOR INFORMATION ON POLITICS
str(main_df_desc$intPol)

#Häufigkeiten:
tableIntPol <- with(main_df_desc, table(main_df_desc$intPol))
percfreqTabIntPol <- round(prop.table(tableIntPol)*100, 2)
cumFreqTabIntPol <- cumsum(percfreqTabIntPol)
freqTabIntPol <- rbind(tableIntPol, percfreqTabIntPol, cumFreqTabIntPol)
remove(tableIntPol, percfreqTabIntPol, cumFreqTabIntPol)
rownames(freqTabIntPol)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabIntPol <- t(freqTabIntPol))

#Maße der zentralen Tendenz:
main_df_desc$intPol %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$intPol %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(intPol)) + 
  scale_x_discrete(limits = c("JA", "NEIN")) + 
  labs(y = "Anzahl", x = "Internetnutzung für Informationen zu Politik", title = "Internetnutzung") +
  ggsave("freqIntPol.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###HOW OFTEN INTERNET FOR INFO ON POLITICS?
str(main_df_desc$freqIntPol)

#Häufigkeiten:
tableFreqIntPol <- with(main_df_desc, table(main_df_desc$freqIntPol))
percfreqTabFreqIntPol <- round(prop.table(tableFreqIntPol)*100, 2)
cumFreqTabFreqIntPol <- cumsum(percfreqTabFreqIntPol)
freqTabFreqIntPol <- rbind(tableFreqIntPol, percfreqTabFreqIntPol, cumFreqTabFreqIntPol)
remove(tableFreqIntPol, percfreqTabFreqIntPol, cumFreqTabFreqIntPol)
rownames(freqTabFreqIntPol)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabFreqIntPol <- t(freqTabFreqIntPol))

#Maße der zentralen Tendenz:
main_df_desc$freqIntPol %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqIntPol %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(freqIntPol)) + 
  scale_x_discrete(limits = c("TAEGLICH", "EINMAL JEDE WOCHE", "EINMAL JEDEN MONAT", "SELTENER", "NIE")) + 
  labs(y = "Anzahl", x = "Häufigkeit der Internetnutzung für Informationen zu Politik", title = "Häufigkeit der Internetnutzung") +
  ggsave("freqFreqIntPol.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###TRUST: TELEVISION
str(main_df_desc$trustTv)
levels(main_df_desc$trustTv) <- c("GAR KEIN VERTRAUEN", "2", "3", "4", "5", "6", "GROSSES VERTRAUEN")

#Häufigkeiten:
tableTrustTv <- with(main_df_desc, table(main_df_desc$trustTv))
percfreqTabTrustTv <- round(prop.table(tableTrustTv)*100, 2)
cumFreqTabTrustTv <- cumsum(percfreqTabTrustTv)
freqTabTrustTv <- rbind(tableTrustTv, percfreqTabTrustTv, cumFreqTabTrustTv)
remove(tableTrustTv, percfreqTabTrustTv, cumFreqTabTrustTv)
rownames(freqTabTrustTv)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabTrustTv <- t(freqTabTrustTv))

#Maße der zentralen Tendenz:
main_df_desc$trustTv %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$trustTv %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_histogram(aes(trustTv), stat = "count") + 
  labs(y = "Anzahl", x = "Vertrauen in TV", title = "Vertrauen") +
  ggsave("freqTrustTv.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###TRUST: NEWSPAPERS
str(main_df_desc$trustNewsp)
levels(main_df_desc$trustNewsp) <- c("GAR KEIN VERTRAUEN", "2", "3", "4", "5", "6", "GROSSES VERTRAUEN")

#Häufigkeiten:
tableTrustNewsp <- with(main_df_desc, table(main_df_desc$trustNewsp))
percfreqTabTrustNewsp <- round(prop.table(tableTrustNewsp)*100, 2)
cumFreqTabTrustNewsp <- cumsum(percfreqTabTrustNewsp)
freqTabTrustNewsp <- rbind(tableTrustNewsp, percfreqTabTrustNewsp, cumFreqTabTrustNewsp)
remove(tableTrustNewsp, percfreqTabTrustNewsp, cumFreqTabTrustNewsp)
rownames(freqTabTrustNewsp)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabTrustNewsp <- t(freqTabTrustNewsp))

#Maße der zentralen Tendenz:
main_df_desc$trustNewsp %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$trustNewsp %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_histogram(aes(trustNewsp), stat = "count") + 
  labs(y = "Anzahl", x = "Vertrauen in Zeitungen", title = "Vertrauen") +
  ggsave("freqTrustNewsp.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###MEDIA: INFLUENCE ON PUBLIC OPINION
str(main_df_desc$infMedia)

#Häufigkeiten:
tableInfMedia <- with(main_df_desc, table(main_df_desc$infMedia))
percfreqTabInfMedia <- round(prop.table(tableInfMedia)*100, 2)
cumFreqTabInfMedia <- cumsum(percfreqTabInfMedia)
freqTabInfMedia <- rbind(tableInfMedia, percfreqTabInfMedia, cumFreqTabInfMedia)
remove(tableInfMedia, percfreqTabInfMedia, cumFreqTabInfMedia)
rownames(freqTabInfMedia)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabInfMedia <- t(freqTabInfMedia))

#Maße der zentralen Tendenz:
main_df_desc$infMedia %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$infMedia %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(infMedia)) + 
  scale_x_discrete(limits = c("ZU GERINGER EINFLUSS", "GERADE RICHTIG", "ZU GROSSER EINFLUSS")) + 
  labs(y = "Anzahl", x = "Einfluss der Medien auf die öffentliche Meinung", title = "Einfluss der Medien") +
  ggsave("freqInfMedia.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###POLITICS IS TOO COMPLICATED FOR ME
str(main_df_desc$polCompl)

#Häufigkeiten:
tablePolCompl <- with(main_df_desc, table(main_df_desc$polCompl))
percfreqTabPolCompl <- round(prop.table(tablePolCompl)*100, 2)
cumFreqTabPolCompl <- cumsum(percfreqTabPolCompl)
freqTabPolCompl <- rbind(tablePolCompl, percfreqTabPolCompl, cumFreqTabPolCompl)
remove(tablePolCompl, percfreqTabPolCompl, cumFreqTabPolCompl)
rownames(freqTabPolCompl)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabPolCompl <- t(freqTabPolCompl))

#Maße der zentralen Tendenz:
main_df_desc$polCompl %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$polCompl %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(polCompl)) + 
  scale_x_discrete(limits = c("STIMME VOLL ZU", "STIMME EHER ZU", "STIMME EHER NICHT ZU", "STIMME GAR NICHT ZU")) + 
  labs(y = "Anzahl", x = "'Politik ist zu kompliziert'", title = "Zustimmung") +
  ggsave("freqPolCompl.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###I DO NOT KNOW MUCH ABOUT POLITICS
str(main_df_desc$knowlPol)

#Häufigkeiten:
tableKnowlPol <- with(main_df_desc, table(main_df_desc$knowlPol))
percfreqTabKnowlPol <- round(prop.table(tableKnowlPol)*100, 2)
cumFreqTabKnowlPol <- cumsum(percfreqTabKnowlPol)
freqTabKnowlPol <- rbind(tableKnowlPol, percfreqTabKnowlPol, cumFreqTabKnowlPol)
remove(tableKnowlPol, percfreqTabKnowlPol, cumFreqTabKnowlPol)
rownames(freqTabKnowlPol)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabKnowlPol <- t(freqTabKnowlPol))

#Maße der zentralen Tendenz:
main_df_desc$knowlPol %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$knowlPol %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(knowlPol)) + 
  scale_x_discrete(limits = c("STIMME VOLL ZU", "STIMME EHER ZU", "STIMME EHER NICHT ZU", "STIMME GAR NICHT ZU")) + 
  labs(y = "Anzahl", x = "Wenig über Politik wissen", title = "Zustimmung") +
  ggsave("freqKnowlPol.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###PEOPLE SHLD STAY INFORMED ABOUT POLITICS
str(main_df_desc$shldInfPol)

#Häufigkeiten:
tableShldInfPol <- with(main_df_desc, table(main_df_desc$shldInfPol))
percfreqTabShldInfPol <- round(prop.table(tableShldInfPol)*100, 2)
cumFreqTabShldInfPol <- cumsum(percfreqTabShldInfPol)
freqTabShldInfPol <- rbind(tableShldInfPol, percfreqTabShldInfPol, cumFreqTabShldInfPol)
remove(tableShldInfPol, percfreqTabShldInfPol, cumFreqTabShldInfPol)
rownames(freqTabShldInfPol)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabKShldInfPol <- t(freqTabShldInfPol))

#Maße der zentralen Tendenz:
main_df_desc$shldInfPol %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$shldInfPol %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(shldInfPol)) + 
  scale_x_discrete(limits = c("STIMME VOLL ZU", "STIMME EHER ZU", "STIMME EHER NICHT ZU", "STIMME GAR NICHT ZU")) + 
  labs(y = "Anzahl", x = "Informiert sein über Politik", title = "Zustimmung") +
  ggsave("freqShldInfPol.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###SELF-PLACEMENT ON LEFT-RIGHT CONTINUUM
str(main_df_desc$leftRight)

#Häufigkeiten:
tableLeftRight <- with(main_df_desc, table(main_df_desc$leftRight))
percfreqTabLeftRight <- round(prop.table(tableLeftRight)*100, 2)
cumFreqTabLeftRight <- cumsum(percfreqTabLeftRight)
freqTabLeftRight <- rbind(tableLeftRight, percfreqTabLeftRight, cumFreqTabLeftRight)
remove(tableLeftRight, percfreqTabLeftRight, cumFreqTabLeftRight)
rownames(freqTabLeftRight)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabLeftRight <- t(freqTabLeftRight))

#Maße der zentralen Tendenz:
main_df_desc$leftRight %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$leftRight %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_histogram(aes(leftRight), stat = "count") + 
  labs(y = "Anzahl", x = "Rechts-links Selbsteinschätzung", title = "Einschätzung") +
  ggsave("freqLeftRight.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###SELF-ASSESSMENT OF SOCIAL CLASS, RESP
str(main_df_desc$class)

#Häufigkeiten:
tableClass <- with(main_df_desc, table(main_df_desc$class))
percfreqTabClass <- round(prop.table(tableClass)*100, 2)
cumFreqTabClass <- cumsum(percfreqTabClass)
freqTabClass <- rbind(tableClass, percfreqTabClass, cumFreqTabClass)
remove(tableClass, percfreqTabClass, cumFreqTabClass)
rownames(freqTabClass)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabClass <- t(freqTabClass))

#Maße der zentralen Tendenz:
main_df_desc$class %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$class %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(class)) + 
  scale_x_discrete(limits = c("UNTERSCHICHT", "ARBEITERSCHICHT", "MITTELSCHICHT", "OBERE MITTELSCHICHT", "OBERSCHICHT")) + 
  labs(y = "Anzahl", x = "Schicht Selbsteinschätzung", title = "Selbsteinschätzung") +
  ggsave("freqClass.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###RESPONDENT: SEX
str(main_df_desc$sex)

#Häufigkeiten:
tableSex <- with(main_df_desc, table(main_df_desc$sex))
percfreqTabSex <- round(prop.table(tableSex)*100, 2)
cumFreqTabSex <- cumsum(percfreqTabSex)
freqTabSex <- rbind(tableSex, percfreqTabSex, cumFreqTabSex)
remove(tableSex, percfreqTabSex, cumFreqTabSex)
rownames(freqTabSex)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabSex <- t(freqTabSex))

#Maße der zentralen Tendenz:
main_df_desc$sex %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$sex %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(sex)) + 
  scale_x_discrete(limits = c("MANN", "FRAU")) + 
  labs(y = "Anzahl", x = "Geschlecht der Befragten", title = "Geschlecht") +
  ggsave("freqSex.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###RESPONDENT: AGE, CATEGORIZED
str(main_df_desc$agec)

#Häufigkeiten:
tableAgec <- with(main_df_desc, table(main_df_desc$agec))
percfreqTabAgec <- round(prop.table(tableAgec)*100, 2)
cumFreqTabAgec <- cumsum(percfreqTabAgec)
freqTabAgec <- rbind(tableAgec, percfreqTabAgec, cumFreqTabAgec)
remove(tableAgec, percfreqTabAgec, cumFreqTabAgec)
rownames(freqTabAgec)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabAgec <- t(freqTabAgec))

#Maße der zentralen Tendenz:
main_df_desc$agec %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$agec %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(agec)) + 
  scale_x_discrete(limits = c("18-29 JAHRE", "30-44 JAHRE", "45-59 JAHRE", "60-74 JAHRE", "75-89 JAHRE", "UEBER 89 JAHRE")) + 
  labs(y = "Anzahl", x = "Alter der Befragten", title = "Alter") +
  ggsave("freqAgec.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###RESP.: GENERAL SCHOOL LEAVING CERTIFICATE
str(main_df_desc$educ)

#Häufigkeiten:
tableEduc <- with(main_df_desc, table(main_df_desc$educ))
percfreqTabEduc <- round(prop.table(tableEduc)*100, 2)
cumFreqTabEduc <- cumsum(percfreqTabEduc)
freqTabEduc <- rbind(tableEduc, percfreqTabEduc, cumFreqTabEduc)
remove(tableEduc, percfreqTabEduc, cumFreqTabEduc)
rownames(freqTabEduc)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabEduc <- t(freqTabEduc))

#Maße der zentralen Tendenz:
main_df_desc$educ %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$educ %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(educ)) + 
  scale_x_discrete(limits = c("OHNE ABSCHLUSS", "VOLKS-,HAUPTSCHULE", "MITTLERE REIFE", "FACHHOCHSCHULREIFE", "HOCHSCHULREIFE", "ANDERER ABSCHLUSS", "NOCH SCHUELER")) + 
  labs(y = "Anzahl", x = "Schulabschluss der Befragten", title = "Schulabschluss") +
  ggsave("freqEduc.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###RESP.: UNIVERSITY DEGREE
str(main_df_desc$uniDeg)

#Häufigkeiten:
tableUniDeg <- with(main_df_desc, table(main_df_desc$uniDeg))
percfreqTabUniDeg <- round(prop.table(tableUniDeg)*100, 2)
cumFreqTabUniDeg <- cumsum(percfreqTabUniDeg)
freqTabUniDeg <- rbind(tableUniDeg, percfreqTabUniDeg, cumFreqTabUniDeg)
remove(tableUniDeg, percfreqTabUniDeg, cumFreqTabUniDeg)
rownames(freqTabUniDeg)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabUniDeg <- t(freqTabUniDeg))

#Maße der zentralen Tendenz:
main_df_desc$uniDeg %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$uniDeg %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(uniDeg)) + 
  scale_x_discrete(limits = c("NICHT GENANNT", "GENANNT")) + 
  labs(y = "Anzahl", x = "Angabe zu Uniabschluss", title = "Universitätsabschluss") +
  ggsave("freqUniDeg.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###RESP.: TYPE OF UNIVERSITY DEGREE
str(main_df_desc$typeUniDeg)

#Häufigkeiten:
tableTypeUniDeg <- with(main_df_desc, table(main_df_desc$typeUniDeg))
percfreqTabTypeUniDeg <- round(prop.table(tableTypeUniDeg)*100, 2)
cumFreqTabTypeUniDeg <- cumsum(percfreqTabTypeUniDeg)
freqTabTypeUniDeg <- rbind(tableTypeUniDeg, percfreqTabTypeUniDeg, cumFreqTabTypeUniDeg)
remove(tableTypeUniDeg, percfreqTabTypeUniDeg, cumFreqTabTypeUniDeg)
rownames(freqTabTypeUniDeg)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabTypeUniDeg <- t(freqTabTypeUniDeg))

#Maße der zentralen Tendenz:
main_df_desc$typeUniDeg %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$typeUniDeg %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(typeUniDeg)) + 
  scale_x_discrete(limits = c("BACHELOR", "MASTER", "DIPLOM", "MAGISTER", "STAATSEXAMEN", "PROMOTION", "SONSTIGES")) + 
  labs(y = "Anzahl", x = "Art des Uniabschluss", title = "Universitätsabschluss") +
  ggsave("freqTypeUniDeg.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###RESPONDENT: CURRENT EMPLOYMENT STATUS
str(main_df_desc$work)

#Häufigkeiten:
tableWork <- with(main_df_desc, table(main_df_desc$work))
percfreqTabWork <- round(prop.table(tableWork)*100, 2)
cumFreqTabWork <- cumsum(percfreqTabWork)
freqTabWork <- rbind(tableWork, percfreqTabWork, cumFreqTabWork)
remove(tableWork, percfreqTabWork, cumFreqTabWork)
rownames(freqTabWork)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabWork <- t(freqTabWork))

#Maße der zentralen Tendenz:
main_df_desc$work %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$work %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(work)) + 
  scale_x_discrete(limits = c("HAUPTBERUFL.GANZTAGS", "HAUPTBERUFL.HALBTAGS", "NEBENHER BERUFSTAE.", "NICHT ERWERBSTAETIG")) + 
  labs(y = "Anzahl", x = "Art der Erwerbstätigkeit", title = "Erwerbstätigkeit") +
  ggsave("freqWork.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###RESPONDENT: CURRENT OCCUPATION
str(main_df_desc$currOcc)

#Häufigkeiten:
tableCurrOcc <- with(main_df_desc, table(main_df_desc$currOcc))
percfreqTabCurrOcc <- round(prop.table(tableCurrOcc)*100, 2)
cumFreqTabCurrOcc <- cumsum(percfreqTabCurrOcc)
freqTabCurrOcc <- rbind(tableCurrOcc, percfreqTabCurrOcc, cumFreqTabCurrOcc)
remove(tableCurrOcc, percfreqTabCurrOcc, cumFreqTabCurrOcc)
rownames(freqTabCurrOcc)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabCurrOcc <- t(freqTabCurrOcc))

#Maße der zentralen Tendenz:
main_df_desc$currOcc %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$currOcc %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(currOcc)) + 
  scale_x_discrete(limits = c("LANDWIRT", "AKADEM.FREIER BERUF", "SONST.SELBSTAENDIGE", "BEAMT,RICHTER,SOLDAT", "ANGESTELLTER", "ARBEITER", "IN AUSBILDUNG", "MITHELF.FAMILIENANG.")) + 
  labs(y = "Anzahl", x = "Derzeitige Beschäftigung", title = "Beschäftigung") +
  ggsave("freqCurrOcc.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###RESP.: SUPERVISING THE WORK OF OTHERS?
str(main_df_desc$subvOther)

#Häufigkeiten:
tableSubvOther <- with(main_df_desc, table(main_df_desc$subvOther))
percfreqTabSubvOther <- round(prop.table(tableSubvOther)*100, 2)
cumFreqTabSubvOther <- cumsum(percfreqTabSubvOther)
freqTabSubvOther <- rbind(tableSubvOther, percfreqTabSubvOther, cumFreqTabSubvOther)
remove(tableSubvOther, percfreqTabSubvOther, cumFreqTabSubvOther)
rownames(freqTabSubvOther)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabSubvOther <- t(freqTabSubvOther))

#Maße der zentralen Tendenz:
main_df_desc$subvOther %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$subvOther %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(subvOther)) + 
  scale_x_discrete(limits = c("JA", "NEIN")) + 
  labs(y = "Anzahl", x = "Beaufsichtigung von Angestellten", title = "Beaufsichtigung") +
  ggsave("freqSubvOther.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###RESP.: UNEMPLOYMENT IN THE LAST 10 YEARS
str(main_df_desc$unempl)

#Häufigkeiten:
tableUnempl <- with(main_df_desc, table(main_df_desc$unempl))
percfreqTabUnempl <- round(prop.table(tableUnempl)*100, 2)
cumFreqTabUnempl <- cumsum(percfreqTabUnempl)
freqTabUnempl <- rbind(tableUnempl, percfreqTabUnempl, cumFreqTabUnempl)
remove(tableUnempl, percfreqTabUnempl, cumFreqTabUnempl)
rownames(freqTabUnempl)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabUnempl <- t(freqTabUnempl))

#Maße der zentralen Tendenz:
main_df_desc$unempl %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$unempl %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(unempl)) + 
  scale_x_discrete(limits = c("JA", "NEIN")) + 
  labs(y = "Anzahl", x = "Arbeitslosigkeit in den letzten 10 Jahren", title = "Arbeitslosigkeit") +
  ggsave("freqUnempl.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###HH.: MONTHLY NET INC.-OPEN+CLOSED, CAT.
str(main_df_desc$hhincc)

#Häufigkeiten:
tableHhincc <- with(main_df_desc, table(main_df_desc$hhincc))
percfreqTabHhincc <- round(prop.table(tableHhincc)*100, 2)
cumFreqTabHhincc <- cumsum(percfreqTabHhincc)
freqTabHhincc <- rbind(tableHhincc, percfreqTabHhincc, cumFreqTabHhincc)
remove(tableHhincc, percfreqTabHhincc, cumFreqTabHhincc)
rownames(freqTabHhincc)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabHhincc <- t(freqTabHhincc))

#Maße der zentralen Tendenz:
main_df_desc$hhincc %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$hhincc %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_histogram(aes(hhincc), stat = "count") + 
  labs(y = "Anzahl", x = "Monatliches Nettoeinkommen pro Haushalt", title = "Haushaltseinkommen") +
  ggsave("freqHhincc.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###HOW OFTEN TALK ABOUT POLITICS: IN FAMILY
str(main_df_desc$freqPolFam)

#Häufigkeiten:
tableFreqPolFam <- with(main_df_desc, table(main_df_desc$freqPolFam))
percfreqTabFreqPolFam <- round(prop.table(tableFreqPolFam)*100, 2)
cumFreqTabFreqPolFam <- cumsum(percfreqTabFreqPolFam)
freqTabFreqPolFam <- rbind(tableFreqPolFam, percfreqTabFreqPolFam, cumFreqTabFreqPolFam)
remove(tableFreqPolFam, percfreqTabFreqPolFam, cumFreqTabFreqPolFam)
rownames(freqTabFreqPolFam)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabFreqPolFam <- t(freqTabFreqPolFam))

#Maße der zentralen Tendenz:
main_df_desc$freqPolFam %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqPolFam %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(freqPolFam)) + 
  scale_x_discrete(limits = c("SEHR OFT", "OFT", "MANCHMAL", "SELTEN", "NIE")) + 
  labs(y = "Anzahl", x = "Austausch über Politik in der Familie", title = "Austausch über Politik") +
  ggsave("freqFreqPolFam.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###HOW OFTEN TALK ABOUT POLITICS: FRIENDS
str(main_df_desc$freqPolFre)

#Häufigkeiten:
tableFreqPolFre <- with(main_df_desc, table(main_df_desc$freqPolFre))
percfreqTabFreqPolFre <- round(prop.table(tableFreqPolFre)*100, 2)
cumFreqTabFreqPolFre <- cumsum(percfreqTabFreqPolFre)
freqTabFreqPolFre <- rbind(tableFreqPolFre, percfreqTabFreqPolFre, cumFreqTabFreqPolFre)
remove(tableFreqPolFre, percfreqTabFreqPolFre, cumFreqTabFreqPolFre)
rownames(freqTabFreqPolFre)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabFreqPolFre <- t(freqTabFreqPolFre))

#Maße der zentralen Tendenz:
main_df_desc$freqPolFre %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$freqPolFre %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(freqPolFre)) + 
  scale_x_discrete(limits = c("SEHR OFT", "OFT", "MANCHMAL", "SELTEN", "NIE")) + 
  labs(y = "Anzahl", x = "Austausch über Politik im Freundeskreis", title = "Austausch über Politik") +
  ggsave("freqFreqPolFre.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###SELF-ASSESSMENT OF PLACE WHERE R LIVES
str(main_df_desc$placeLive)

#Häufigkeiten:
tablePlaceLive <- with(main_df_desc, table(main_df_desc$placeLive))
percfreqTabPlaceLive <- round(prop.table(tablePlaceLive)*100, 2)
cumFreqTabPlaceLive <- cumsum(percfreqTabPlaceLive)
freqTabPlaceLive <- rbind(tablePlaceLive, percfreqTabPlaceLive, cumFreqTabPlaceLive)
remove(tablePlaceLive, percfreqTabPlaceLive, cumFreqTabPlaceLive)
rownames(freqTabPlaceLive)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabPlaceLive<- t(freqTabPlaceLive))

#Maße der zentralen Tendenz:
main_df_desc$placeLive %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$placeLive %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(placeLive)) + 
  scale_x_discrete(limits = c("GROSSSTADT", "VORORT GROSSSTADT", "MITTEL-, KLEINSTADT", "LAENDL. DORF", "EINZELHAUS, LAND")) + 
  labs(y = "Anzahl", x = "Wohnumgebung des Befragten", title = "Wohnumgebung") +
  ggsave("freqPlaceLive.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###DID YOU VOTE IN LAST FEDERAL ELECTION?
str(main_df_desc$vote)

#Häufigkeiten:
tableVote <- with(main_df_desc, table(main_df_desc$vote))
percfreqTabVote <- round(prop.table(tableVote)*100, 2)
cumFreqTabVote <- cumsum(percfreqTabVote)
freqTabVote <- rbind(tableVote, percfreqTabVote, cumFreqTabVote)
remove(tableVote, percfreqTabVote, cumFreqTabVote)
rownames(freqTabVote)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabVote<- t(freqTabVote))

#Maße der zentralen Tendenz:
main_df_desc$vote %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$vote %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(vote)) + 
  scale_x_discrete(limits = c("JA", "NEIN")) + 
  labs(y = "Anzahl", x = "Wahlbeteiligung bei der letzten Bundestagswahl", title = "Wahlbeteiligung") +
  ggsave("freqVote.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###PARTY VOTE IN LAST FEDERAL ELECTION
str(main_df_desc$partyVote)

#Häufigkeiten:
tablePartyVote <- with(main_df_desc, table(main_df_desc$partyVote))
percfreqTabPartyVote <- round(prop.table(tablePartyVote)*100, 2)
cumFreqTabPartyVote <- cumsum(percfreqTabPartyVote)
freqTabPartyVote <- rbind(tablePartyVote, percfreqTabPartyVote, cumFreqTabPartyVote)
remove(tablePartyVote, percfreqTabPartyVote, cumFreqTabPartyVote)
rownames(freqTabPartyVote)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabPartyVote<- t(freqTabPartyVote))

#Maße der zentralen Tendenz:
main_df_desc$partyVote %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$partyVote %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(partyVote)) + 
  scale_x_discrete(limits = c("CDU-CSU", "SPD", "FDP", "DIE GRUENEN", "DIE LINKE", "AFD", "ANDERE PARTEI")) + 
  labs(y = "Anzahl", x = "Wahlentscheidung bei der letzten Bundestagswahl", title = "Wahlentscheidung") +
  ggsave("freqPartyVote.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###INT.: SOCIAL CLASS OF HOUSEHOLD
str(main_df_desc$intClassH)

#Häufigkeiten:
tableIntClassH <- with(main_df_desc, table(main_df_desc$intClassH))
percfreqTabIntClassH <- round(prop.table(tableIntClassH)*100, 2)
cumFreqTabIntClassH <- cumsum(percfreqTabIntClassH)
freqTabIntClassH <- rbind(tableIntClassH, percfreqTabIntClassH, cumFreqTabIntClassH)
remove(tableIntClassH, percfreqTabIntClassH, cumFreqTabIntClassH)
rownames(freqTabIntClassH)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabIntClassH<- t(freqTabIntClassH))

#Maße der zentralen Tendenz:
main_df_desc$intClassH %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$intClassH %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(intClassH)) + 
  scale_x_discrete(limits = c("UNTERSCHICHT", "ARBEITERSCHICHT", "MITTELSCHICHT", "OBERE MITTELSCHICHT", "OBERSCHICHT", "NICHT ERKENNBAR")) + 
  labs(y = "Anzahl", x = "Schichteinstufung des Haushalts durch den Interviewer", title = "Schichteinstufung des Haushalts") +
  ggsave("freqIntClassH.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)


###FEDERAL STATE THAT RESPONDENT LIVES IN
str(main_df_desc$land)

#Häufigkeiten:
tableLand<- with(main_df_desc, table(main_df_desc$land))
percfreqTabLand <- round(prop.table(tableLand)*100, 2)
cumFreqTabLand <- cumsum(percfreqTabLand)
freqTabLand <- rbind(tableLand, percfreqTabLand, cumFreqTabLand)
remove(tableLand, percfreqTabLand, cumFreqTabLand)
rownames(freqTabLand)[c(1, 2, 3)] <- c("n", "Prozente", "kummulierte Prozente")
(freqTabLand<- t(freqTabLand))

#Maße der zentralen Tendenz:
main_df_desc$land %>% as.numeric() %>% summary()

#Streuung:
main_df_desc$land %>% as.numeric() %>% sd(na.rm = TRUE)

#Graphische Darstellung:
ggplot(main_df_desc) + 
  geom_bar(aes(land)) + 
  scale_x_discrete(limits = c("SCHLESWIG-HOLSTEIN", "HAMBURG", "NIEDERSACHSEN", "BREMEN", "NORDRHEIN-WESTFALEN", "HESSEN", "RHEINLAND-PFALZ", "BADEN-WUERTTEMBERG", "BAYERN", "SAARLAND", "EHEM. BERLIN-WEST", "EHEM. BERLIN-OST", "BRANDENBURG", "MECKLENB.-VORPOMMERN", "SACHSEN", "SACHSEN-ANHALT", "THUERINGEN"), labels = c("SCHLESWIG-\nHOLSTEIN", "HAMBURG", "NIEDERSACHSEN", "BREMEN", "NORDRHEIN-\nWESTFALEN", "HESSEN", "RHEINLAND-\nPFALZ", "BADEN-\nWUERTTEMBERG", "BAYERN", "SAARLAND", "EHEM. \nBERLIN-WEST", "EHEM. \nBERLIN-OST", "BRANDENBURG", "MECKLENBURG-\nVORPOMMERN", "SACHSEN", "SACHSEN-\nANHALT", "THUERINGEN")) + 
  labs(y = "Anzahl", x = "Bundesland des Befragten", title = "Bundesland") +
  ggsave("freqLand.pdf", path = "C:/Users/Celik/Documents/OneDrive/Documents/Einführung in R/Bericht/Graphiken", width = 10, height = 10)

#####Ende