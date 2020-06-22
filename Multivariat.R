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


###Datensatz vorbereiten:
# Zu wenige besitzen einen Universitätsabschluss (typeUniDeg); currOcc, subvOther durch dw10 gefiltert; unempl durch work
# gefiltert; freqNewsPriv durch lm21 (newsPriv) gefiltert; freqIntPol durch lm23 (intPol) gefiltert; partyVote durch pv03 
# (vote) gefiltert; freqNewsPubl durch lm19 (selbiges durch lm01 (tvWeek)) gefiltert

# Ausschluss von typeUniDeg [24], currOcc [8], subvOther [26], unempl [27], freqNewsPriv [28], freqIntPol [11], 
# partyVote [35] freqNewsPubl [6]

main_df_logReg %>% missmap(main = "Missing values vs observed")
main_df_logReg <- main_df_logReg[, c(1:5, 7, 9:10, 12:23, 25, 29:34, 36:38)]
main_df_logReg <- na.omit(main_df_logReg)
main_df_logReg %>% missmap(main = "Missing values vs observed")


#####Analyse:
####Datenreduktion:
main_df_logReg_num <- dplyr::mutate_if(main_df_logReg, is.factor, ~ as.numeric(.x))


###Explorative Faktorenanalyse:
##Korrelationsmatrix
(corTbl <- round(cor(main_df_logReg_num), 4))


##Multikollinearität:
corTblLongFormat <- melt(corTbl)

corTblLongFormat %>% filter(corTblLongFormat$value == 1 | corTblLongFormat$value == -1)
# Nur wghtpew und estWest

corTblLongFormat %>% filter(corTblLongFormat$value >= 0.7 & corTblLongFormat$value != 1)
# Nur land und eastWest

corTblLongFormat %>% filter(corTblLongFormat$value <= -0.7 & corTblLongFormat$value != -1)
# Nur land und wghtpew

#Ausschluss von Variablen unter Multikollinearität:
# (wghtpew, land)
main_df_logReg <- main_df_logReg[, 1:28]


##Korrelationsplot:
(corTbl <- round(cor(main_df_logReg_num), 4))
main_df_logReg_num <- main_df_logReg_num[, 1:28]
(corTbl <- round(cor(main_df_logReg_num), 4))
corTblLongFormat <- melt(corTbl)

ggplot(corTblLongFormat) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_gradient(low = "white", high = "black", limits = c(-1, 1)) +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Korrelationsplot") +
  ggsave("corPlot.pdf", path = "H:/01 Studium/01 Bachelor of Arts/03 Schluesselqualifikationen/Einfuehrung in R/Seminararbeit/Bericht/Graphiken", width = 10, height = 10)

# Einige Chunks existieren! german [2], class [16], incc [22], uniDeg [20], intClassH [28], trustNewsp [10], polCompl [12],
# freqPolFre [25], work [21], freqPolFam [24], finSit [3]

main_df_logReg <- main_df_logReg[, c(1, 4:9, 11, 13:15, 17, 23, 26:27)]
main_df_logReg_num <- main_df_logReg_num[, c(1, 4:9, 11, 13:15, 17, 23, 26:27)]


##Finale Prüfung nach Korrelationen
(corTbl <- round(cor(main_df_logReg_num), 4))
main_df_logReg_num <- main_df_logReg_num[, 1:17]
(corTbl <- round(cor(main_df_logReg_num), 4))
corTblLongFormat <- melt(corTbl)

ggplot(corTblLongFormat) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_gradient(low = "white", high = "black", limits = c(-1, 1)) +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Korrelationsplot") +
  ggsave("corPlot.pdf", path = "H:/01 Studium/01 Bachelor of Arts/03 Schluesselqualifikationen/Einfuehrung in R/Seminararbeit/Bericht/Graphiken", width = 10, height = 10)

remove(corTblLongFormat, corTbl)

###Konfirmative Faktorenanalyse:
##Mordellvoraussetzung für die Faktorenanalyse (Barletts Test):
cortest.bartlett(main_df_logReg_num)
# Signifikant.


##Null-Modell:
pca.null <- principal(main_df_logReg_num, nfactors = 15, rotate = "none")

# Das Null-Modell hat zwei Spalten mit h2 und u2. h2, die Kommunalität, ist der Anteil der 
# Varianz, den jede Variable mit anderen Variablen gemeinsam hat - vorerst sind alle 1. 
# u2 ist unique variance. Sie beschreibt den Anteil der Varianz einer Variable, die nicht 
# nicht mit einer Faktoren-Struktur geteilt wird. Sie ist 1 minus h2 - somit sind alle vorerst 0.


##Kaiser-Kriterium:
plot(pca.null$values, type = "b", xaxt = 'n', pch = 4)
axis(2, at = 1)
abline(h = 1)

# Das Eigenwertkriterium bei x = 1 trennt nützliche von untützen Faktoren. NÜtzliche Faktoren
# liegen im Bereich über dem Eigenwertkriterium. Hier 8.

#Modell mit den nützlichen Faktoren:
pca.nfactors <- principal(main_df_logReg_num, nfactors = 5, rotate = "none")


##Full-Modell:
pca.full <- principal(main_df_logReg_num, nfactors = 5, rotate="varimax")

remove(main_df_logReg_num, pca.null, pca.nfactors)

# Nun auch rotierte Komponenten und folgende Komponentenmatrix:
print.psych(pca.full, cut = 0.3, sort = TRUE)


###Faktoren in denen die Variablen laden:
##F01
# knowlPol    (F02)
# ardZdf
# vote
# shldInfPol
# freqNewsp   (F02)

# Index: Politische Partizipation, ardZdf ausschließen


##F02
# intPol      (F03)
# placeLive

# Kein theoretisch begründbarer Index


##F03
# infMedia
# trustTv

# Index: Vertrauen in Medien


##F04
# eastWest
# newsPriv    (F05)
# tvWeek      (F02)
# hhincc      (F01)

# Kein theoretisch begründbarer Index


##F05
# leftRight
# sex

# Kein theoretisch begründbarer Index


###Indize-lose Variablen:
# ardZdf
# intPol
# placeLive
# eastWest
# newsPriv
# tvWeek
# hhincc
# leftRight
# sex


####Indizes bilden:
###Index: Politische Partizipation (polPart)
# knowlPol
# vote
# shldInfPol
# freqNewsp 

summary(main_df_logReg$knowlPol)
summary(main_df_logReg$vote)
summary(main_df_logReg$shldInfPol)
summary(main_df_logReg$freqNewsp)

var01 <- factor(main_df_logReg$knowlPol,levels(main_df_logReg$knowlPol)[c(4, 3, 2, 1)])
var02 <- factor(main_df_logReg$vote,levels(main_df_logReg$vote)[c(2, 1)])
var03 <- factor(main_df_logReg$shldInfPol,levels(main_df_logReg$shldInfPol)[c(4, 3, 2, 1)])

var01 <- plyr::revalue(var01, c("STIMME GAR NICHT ZU" = "KEINE ZUSTIMMUNG",
                                "STIMME EHER NICHT ZU" = "KEINE ZUSTIMMUNG",
                                "STIMME EHER ZU" = "ZUSTIMMUNG",
                                "STIMME VOLL ZU" = "ZUSTIMMUNG"))

var03 <- plyr::revalue(var03, c("STIMME GAR NICHT ZU" = "KEINE ZUSTIMMUNG",
                                 "STIMME EHER NICHT ZU" = "KEINE ZUSTIMMUNG",
                                 "STIMME EHER ZU" = "ZUSTIMMUNG",
                                 "STIMME VOLL ZU" = "ZUSTIMMUNG"))

var04 <- plyr::revalue(main_df_logReg$freqNewsp, c("NIE" = "NEIN",
                                                   "SELTENER" = "JA",
                                                   "AN EINEM TAG" = "JA",
                                                   "AN 2 TAGEN" = "JA",
                                                   "AN 3 TAGEN" = "JA",
                                                   "AN 4 TAGEN" = "JA",
                                                   "AN 5 TAGEN" = "JA",
                                                   "AN 6 TAGEN" = "JA",
                                                   "AN ALLEN 7 TAGEN" = "JA"))

var01 <- as.numeric(var01)
var02 <- as.numeric(var02)
var03 <- as.numeric(var03)
var04 <- as.numeric(var04)

main_df_logReg$polPart <- (var01 + var02 + var03 + var04)/4 - 1
remove(var01, var02, var03, var04)
summary(main_df_logReg$polPart)
# 0 Keine politische Partizipation - 1 Starke politische Partizipation


###Index: Vertrauen in Medien (trustMed)
# trustTv
# infMedia

summary(main_df_logReg$trustTv)
summary(main_df_logReg$infMedia)

#########################

var01 <- plyr::revalue(main_df_logReg$trustTv, c("KEIN VERTRAUEN" = "GERINGES VERTRAUEN",
                                                 "2" = "GERINGES VERTRAUEN",
                                                 "3" = "MODERATES VERTRAUEN",
                                                 "4" = "MODERATES VERTRAUEN",
                                                 "5" = "MODERATES VERTRAUEN",
                                                 "6" = "STÄRKERES VERTRAUEN",
                                                 "GROßES VERTRAUEN" = "STÄRKERES VERTRAUEN"))

var02 <- factor(main_df_logReg$infMedia,levels(main_df_logReg$infMedia)[c(3, 2, 1)])

var01 <- as.numeric(var01)
var02 <- as.numeric(var02)

main_df_logReg$trustMed <- (var01 + var02)/2 - 1
remove(var01, var02)
summary(main_df_logReg$trustMed)
# 0 Geringes Vertrauen in Medien - 2 Hohes Vertrauen in Medien


####Aufbereitung der Variablen für die Analyse:
###Alleinstehende Variablen:
# ardZdf 3
# intPol 6
# placeLive 14
# eastWest 1
# newsPriv 4
# tvWeek 2
# hhincc 13
# leftRight 11
# sex 12

###Indices:
# polPart 16
# trustMed 17

###Datensatz vorbereiten:

summary(main_df_logReg$ardZdf)
summary(main_df_logReg$intPol)
summary(main_df_logReg$placeLive)
summary(main_df_logReg$eastWest)
summary(main_df_logReg$newsPriv)
summary(main_df_logReg$tvWeek)
summary(main_df_logReg$hhincc)
summary(main_df_logReg$leftRight)
summary(main_df_logReg$sex)

main_df_logReg <- main_df_logReg[, c(1:4, 6, 11:14, 16:17)]





main_df_logReg$placeLive
# Einzelhaus muss nicht arm sein? Aussagekraft?




############## Quasi-metrik der Variablen <- as.numeric


# main_df_logReg$hhincc <- as.numeric(main_df_logReg$hhincc)


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
model.final = glm(ardZdf ~ tvWeek + polPart + trustMed + newsPriv + hhincc + leftRight,
                  data=  main_df_logReg,
                  family = binomial(link = "logit")
)

remove(model.null, model.full)

#Koeffizienten:
summary(model.final)
nagelkerke(model.final)
Anova(model.final, type="II", test="Wald")


##Modellvoraussetzungen:
#Multikollinearität:
vif(model.final)

#####Ende