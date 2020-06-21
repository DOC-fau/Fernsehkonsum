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
  scale_fill_gradient(low = "white", high = "black") +
  theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Korrelationsplot") +
  ggsave("corPlot.pdf", path = "H:/01 Studium/01 Bachelor of Arts/03 Schluesselqualifikationen/Einfuehrung in R/Seminararbeit/Bericht/Graphiken", width = 10, height = 10)

remove(corTblLongFormat, corTbl)
# Einige Chunks existieren! german [2], class [16], incc [22], uniDeg [20], intClassH [28]

main_df_logReg <- main_df_logReg[, c(1, 3:15, 17:19, 21, 23:27)]
main_df_logReg_num <- main_df_logReg_num[, c(1, 3:15, 17:19, 21, 23:27)]


###Konfirmative Faktorenanalyse:
##Mordellvoraussetzung für die Faktorenanalyse (Barletts Test):
cortest.bartlett(main_df_logReg_num)
# Signifikant.


##Null-Modell:
pca.null <- principal(main_df_logReg_num, nfactors = 23, rotate = "none")

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
pca.nfactors <- principal(main_df_logReg_num, nfactors = 7, rotate = "none")


##Full-Modell:
pca.full <- principal(main_df_logReg_num, nfactors = 7, rotate="varimax")

remove(main_df_logReg_num, pca.null, pca.nfactors)

# Nun auch rotierte Komponenten und folgende Komponentenmatrix:
print.psych(pca.full, cut = 0.3, sort = TRUE)


###Faktoren in denen die Variablen laden:
##F01
# freqPolFam
# freqPolFre
# knowlPol    (F06)
# shldInfPol
# polCompl    (F06)
# vote        (F03)

# Index: Politische Partizipation; vote außenvor


##F02
# agec
# work        (F03)
# intPol      (F01)
# freqNewsp
# ardZdf

# Kein theoretisch begründbarer Index


##F03
# finSit
# hhincc

# Kein sinnvoller Index


##F04
# trustTv
# trustNewsp
# infMedia

# Vertrauen in die Medianlandschaft


##F05
# placeLive
# educ        (F02, F03)

# Kein sinnvoller Index


##F06
# sex
# leftRight   (F05)

# Kein theoretisch begründbarer Index


##F07
# eastWest    (F05)
# newsPriv    (F05)
# tvWeek      (F02)

# Kein sinnvoller Index


###Indize-lose Variablen:
# vote
# agec
# work
# intPol
# freqNewsp
# ardZdf
# finSit
# hhincc
# placeLive
# educ
# sex
# leftRight
# eastWest
# newsPriv
# tvWeek 


####Indizes bilden:
###Index: Politische Partizipation
# freqPolFam
# freqPolFre
# knowlPol
# shldInfPol
# polCompl 

summary(main_df_logReg$freqPolFam)
summary(main_df_logReg$freqPolFre)
summary(main_df_logReg$knowlPol)
summary(main_df_logReg$shldInfPol)
summary(main_df_logReg$polCompl)

var01 <- plyr::revalue(main_df_logReg$freqPolFam, c("KEINE ANTWORT" = NA,
                                                      "KEIN VERTRAUEN" = "1",
                                                      "GROßES VERTRAUEN" = "7"))

var02 <- plyr::revalue(main_df_logReg$freqPolFre, c("KEINE ANTWORT" = NA,
                                                     "KEIN VERTRAUEN" = "1",
                                                     "GROßES VERTRAUEN" = "7"))

var03 <- plyr::revalue(main_df_logReg$knowlPol, c("KEINE ANTWORT" = NA,
                                                   "KEIN VERTRAUEN" = "1",
                                                   "GROßES VERTRAUEN" = "7"))

var04 <- plyr::revalue(main_df_logReg$shldInfPol, c("KEINE ANTWORT" = NA,
                                                      "KEIN VERTRAUEN" = "1",
                                                      "GROßES VERTRAUEN" = "7"))

var05 <- plyr::revalue(main_df_logReg$polCompl, c("KEINE ANTWORT" = NA,
                                                    "KEIN VERTRAUEN" = "1",
                                                    "GROßES VERTRAUEN" = "7"))



###Vertrauen in die Medianlandschaft
# trustTv
# trustNewsp
# infMedia








#########################


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