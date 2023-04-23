setwd("C:/Users/joshu/OneDrive - bwedu/Dokumente/Uni 7 Year abroad/3 Music & Science Eerola/Replication Project/Data analysis")

library(psych) #psych::describe
library(scales)
library(tidyverse)
library("Hmisc") #Hmisc::rcorr
library(tidyr) #tidyr::gather
library(ggplot2) #ggplot2::ggplot
library(emmeans)
library(afex)

#import data
fulldata <- read.csv("Replication+project+group+4_March+6,+2023_16.07.csv", header=TRUE)

##Descriptives----
#Age
describe(fulldata$age) #M=27.31, SD=14.45, min=19, max=78

#Gender
table(fulldata$gender) #f=35, m=13, o=3
prop.table(table(fulldata$gender)) #f=68.63%, m=25.49%, o=5.88%

#Occupation
table(fulldata$occupation) #school=2 , uni=40 , full-time=3 , part-time=3 , retired=3
prop.table(table(fulldata$occupation)) #3.9%, 78.4%, 5.9%, 5.9%, 5.9%

#Country
prop.table(table(fulldata$country)) #Germany=11 (21.6%), Switzerland=6 (11.8%), UK=29 (56.9%)

#Formal Musical Experience
prop.table(table(fulldata$mus_instrument)) #instrument playing=33 (64.7%), non-playing=18(35.3%)
table(fulldata$mus_lessons) #all 33 instrumentalists have taken lessons
describe(fulldata$mus_lessons_1_TEXT) #M=9.66, SD=3.66, Min=2, Max=15
describe(fulldata$mus_playing) #M=7.85, SD=10.17, Min=0, Max=50

#Informal Music Engagement
describe(fulldata$mus_listening) #M=12.51, SD=10.74, Min=0.5, Max=45

#OMSI
prop.table(table(fulldata$OMSI.1.question)) 
# Nonmusician  4 (7.8%)
# Music-loving nonmusician  15 (29.4%)
# Amateur musician  14 (27.5%)
# Serious amateur musician  9  (17.6%)
# Semi-professional musician  5 (9.8%)
# Professional musician  4 (7.8%)

#Familiarity with WTC
table(fulldata$context_knowledge_1) #15
table(fulldata$context_knowledge_2) #25
table(fulldata$context_knowledge_3) #3
table(fulldata$context_knowledge_4) #3
table(fulldata$context_knowledge_5) #1
fulldata$context_knowledge_6_TEXT

fulldata$context_knowledge <- ifelse(fulldata$context_knowledge_5 %in% 1, 5, 
                                          ifelse(fulldata$context_knowledge_4 %in% 1, 4, 
                                                 ifelse(fulldata$context_knowledge_3 %in% 1, 3, 
                                                        ifelse(fulldata$context_knowledge_1 %in% 1, 2, 
                                                               ifelse(fulldata$context_knowledge_2 %in% 1, 1, 
                                                                      0)))))
prop.table(table(fulldata$context_knowledge))

#for descriptives of outcome variables: run script section "Correlations" first
#Valence
cor.test(fulldata$dminfug_fast_valence_1, fulldata$dminfug_fas1_valence_1) #r = .39, p = .005
psych::describe(data_long_clean[which(data_long_clean$manipulation == "orig"), ]$valence) #M = 5.07, SD = 1.89, min = 1, max = 9
psych::describe(data_long_clean[which(data_long_clean$manipulation == "fast"), ]$valence) #M = 5.32, SD = 1.84, min = 1, max = 9
psych::describe(data_long_clean[which(data_long_clean$manipulation == "maj"), ]$valence) #M = 5.75, SD = 2.06, min = 1, max = 9
psych::describe(data_long_clean[which(data_long_clean$manipulation == "min"), ]$valence) #M = 5.38, SD = 1.54, min = 1, max = 9
psych::describe(data_long_clean[which(data_long_clean$manipulation == "pedl"), ]$valence) #M = 5.18, SD = 1.76, min = 1, max = 9

#Arousal
cor.test(fulldata$dminfug_fast_arousal_1, fulldata$dminfug_fas1_arousal_1) #r = .52, p < .001
psych::describe(data_long_clean[which(data_long_clean$manipulation == "orig"), ]$arousal) #M = 5.13, SD = 1.82, min = 1, max = 9
psych::describe(data_long_clean[which(data_long_clean$manipulation == "fast"), ]$arousal) #M = 6.15, SD = 1.8, min = 1, max = 9
psych::describe(data_long_clean[which(data_long_clean$manipulation == "maj"), ]$arousal) #M = 5.01, SD = 2.04, min = 1, max = 9
psych::describe(data_long_clean[which(data_long_clean$manipulation == "min"), ]$arousal) #M = 4.86, SD = 1.85, min = 1, max = 8
psych::describe(data_long_clean[which(data_long_clean$manipulation == "pedl"), ]$arousal) #M = 5.12, SD = 1.76, min = 1, max = 9

#Authenticity
cor.test(fulldata$dminfug_fast_auth_1, fulldata$dminfug_fas1_auth_1) #r = .52, p < .001
psych::describe(data_long_clean[which(data_long_clean$manipulation == "orig"), ]$auth) #M = 6.11, SD = 2.17, min = 1, max = 9
psych::describe(data_long_clean[which(data_long_clean$manipulation == "fast"), ]$auth) #M = 6.15, SD = 2.03, min = 1, max = 9
psych::describe(data_long_clean[which(data_long_clean$manipulation == "maj"), ]$auth) #M = 5.62, SD = 2.39, min = 1, max = 9
psych::describe(data_long_clean[which(data_long_clean$manipulation == "min"), ]$auth) #M = 5.25, SD = 2.24, min = 1, max = 8
psych::describe(data_long_clean[which(data_long_clean$manipulation == "pedl"), ]$auth) #M = 5.85, SD = 2.08, min = 1, max = 9

##Correlations----
#Remove irrelevant variables
colnames(fulldata)
irrelevant_variables <- c("StartDate", "EndDate", "Status", 
                          "Progress", "Duration..in.seconds.", "Finished", 
                          "RecordedDate", "ResponseId", "DistributionChannel", 
                          "UserLanguage", "consent", "gender_3_TEXT", "mus_instrument_1_TEXT", 
                          "context_knowledge_6_TEXT", "context_associations", "feedback", 
                          "FL_19_DO_DMinorFuguefast", "FL_19_DO_DMinorFuguefast.Copy", "FL_19_DO_DMinorFuguemajor", 
                          "FL_19_DO_DMinorFuguenopedal", "FL_19_DO_DMinorFugueoriginal", "FL_19_DO_EMinorPreludenopedal", 
                          "FL_19_DO_EMinorPreludefast", "FL_19_DO_EMinorPreludeoriginal", "FL_19_DO_EMinorPreludeMajor", 
                          "FL_19_DO_BMajorPreludefast", "FL_19_DO_BMajorPreludeminor", "FL_19_DO_BMajorPreludeoriginal", 
                          "FL_19_DO_BMajorPreludenopedal", "FL_19_DO_AMajorFugueoriginal", "FL_19_DO_AMajorFuguefast", 
                          "FL_19_DO_AMajorFugueminor", "FL_19_DO_AMajorFuguenopedal", "context_knowledge_1", "context_knowledge_2", 
                          "context_knowledge_3", "context_knowledge_4", "context_knowledge_5", "context_knowledge_6")
ratings_data <- fulldata[,!(names(fulldata) %in% irrelevant_variables)]

#Table of Intercorrelations
#dichotomise occupation
fulldata$occupation_dic <- ifelse(fulldata$occupation %in% c("1" : "2"), 1,
                                  ifelse(fulldata$occupation %in% c("3" : "5"), 2,
                                         NA))
table(fulldata$occupation_dic)

intercorr_variables <- c("age", "gender", "occupation_dic", "education", "mus_instrument", "mus_lessons", "mus_lessons_1_TEXT", 
                         "mus_playing", "mus_listening", "div_musicians_1", "div_genres_1", "OMSI.1.question", "context_knowledge")
intercorr_data <- fulldata[, (names(fulldata) %in% intercorr_variables)]

intercorr_data$mus_lessons[is.na(intercorr_data$mus_lessons)] <- 0
intercorr_data$mus_lessons_1_TEXT[is.na(intercorr_data$mus_lessons_1_TEXT)] <- 0
intercorr_data$mus_playing[is.na(intercorr_data$mus_playing)] <- 0
#dichotomise gender
intercorr_data$gender[which(intercorr_data$gender == 3)] <- NA
#invert mus_instrument
intercorr_data$mus_instrument <- ifelse(intercorr_data$mus_instrument %in% 1, 2, 
                                        ifelse(intercorr_data$mus_instrument %in% 2, 1,
                                               NA))

intercorr <- rcorr(as.matrix(intercorr_data), type="spearman")
intercorr$r
format(intercorr$P, scientific=FALSE)

#Cast to long format
ratings_data$ID <- c(1:51)
ratings_data$ID <- factor(ratings_data$ID)

data_long_interim <- gather(ratings_data, key="condition", value="rating", dminfug_fast_valence_1:amajfug_pedl_auth_1, factor_key = TRUE)
data_long_interim <- separate(data_long_interim, condition, c("piece", "manipulation", "dimension", NA), sep="_", remove=TRUE)
data_long <- spread(data_long_interim, key="dimension", value="rating")

data_long$mode <- ifelse(data_long$manipulation == "min", "minor",
                         ifelse(data_long$manipulation == "maj", "major", 
                                ifelse(grepl("min", data_long$piece, fixed = TRUE), "minor", 
                                       ifelse(grepl("maj", data_long$piece, fixed = TRUE), "major", 
                                              NA))))

#remove duplicate manipulation used for reliability test
data_long_clean <- subset(data_long, !(manipulation %in% "fas1"))

#Correlations with dependent variables
#chose only numerical
ratings_corr_variables <- c("age", "gender", "occupation_dic", "mus_lessons", "mus_lessons_1_TEXT", 
                            "mus_playing", "mus_listening", "OMSI.1.question", "context_knowledge", 
                            "valence", "arousal", "auth")
ratings_corr_data <- data_long[, (names(data_long) %in% ratings_corr_variables)]

ratings_corr_data$mus_lessons[is.na(ratings_corr_data$mus_lessons)] <- 0
ratings_corr_data$mus_lessons_1_TEXT[is.na(ratings_corr_data$mus_lessons_1_TEXT)] <- 0
ratings_corr_data$mus_playing[is.na(ratings_corr_data$mus_playing)] <- 0
ratings_corr_data$gender[which(ratings_corr_data$gender == 3)] <- NA

ratings_corr <- rcorr(as.matrix(ratings_corr_data), type="spearman")
ratings_corr$r
format(ratings_corr$P, scientific=FALSE)

#To-Do: remove outliers?

##Plots----
#define nonmusicans and musicians by OMSI question
data_long_clean$OMSImusician <- ifelse (data_long_clean$OMSI.1.question %in% c("1" : "2"), 1,
                                        ifelse(data_long_clean$OMSI.1.question %in% c("3" : "6"), 2,
                                               NA))
table(data_long_clean$OMSImusician, data_long_clean$mus_instrument)

#Valence barplots for 4 conditions
valenceplot <- ggplot(data_long_clean, aes(x = manipulation, y = valence, fill = manipulation))+ 
  geom_boxplot()+
  facet_grid(~OMSImusician)+
  scale_fill_brewer(palette = "Set1")+
  theme_classic()
valenceplot

#Arousal barplots for 4 conditions
arousalplot <- ggplot(data_long_clean, aes(x = manipulation, y = arousal, fill = manipulation))+
  geom_boxplot()+
  facet_grid(~OMSImusician)+
  scale_fill_brewer(palette = "Set2")+
  theme_classic()
arousalplot

#Authenticity barplots for 4 conditions
authplot <- ggplot(data_long_clean, aes(x = manipulation, y = auth, fill = manipulation))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set3")+
  theme_classic()
authplot

#To-Do: Make scatterplot of means of manipulations

##ANCOVA Analysis----
data_long_clean$auth_centr <- data_long_clean$auth - mean(data_long_clean$auth)
data_long_clean$mus_playing_centr <- data_long_clean$mus_playing - mean(data_long_clean$mus_playing)

#valence
val_model <- aov_ez(id="ID", dv="valence", between="manipulation", covariate=c("gender", "auth_centr"), factorize=FALSE, data=data_long_clean)
val_model$Anova
val_em <- emmeans(val_model, specs = "manipulation")
contrast(val_em, method="tukey")

#arousal
arousal_model <- aov_ez(id="ID", dv="arousal", between="manipulation", covariate=c("auth_centr"), factorize=FALSE, data=data_long_clean)
arousal_model$Anova
arousal_em <- emmeans(arousal_model, specs = "manipulation")
contrast(arousal_em, method="tukey")
