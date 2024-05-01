
#Kyle Hudson
#April 3024
#For Biol 499

###### Packages #####

library(tidyverse) 
library(ggplot2)
library(nnet)

###### Data #####

#Create new variables that are the max of left vs. right 
CountData <- read.csv("Chaetae_counts.csv") %>%
  mutate(One = pmax(L1, R1, na.rm = TRUE),
         Two = pmax(L2, R2, na.rm = TRUE),
         Three = pmax(L3, R3, na.rm = TRUE),
         Four = pmax(L4, R4, na.rm = TRUE),
         Five = pmax(L5, R5, na.rm = TRUE),
         Six = pmax(L6, R6, na.rm = TRUE))%>%
  drop_na(Five) %>%
  mutate(Family = as.factor(Family),
         Clade = as.factor(Clade))

#to view data types 
str(CountData)

##### Check left vs. right #####

#to make sure that left and right are pretty much the same

#All of the left and right Chaetae counts as lists
AllLeft <- list(c(CountData[ ,8], 
                  CountData[ ,10], 
                  CountData[ ,12],
                  CountData[ ,14],
                  CountData[ ,16]))

AllRight <- list(c(CountData[ ,9],
                   CountData[ ,11],
                   CountData[ ,13],
                   CountData[ ,15],
                   CountData[ ,17]))

t.test(AllLeft[[1]],
       AllRight[[1]],
       alternative = "two.sided",
       paired = T,
       conf.level = 0.95)


###### Stats #####

CountData$Fam2 <- relevel(CountData$Family, ref = "Lymnaeidae")

NomLog <- multinom(formula = Fam2 ~ One + Two + Three + Four + Five, 
                   data = CountData)

summary(NomLog)

fam.z <- summary(NomLog)$coefficients/summary(NomLog)$standard.errors
fam.z

fam.p <- (1 - pnorm(abs(z), 0, 1)) * 2
fam.p

t.test(One ~ Clade, data = CountData, paired = F, conf.level = 0.95)

t.test(Two ~ Clade, data = CountData, paired = F, conf.level = 0.95)

t.test(Three ~ Clade, data = CountData, paired = F, conf.level = 0.95)

t.test(Four ~ Clade, data = CountData, paired = F, conf.level = 0.95)

t.test(Five ~ Clade, data = CountData, paired = F, conf.level = 0.95)

##### Descriptive Stats ######

Fam_descriptive <- 
  CountData %>%
  group_by(Family)%>% 
  summarise(Chaet.mean1 = mean(One), Chaet.sd1 = sd(One),
            Chaet.mean2 = mean(Two), Chaet.sd2 = sd(Two),
            Chaet.mean3 = mean(Three), Chaet.sd3 = sd(Three),
            Chaet.mean4 = mean(Four), Chaet.sd4 = sd(Four),
            Chaet.mean5 = mean(Five), Chaet.sd5 = sd(Five)) %>%
  mutate(across((c(2,4,6,8,10)), round, 1),
         across((c(3,5,7,9,11)), round, 1)) %>%
  unite("Bundle One\u00B1 SD", Chaet.mean1:Chaet.sd1, 
        sep = " \u00B1 ", remove = TRUE) %>%
  unite("Bundle Two\u00B1 SD", Chaet.mean2:Chaet.sd2, 
        sep = " \u00B1 ", remove = TRUE) %>%
  unite("Bundle Three\u00B1 SD", Chaet.mean3:Chaet.sd3, 
        sep = " \u00B1 ", remove = TRUE) %>%
  unite("Bundle Four\u00B1 SD", Chaet.mean4:Chaet.sd4, 
        sep = " \u00B1 ", remove = TRUE) %>%
  unite("Bundle Five\u00B1 SD", Chaet.mean5:Chaet.sd5, 
        sep = " \u00B1 ", remove = TRUE)

Clade_descriptive <- 
  CountData %>%
  group_by(Clade) %>% 
  filter(!is.na(Clade)) %>%
  summarise(Chaet.mean1 = mean(One), Chaet.sd1 = sd(One),
            Chaet.mean2 = mean(Two), Chaet.sd2 = sd(Two),
            Chaet.mean3 = mean(Three), Chaet.sd3 = sd(Three),
            Chaet.mean4 = mean(Four), Chaet.sd4 = sd(Four),
            Chaet.mean5 = mean(Five), Chaet.sd5 = sd(Five)) %>%
  mutate(across((c(2,4,6,8,10)), round, 1),
         across((c(3,5,7,9,11)), round, 1)) %>%
  unite("Bundle One\u00B1 SD", Chaet.mean1:Chaet.sd1, 
        sep = " \u00B1 ", remove = TRUE) %>%
  unite("Bundle Two\u00B1 SD", Chaet.mean2:Chaet.sd2, 
        sep = " \u00B1 ", remove = TRUE) %>%
  unite("Bundle Three\u00B1 SD", Chaet.mean3:Chaet.sd3, 
        sep = " \u00B1 ", remove = TRUE) %>%
  unite("Bundle Four\u00B1 SD", Chaet.mean4:Chaet.sd4, 
        sep = " \u00B1 ", remove = TRUE) %>%
  unite("Bundle Five\u00B1 SD", Chaet.mean5:Chaet.sd5, 
        sep = " \u00B1 ", remove = TRUE)
  
  
