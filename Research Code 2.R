library(tidyverse)
library(stargazer)
library(AER)
library(readxl)
library(ggplot2)
library(dplyr)

WNBA <- read_excel("ECON 551 - Research Data.xlsx", sheet = "WNBA", skip = 1)
NBA <- read_excel("ECON 551 - Research Data.xlsx", sheet = "NBA", skip = 1)

log(WNBA$Base_Salary)

WNBA_Model <- lm(formula = log(`Base_Salary`) ~ FGM + FGA + `FG%` + `3PTM` + `3PTA` + `3P%` + FTM + FTA + `FT%` + REB + AST + BLK + STL + PF + log(PTS+1),
                 data = WNBA)

summary(WNBA_Model)

NBA_Model <- lm(formula = log(`Base_Salary`) ~ FGM + FGA + `FG%` + `3PTM` + `3PTA` + `3P%` + FTM + FTA + `FT%` + REB + AST + BLK + STL + PF + PTS,
                data = NBA)

summary(NBA_Model)

ggplot(data=WNBA,
       aes(x= PTS, y = log(`Base_Salary`)))+
  geom_point(aes(color=`FG%`))+
  geom_smooth(method="lm")

ggplot(data=NBA,
       aes(y=log(Base_Salary), x= `PTS`))+
  geom_point(aes(color=`FG%`))+
  geom_smooth(method="lm")

combined_data <- bind_rows(mutate(WNBA, League = "WNBA"), mutate(NBA, League = "NBA"))

ggplot(data=combined_data,
       aes(y=log(`Base_Salary`), x = `PTS`))+
  geom_point(aes(color=`FG%`, shape= League))+
  geom_smooth(method="lm")

stargazer(WNBA_Model, NBA_Model, type="text")
