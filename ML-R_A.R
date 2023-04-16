
########################################
## R을 이용한 머신러닝과 텍스트마이닝 ##
##       (곽기영, 도서출판 청람)      ## 
########################################

#########################
## 부록. 데이터 전처리 ##
#########################

## A.1 분할–적용–결합

head(airquality)

library(dplyr)
filter(airquality, Month == 6)

filter(airquality, Month == 6, Temp > 90)
filter(airquality, Month == 6 & Temp > 90)
filter(airquality, Ozone > 80 | Temp > 90)

slice(airquality, 6:10)

slice_max(airquality, order_by=Ozone, n=3)
slice_min(airquality, order_by=Temp, n=3)

select(airquality, Month, Day, Temp)
select(airquality, Temp:Day)

select(airquality, Month, Day, everything())

arrange(airquality, Temp, Month, Day)
arrange(airquality, desc(Temp), Month, Day)

mutate(airquality, Temp.C=(Temp-32)/1.8, Diff=Temp.C - mean(Temp.C))

summarise(airquality, mean(Temp))
summarise(airquality, 
          Min=min(Temp, na.rm=TRUE),
          Median=median(Temp, na.rm=TRUE),
          Mean=mean(Temp, na.rm=TRUE),
          SD=sd(Temp, na.rm=TRUE),
          Max=max(Temp, na.rm=TRUE),
          N=n(),
          Distinct.Month=n_distinct(Month),
          First.Month=first(Month),
          Last.Month=last(Month))	

air.group <- group_by(airquality, Month)
class(air.group)
air.group

summarise(air.group, Mean.Temp=mean(Temp, na.rm=TRUE))
summarise(air.group,
          Number.of.Days=n(),
          Mean.Temp=mean(Temp, na.rm=TRUE),
          SD.Temp=sd(Temp, na.rm=TRUE))

air.ungroup <- ungroup(air.group)
class(air.ungroup)
summarise(air.ungroup, Mean.Temp=mean(Temp, na.rm=TRUE))

iris %>% head
1:10 %>% mean

a1 <- select(airquality, Ozone, Temp, Month)
a2 <- group_by(a1, Month)
a3 <- summarise(a2, 
                Mean.Ozone=mean(Ozone, na.rm=TRUE), 
                Mean.Temp=mean(Temp, na.rm=TRUE))
a4 <- filter(a3, Mean.Ozone > 30 | Mean.Temp > 70)
a5 <- arrange(a4, desc(Mean.Temp))
a6 <- left_join(a5, tibble(Month=1:12, Month.Name=month.name), by="Month")
a6

air <- airquality %>% 
  select(Ozone, Temp, Month) %>% 
  group_by(Month) %>% 
  summarise(Mean.Ozone=mean(Ozone, na.rm=TRUE), 
            Mean.Temp=mean(Temp, na.rm=TRUE)) %>% 
  filter(Mean.Ozone > 30 | Mean.Temp > 70) %>% 
  arrange(desc(Mean.Temp)) %>% 
  left_join(tibble(Month=1:12, Month.Name=month.name), by="Month")
air

## A.2 반복 적용

x <- matrix(1:20, nrow=4, ncol=5)
x

apply(X=x, MARGIN=1, FUN=max)

apply(x, 2, min)

exams <- list(s1=c(78, 89, 91, 85, 95, 98),
              s2=c(85, 86, 97, 99, 90),
              s3=c(98, 96, 89, 90, 93, 85, 92),
              s4=c(98, 96, 91, 88, 93, 99))
exams

lapply(exams, length)
sapply(exams, length)

sapply(exams, mean)
sapply(exams, sd)

sapply(exams, range)

lapply(iris, class)

sapply(iris, class)

exams <- list(s1=c(78, 89, 91, 85, 95, 98),
              s2=c(85, 86, 97, 99, 90),
              s3=c(98, 96, 89, 90, 93, 85, 92),
              s4=c(98, 96, 91, 88, 93, 99))
exams
library(purrr)
map(.x=exams, .f=mean)

map_dbl(exams, mean)

map_dbl(exams, mean, trim=0.3)

exams %>% 
  map_dbl(mean, trim=0.3)

exams %>% 
  map(range) %>% 
  map_dbl(diff)

exams %>% 
  map(function(x) x*1.1) 

exams %>% 
  map(~.x*1.1)
exams %>% 
  map(~.*1.1)

fruits <- c("Apple", "Banana", "Strawberry")
fruits %>% 
  map_chr(paste, "Juice", sep="-")
fruits %>% 
  map_chr(~paste(.x, "Juice", sep="-"))

## A.3 형태 변환

library(tidyr)
head(airquality)
aq.long <- pivot_longer(data=airquality, cols=c(Ozone, Solar.R, Wind, Temp), 
                        names_to="Factor", values_to="Measurement")
head(aq.long)
tail(aq.long)

pivot_longer(data=airquality, cols=Ozone:Temp, 
             names_to="Factor", values_to="Measurement")
pivot_longer(data=airquality, cols=c(-Month, -Day), 
             names_to="Factor", values_to="Measurement")

aq.wide <- pivot_wider(data=aq.long, names_from=Factor, values_from=Measurement)
head(aq.wide)
tail(aq.wide)
