install.packages("statsr")
library(infer);library(ggplot2);library(dplyr);library(statsr);

vignette(package="infer")

data(gss)
summary(gss)
str(gss)
#partyid : 선호정당
#gss %>% group_by(partyid) %>% summarise(n=n())
plot(gss$weight)
#hompop : 가구당 사람수
#finrela : 가족소득수준

help(specify)
gss %>%  specify(response=age) %>% class()
gss %>%  specify(response=age, explanatory=partyid) 
#gss %>%  specify(age ~ partyid) #numeric
#gss %>%  specify(response=partyid) #multi-factor
#gss %>%  specify(response=college, success="degree") #two-factor


help(hypothesize)
gss %>% #multi-factor
  specify(college ~ partyid, success = "degree") %>%
  hypothesize(null = "independence")

gss %>% #numeric
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40)


help(generate)

gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap")

help(calculate)
