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

#help(specify)
gss %>%  specify(response=age) %>% class()
gss %>%  specify(response=age, explanatory=partyid) 
#gss %>%  specify(age ~ partyid) #numeric
#gss %>%  specify(response=partyid) #multi-factor
#gss %>%  specify(response=college, success="degree") #two-factor


#help(hypothesize)
gss %>% #multi-factor
  specify(college ~ partyid, success = "degree") %>%
  hypothesize(null = "independence")

gss %>% #numeric
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40)


#help(generate)
gss %>%
  specify(response = hours) %>% #numeric
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap")

gss %>%
  specify(partyid ~ age) %>% #two-factor
  hypothesize(null = "independence") %>%
  generate(reps = 200, type = "permute")


#help(calculate)
gss %>%
  specify(response = hours) %>% #numeric
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

gss %>%
  specify(age ~ college) %>% #numeric
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate("diff in means", order = c("degree", "no degree"))



## Other Utilities

# find the point estimate
point_estimate <- gss %>%
  specify(response = hours) %>%
  calculate(stat = "mean")

# generate a null distribution
null_dist <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

null_dist %>%
  visualize()

null_dist %>%
  visualize() +
  shade_p_value(obs_stat = point_estimate, direction = "two-sided")


# get a two-tailed p-value
p_value <- null_dist %>%
  get_p_value(obs_stat = point_estimate, direction = "two-sided")

p_value





