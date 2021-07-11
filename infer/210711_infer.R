library(infer);library(ggplot2);library(dplyr);library(statsr);

library(tidyverse);library(cowplot);library(extrafont);library(tidyr);library(caret);
loadfonts();

# [infer 기본 사용]
# 1.코딩기반 t-검정을 수행할 경우 자유도가 delta(두샘플의 평균차이)∗어쩌구라서 사전에 계산해 놓는다.
# 2.가설검정 공식을 specify 함수에 명세한다.
# 3.귀무가설을 hypothesize 함수에서 적시한다.
# 4.컴퓨터에서 모의실험 난수를 generate에서 생성시킨다.
# 5.검정 통계량을 calculate 함수에 명시한다.



#######################icecream###############################

# 가장 간단하게 0523 지은님 아이스크림 코드로 검증.
#setwd("C:/Users/Administrator/Documents/R/Kaggle/rpackage/data")
icecream <- read.csv("Ch0701.OST.csv", stringsAsFactors=TRUE, encoding="UTF-8")
str(icecream)
summary(icecream)

icecream.r <- round(icecream$weight,1)
boxplot(icecream.r)
hist(icecream.r)

# 1.2 평균 아이스크림 무게가 320g과 같은지 검정하고 그 결과를 print함수로 출력하라.(양측검정, 유의수준 0.05)
icecream.t <- t.test(icecream, alternative="two.sided", mu=290, conf.level=0.95)
print(icecream.t) # 귀무가설(평균320)


### tidyverse로 기본적인 통계검정
icecream %>% 
  t_test(response=weight,alternative="two.sided", mu=290, conf_level=0.95)
# p_value ; 0.00778 귀무 기각

# find the point estimate 추정치 확인
estmt_ice <- icecream %>% specify(response=weight) %>% 
  calculate(stat="mean")
# estmt_ice ; 295 (mean(icecream$weight))

str(icecream)
## hypothesize 귀무가설 만들고
null_ice2<- icecream %>% specify(response=weight) %>% 
  hypothesize(null="point", mu=290) %>% 
  generate(reps=1, type="bootstrap") %>% 
  calculate(stat = "mean")

## 그래프로 확인
null_ice %>% visualise()
null_ice %>% visualise()+shade_p_value(obs_stat=estmt_ice, direction="two-sided")
pvalue_ice <- null_ice %>% get_p_value(obs_stat=estmt_ice, direction="two-sided")
# pvalue_ice ; 0.004 귀무가설 기각

# calculate the confidence interval around the point estimate 95신뢰구간se
null_ice %>%
  get_confidence_interval(point_estimate = estmt_ice,
                          # at the 95% confidence level
                          level = .95,
                          # using the standard error
                          type = "se")
# lower_ci ; 291, upper_ci ; 299



#######################bottle##################################

bottles <- read.csv("bottles.csv", stringsAsFactors=TRUE, encoding="UTF-8")

str(bottles)
mean(bottles$Volume) #491.5750

### tidyverse로 기본적인 통계검정
bottles %>% 
  t_test(response=Volume,alternative="two.sided", mu=500, conf_level=0.95)
# p_value ; 0.145 귀무 기각

# find the point estimate 추정치 확인
estmt_bottles <- bottles %>% specify(response=Volume) %>% 
  calculate(stat="mean")
# estmt_bottles ; 492 (mean(icecream$weight))


## hypothesize 귀무가설 만들고
null_bottles <- bottles %>% specify(response=Volume) %>% 
  hypothesize(null="point", mu=500) %>% 
  generate(reps=1000, type="bootstrap") %>% 
  calculate(stat = "mean")

## 그래프로 확인
null_bottles %>% visualise()
null_bottles %>% visualise()+shade_p_value(obs_stat=estmt_bottles, direction="two-sided")
pvalue_bottles <- null_bottles %>% get_p_value(obs_stat=estmt_bottles, direction="two-sided")
# pvalue_ice ; 0.004 귀무가설 기각

# calculate the confidence interval around the point estimate 95신뢰구간se
null_bottles %>%
  get_confidence_interval(point_estimate = estmt_bottles,
                          # at the 95% confidence level
                          level = .95,
                          # using the standard error
                          type = "se")
# lower_ci ; 481, upper_ci ; 502



#######################bankstudy#######################
# 두집단.독립



# 1. 성차별(Gender Discrimination) -----
## 1.1. 데이터 -----
load(url("http://bit.ly/2DffE7b"))
str(bankstudy)

## 1.2. 탐색적 데이터분석 -----
### 교차표 
bankstudy %>% 
  count(promote, gender) %>% 
  spread(gender, n)
bankstudy %>%
  group_by(gender) %>%
  summarize(promote_prob = mean(promote == "yes"))
# gender promote_prob
# female        0.583
# male          0.875

### 시각화
bankstudy %>% 
  ggplot(aes(x=gender, fill = promote)) +
  geom_bar()

## 1.3. 통계적 검정 -----
### 기본 통계검정
chisq.test(bankstudy$gender, bankstudy$promote)

### tidyverse 통계검정
bankstudy %>% 
  chisq_test(promote ~ gender) # p=0.0513


### tidyverse + 컴퓨팅 통계검정
promote_y <- bankstudy %>%  group_by(gender) %>%
  summarise(prob = mean(promote == "yes")) 
dsex_hat <- promote_y %>% pull(prob) %>% diff()
# 관측된 평균 차이 : 0.2916667
 
?calculate
# calculate(x,
#   stat = c("mean", "median", "sum", "sd", "prop", "count", "diff in means","diff in medians", "diff in props", "Chisq", "F","slope", "correlation", "t", "z","ratio of props", "odds ratio"),
#   order = NULL,
#   ...)
bank_null <- bankstudy %>%
  specify(promote ~ gender, success = "yes") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute")

promote_y2 <- bank_null %>% group_by(gender) %>%  summarise(prob=mean(promote=="yes"))

promote_y2 %>% pull(prob) %>% diff()
# 가설 평균 차이 : ...

dsex_null <- bankstudy %>%
  specify(promote ~ gender, success = "yes") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("male", "female"))
# 0.292, -0.0417, 0.208, -0.0417 ...

# dsex_hat : 0.2916667
ggplot(dsex_null, aes(x = stat)) +
  geom_density(bw=0.05) +
  geom_vline(xintercept = dsex_hat, color="red") +
  scale_x_continuous(limits = c(-0.5, 0.5))

dsex_null %>%
  summarize(mean(stat > dsex_hat)) %>%
  pull() * 2 
# 0.004 * 2

## 1.4. 신뢰구간 -----
### tidyverse + 컴퓨팅
# dsex_boot <- bankstudy %>%
#   specify(promote ~ gender, success = "yes") %>%
#   generate(reps = 1000, type = "bootstrap") %>%
#   calculate(stat = "diff in props", order = c("female", "male"))
# 
# c(lower = dsex_hat - 2 * sd(dsex_boot$stat), 
#   upper = dsex_hat + 2 * sd(dsex_boot$stat))

dsex_null %>%
  get_confidence_interval(point_estimate = dsex_hat,
                          # at the 95% confidence level
                          level = .95,
                          # using the standard error
                          type = "se")
#   lower_ci  upper_ci
#   0.0477    0.536

### 전통적 방식
# n <- nrow(bankstudy)
# dsex_prop <- bankstudy %>%
#     group_by(gender) %>%
#     summarize(prob = mean(promote == "yes")) %>%
#     pull(prob)
# 
# prop.test(x=c(dsex_prop[2], dsex_prop[1]) * n, n = c(n, n),  alternative = "two.sided", correct=FALSE) %>%
#     broom::tidy()




###############################drugstudy########################

# 2. 신약 효과 -----
## 1.1. 데이터 -----
load(url("http://bit.ly/2mDUX9K"))
str(drugstudy)
## 1.2. EDA -----
### 시각화
ggplot(drugstudy, aes(x = time, fill = group)) +
  geom_dotplot()


## 1.3. 통계적 검정 -----
### 기본 통계검정
g1 <- drugstudy %>% 
  filter(group == "treatment") %>% 
  select(time) %>%  pull

g2 <- drugstudy %>% 
  filter(group == "control") %>% 
  select(time) %>%  pull

t.test(g1, g2, conf.level = 0.95, alternative = "two.sided") #p:0.008251 귀무가설 기각

### tidyverse 통계검정
drugstudy %>% 
  t_test(time ~ group)

str(drugstudy)
### tidyverse + 컴퓨팅 통계적 검정
drug_diff_hat <- drugstudy %>%
  group_by(group) %>%
  summarize(xbar = mean(time)) %>%
  pull(xbar) %>%
  diff()

drug_null <- drugstudy %>%
  specify(time ~ group) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("treatment", "control"))

ggplot(drug_null, aes(x = stat)) +
  geom_density() +
  geom_vline(xintercept = drug_diff_hat, color="red")

drug_null %>%
  summarize(mean(stat > drug_diff_hat)) %>%
  pull() * 2

## 1.4. 신뢰구간 -----
# drug_boot <- drugstudy %>%
#   specify(time ~ group) %>%
#   generate(reps = 1000, type = "bootstrap") %>%
#   calculate(stat = "diff in means", order = c("treatment", "control")) 
# 
# drug_boot[!complete.cases(drug_boot$stat),]
# c(lower = drug_diff_hat - 2 * sd(drug_boot$stat,na.rm=T), 
#   upper = drug_diff_hat + 2 * sd(drug_boot$stat,na.rm=T))
drug_null %>%
  get_confidence_interval(point_estimate=drug_diff_hat,
                          # at the 95% confidence level
                          level = .95,
                          # using the standard error
                          type = "se")



########################GSS_CAT##################################

# 0. 환경설정 -----
library(infer)
library(tidyverse)
library(cowplot)
library(extrafont)
loadfonts()

# 1. 데이터 -----
## 1.1. 데이터 정제작업 -----
gss_cat_df  <- gss_cat %>% 
  filter(year==2014) %>% 
  mutate(marital = fct_lump(marital, 1),
         race = fct_lump(race, 1)) %>% 
  mutate(marital = fct_recode(marital, Non_Married = "Other"),
         race    = fct_recode(race, Non_White = "Other"))

## 1.2. 교차표 -----
gss_cat_df %>% 
  count(marital, race) %>% 
  spread(marital, n)

## 1.3. 시각화 -----
marital_orig_g <- gss_cat_df %>% 
  ggplot(aes(x=race, fill=marital)) +
  geom_bar() +
  theme(legend.position = "none")

marital_fill_g <- gss_cat_df %>% 
  ggplot(aes(x=race, fill=marital)) +
  geom_bar(position = "fill")

plot_grid(marital_orig_g, marital_fill_g)


## 2. 통계검정 -----
### 2.1. Base 함수 
chisq.test(gss_cat_df$marital, gss_cat_df$race)

### 2.2. tidyverse 방식
gss_cat_df %>% 
  infer::chisq_test(formula = marital ~ race)

## 3. 코딩기반 통계 검정 -----
### 3.1. 관측점 통계량
obs_chisq <- gss_cat_df %>% 
  infer::chisq_test(formula = marital ~ race) %>% 
  select(statistic) %>% 
  pull()

### 3.2. 귀무가설 통계량
chisq_null <- gss_cat_df %>%
  specify(marital ~ race, success = "Married") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "Chisq", order=c("White", "Non_White"))

chisq_null %>% visualize(obs_stat = obs_chisq, direction = "greater")

### 3.3. p-값 계산
chisq_null %>% 
  summarize(p_value = mean(stat >= obs_chisq)) %>% 
  pull()

### 3.4. 이론 분포
gss_chisq_g <- gss_cat_df %>%
  specify(marital ~ race, success = "Married") %>%
  hypothesize(null = "independence") %>%
  # generate(reps = 1000, type = "permute") %>%
  calculate(stat = "Chisq", order=c("White", "Non_White")) %>% 
  visualize(method = "theoretical", obs_stat = obs_chisq, direction = "right") +
  labs(title="이론적인 카이제곱 귀무가설 분포") +
  theme_bw(base_family = "NanumGothic")

### 3.5. 두 그래프 겹치기
gss_chisq_gg <- gss_cat_df %>%
  specify(marital ~ race, success = "Married") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "Chisq", order=c("White", "Non_White")) %>% 
  visualize(method = "both", obs_stat = obs_chisq, direction = "right") +
  labs(title="이론+컴퓨팅 카이제곱 귀무가설 분포", x="") +
  theme_bw(base_family = "NanumGothic")

plot_grid(gss_chisq_g, gss_chisq_gg, nrow=1)







########################GSS######################################





#vignette(package="infer")

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


# start with the null distribution
null_dist %>%
  # calculate the confidence interval around the point estimate
  get_confidence_interval(point_estimate = point_estimate,
                          # at the 95% confidence level
                          level = .95,
                          # using the standard error
                          type = "se")



null_f_distn <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

null_f_distn_theoretical <- gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  calculate(stat = "F")

F_hat <- gss %>% 
  specify(age ~ partyid) %>%
  calculate(stat = "F")

visualize(null_f_distn_theoretical, method = "theoretical") +
  shade_p_value(obs_stat = F_hat, direction = "greater")

visualize(null_f_distn, method = "both") +
  shade_p_value(obs_stat = F_hat, direction = "greater")








