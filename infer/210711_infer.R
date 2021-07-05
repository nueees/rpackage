install.packages("statsr")
library(infer);library(ggplot2);library(dplyr);library(statsr);


# [infer 기본 사용]
# 1.코딩기반 t-검정을 수행할 경우 df이 δ∗에 해당되어 데이터에서 사전에 계산해 놓는다.
# 2.가설검정 공식을 specify 함수에 명세한다.
# 3.귀무가설을 hypothesize 함수에서 적시한다.
# 4.컴퓨터에서 모의실험 난수를 generate에서 생성시킨다.
# 5.검정 통계량을 calculate 함수에 명시한다.



#######################bankstudy###############################


# 0. 환경설정 -----
library(infer)
library(tidyverse)
library(cowplot)
library(extrafont)
loadfonts()

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
dsex_hat <- bankstudy %>%
  group_by(gender) %>%
  summarize(prob = mean(promote == "yes")) %>%
  pull(prob) %>% diff()

dsex_null <- bankstudy %>%
  specify(promote ~ gender, success = "yes") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("male", "female"))

ggplot(dsex_null, aes(x = stat)) +
  geom_density(bw=0.05) +
  geom_vline(xintercept = dsex_hat, color="red") +
  scale_x_continuous(limits = c(-0.5, 0.5))

dsex_null %>%
  summarize(mean(stat > dsex_hat)) %>%
  pull() * 2 

## 1.4. 신뢰구간 -----
### tidyverse + 컴퓨팅
dsex_boot <- bankstudy %>%
  specify(promote ~ gender, success = "yes") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("female", "male"))

c(lower = dsex_hat - 2 * sd(dsex_boot$stat), 
  upper = dsex_hat + 2 * sd(dsex_boot$stat))

### 전통적 방식
n <- nrow(bankstudy)
dsex_prop <- bankstudy %>%
    group_by(gender) %>%
    summarize(prob = mean(promote == "yes")) %>%
    pull(prob)

prop.test(x=c(dsex_prop[2], dsex_prop[1]) * n, n = c(n, n),  alternative = "two.sided", correct=FALSE) %>%
    broom::tidy()




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

t.test(g1, g2, conf.level = 0.95, alternative = "two.sided") #p:0.008251

### tidyverse 통계검정
drugstudy %>% 
  t_test(time ~ group)


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
drug_boot <- drugstudy %>%
  specify(time ~ group) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("treatment", "control")) 

drug_boot[!complete.cases(drug_boot$stat),]
c(lower = drug_diff_hat - 2 * sd(drug_boot$stat,na.rm=T), 
  upper = drug_diff_hat + 2 * sd(drug_boot$stat,na.rm=T))


















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





