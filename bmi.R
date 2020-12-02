#load packages

library("tidyverse")
library("glue")
library("conflicted")
#fix conflicts
conflict_prefer("filter", "dplyr")


# needs dplyr version 1.0 or development version from github 
stopifnot(packageVersion("dplyr") >= "0.8.99.9003") 
# install with remotes::install_github("tidyverse/dplyr")


# load functions
source("R/functions.R")

#set default theme
theme_set(theme_bw())

#set seed from random.org
#set.seed(9227)

# load data

raw <- read_delim(
  "Study	Arm	height_mean	height_sd	mass_mean	mass_sd	bmi_mean	bmi_sd
4	2	153.3	5.2	85.3	5.3	22.1	3.6
9	1	155.3	5.2	75.3	5.4	30.1	3.3
11	1	155.3	6.2	76.3	6.4	33.9	3.1
4	1	158.3	6.1	64.3	6.4	23.1	3.2
13	1	158.3	5.1	80.3	5.4	31.1	2.9
15	1	158.3	5.1	80.3	5.4	30.1	2.9
11	2	158.3	5.5	77.1	4.2	34.2	2.6
16	2	159.3	4.0	62.1	5.2	24.6	3.2
7	1	160.3	6.2	78.3	6.4	30.5	3.1
14	1	160.3	5.1	68.3	6.4	26.6	3.1
1	2	161.3	5.1	81.3	5.4	26.1	3.1
2	1	163.3	6.1	98.3	6.4	28.1	3.2
12	1	163.3	6.1	98.3	6.4	28.1	3.2
14	2	163.3	4.9	72.1	5.2	28.1	3.8
16	1	163.3	6.1	63.3	6.4	23.8	3.1
6	1	166.3	5.2	85.3	6.4	29.1	3.1
13	2	155.1	4.2	79.1	4.2	29.1	3.1
15	2	155.1	4.2	79.1	4.2	29.1	3.1
7	2	158.1	5.8	81.1	4.2	32.5	2.9
12	2	158.1	5.2	91.1	4.2	27.1	3.1
9	2	159.1	5.3	79.1	4.2	31.6	2.6
15	4	160.1	3.5	74.1	5.1	26.2	3.4
6	2	162.1	5.1	88.1	4.2	30.1	2.1
8	1	158.0	12.3	68.0	3.2	np	
8	2	164.0	14.5	65.0	4.2	np	
1	1	168.0	4.9	78.9	6.2	25.5	3.4
15	3	157.2	5.9	75.1	4.1	26.2	5.1
3	1	np		np		24.1	3.2
3	2	np		np		23.1	3.3
3	3	np		np		25.1	3.1
17	1	np		np		26.5	2.8
17	2	np		np		27.1	1.9
18	1	np		np		29.9	3.1
18	2	np		np		30.5	2.8", delim = "\t", na = "np")

# clean data
raw <- raw %>% 
  na.omit() %>%
  mutate(n = 100) %>% # assume 100 patients - needs fixing
  mutate(across(matches("height"), `/`, 100)) # height in m


# naive estimate of BMI from mean mass and height
naive <- raw %>% 
  mutate(naive_bmi = BMI(mass_mean, height_mean))

simulated_BMI <- raw %>% 
  select(-starts_with("bmi")) %>%
  crossing(z = 1:1000) %>%
  select(-z) %>%
  rowwise() %>%
  mutate(bmi = bmi_sim(mass_mean, mass_sd, height_mean, height_sd, n)) %>%
  unpack(bmi) 

simulated_BMI %>%
  ggplot(aes(x = mean, y = sd)) +
  geom_point() +
  geom_point(data = raw, aes(x = bmi_mean, y = bmi_sd), colour = "red") +
  geom_vline(aes(xintercept = round(naive_bmi, 1)), naive, colour = "blue") + #naive estimate
  facet_wrap(~ glue("Study {Study}  Arm {Arm}"), scales = "free_x")


# calculate z scores
z_scores <- simulated_BMI %>% 
  group_by(Study, Arm) %>%
  summarise(sd = sd(mean)) %>%
  left_join(naive) %>%
  mutate(z = (bmi_mean - naive_bmi) / sd) 

z_scores %>%
  filter(abs(z) < 5) %>% # filter away extreme values
  ggplot(aes(x = z)) +
  geom_histogram() 


# probablity of co-incidence - will be sensitive to n  
simulated_BMI %>% 
  left_join(naive) %>% 
  group_by(Study, Arm) %>%
  summarise(prop = mean(round(mean, 1) == round(naive_bmi, 1))) %>%
  ungroup()

1 - pbinom(0:5, size = 11, prob = 0.15)
