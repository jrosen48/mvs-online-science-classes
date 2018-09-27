# Explanatory models

library(tidyverse)
library(lavaan)
library(lme4)

f <- here::here("online-science-motivation-w-disc.csv")
d <- read_csv(f)

f1 <- here::here("all-vars.csv")
d1 <- read_csv(f1)

dsent <- select(d, student_ID, course_ID, posemo, negemo, cogproc, n)

d1 <- left_join(d1, dsent)

# this filters the data to not include the third semester...
# d1 <- d1 %>%
#     filter(!str_detect(d1$course_ID, "S217"))

d %>% 
    select(pre_int, pre_uv, pre_percomp, time_spent, final_grade) %>% 
    corrr::correlate() %>% 
    corrr::shave() %>% 
    corrr::fashion()

# m1 <- '
# uv =~ q1 + q4 + q5 + q8 + q10
# int =~ q2 + q6 + q9
# pc =~ q3 + q7
# '
# 
# out1 <- sem(m1, data = d)
# summary(out1, fit.measures = T, standardized = T)
# 
# m2 <- '
# int =~ q2 + q6 + q9
# pc =~ q3 + q7
# '
# 
# out2 <- sem(m2, data = d1)
# summary(out2, fit.measures = T, standardized = T)

# Model 1

m1 <- '
# measurement models
pre_uv =~ q1 + q4 + q5 + q8 + q10
pre_int =~ q2 + q6 + q9
pre_pc =~ q3 + q7
# regressions
ts_60 ~ pre_int + pre_pc + pre_uv + male_dummy + n + cogproc
final_grade ~ ts_60 + pre_int + pre_pc + pre_uv + male_dummy + n + cogproc
# # indirect effect (a*b)
# ab := a*b
# # total effect
# total := c + (a*b)
'

out1 <- sem(m1, data = d1)
summary(out1, standardized = T, fit.measures = T)

# Model 2

model <- '
        level: 1
fw =~ y1 + y2 + y3
fw ~ x1 + x2 + x3
level: 2
fb =~ y1 + y2 + y3
fb ~ w1 + w2
'

m1 <- '
# measurement models
pre_uv =~ q1 + q4 + q5 + q8 + q10
pre_int =~ q2 + q6 + q9
pre_pc =~ q3 + q7
# text =~ posemo + cogproc
# regressions
ts_60 ~ pre_int + pre_pc + pre_uv + male_dummy + intervention_dummy + cogproc 
final_grade ~ ts_60 + pre_int + pre_pc + pre_uv + male_dummy + intervention_dummy + cogproc
# # indirect effect (a*b)
# ab := a*b
# # total effect
# total := c + (a*b)
'

out1 <- sem(m1, data = d1)
summary(out1, standardized = T, fit.measures = T)

library(lavaan.survey)

group <- svydesign(ids= ~ course_ID, data = d1)
out_robust <- lavaan.survey(out1, group)
summary(out_robust, standardized = T, fit.measures = T)

