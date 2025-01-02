library(tidyverse)

num_set <- read.csv("C:\\Users\\stefa\\Downloads\\Psych Senior Project Responses\\143_num_processed.csv")

to_rm <- filter(num_set, is.na(s1))

data <- anti_join(num_set,to_rm)

# GEQ Scoring
data = data %>% 
  mutate(challenge = (g11+g23+g26+g32+g33)/5) %>% 
  mutate(tension = (g22+g24+g29)/3) %>% 
  mutate(pos_affect = (g1+g4+g6+g14+g20)/5) %>% 
  mutate(neg_affect = (g7+g8+g9+g16)/4)

# Self-Efficacy and NfC Scoring
data = data %>% 
  mutate(nfc = n1+n2-n3-n4-n5+n6-n7-n8-n9+n10+n11-n12+n13+n14) %>% 
  mutate(efficacy = -s1+s2+s3+s4+s5)

# Normalizing Score
data$score_easy[is.na(data$score_easy)] <- 0
data$score_hard[is.na(data$score_hard)] <- 0

data <- data %>% 
  mutate(easy = (score_hard == 0), hard = (score_easy == 0)) %>% 
  mutate(score = score_easy + score_hard, .keep = "unused")

variables <- data.frame(age = c(data$age), gender = c(data$gender), freq = c(data$gaming_freq),
                        exp = c(data$exp), easy = c(data$easy),
                        score = c(data$score), challenge = c(data$challenge), 
                        tension = c(data$tension), pos_affect = c(data$pos_affect),
                        neg_affect = c(data$neg_affect), efficacy = c(data$efficacy), 
                        nfc = c(data$nfc))

variables = variables %>% 
  mutate(cond=ifelse(variables$easy==TRUE, "easy", "hard")) %>% 
  mutate(easy=NULL)

remove(to_rm)
