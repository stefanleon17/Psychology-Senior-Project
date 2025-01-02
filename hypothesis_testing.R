# Hypothesis Testing

library(psych)
library(report)
library(outliers)
library(corrplot)

theme_set(theme_grey(base_size = 13.5))

# Testing for Gender Differences 
gen <- t.test(score~gender, data=test[test$gender<3,])
report(gen)

sd(test$score[test$gender==1])
sd(test$score[test$gender==2])


#test$score <- scale(test$score)

describe(test$score)

ggplot(test, aes(x=score)) + 
  geom_freqpoly() + 
  geom_vline(xintercept = mean(test$score), color = "red")+
  geom_vline(xintercept = mean(test$score) - sd(test$score), color = "green") +
  geom_vline(xintercept = mean(test$score) + sd(test$score), color = "green")

test = test %>% 
  mutate(cond = variables$cond) %>% 
  mutate(easy = NULL)

### H1: Perceived Difficulty ~ Perceived Effort

result1 <- t.test(challenge ~ cond, data=test)
report(result1)

sd(test$challenge[test$cond=="easy"])
sd(test$challenge[test$cond=="hard"])

# accounting for frequency
result1_2 <- aov(challenge ~ cond*freq, data=test)
report(result1_2)

### H3: Perceived Difficulty ~ Self-Efficacy -- mostly no
ggplot(test, aes(x=efficacy, y=cond)) + 
  geom_boxplot()

result3 <- t.test(efficacy ~ cond, data=test)
report(result3)

sd(test$efficacy[test$cond=="easy"])
sd(test$efficacy[test$cond=="hard"])

result3_4 <- aov(efficacy ~ cond*score, data=test)
report(result3_4)

# considering frequency
result3_2 <- aov(efficacy ~ cond*freq, data=test)
report(result3_2)

result3_3 <- lm(efficacy ~ freq, data=test)        # statistically significant!!!
report(result3_3)


ggplot(test, aes(x=efficacy, y = as.character(freq)))+
  geom_boxplot()

### H2: Need for Cognition + Perceived Difficulty ~ Score
ggplot(test, aes(x=nfc))+
  geom_freqpoly()+
  xlab("Need for Cognition")+ylab("")+ggtitle("Fig. 3.1 - Need for Cognition scoring distribution")
skew(test$nfc)
count(test[test$nfc_cat=="low",])
count(test[test$nfc_cat=="high",])

ggplot(test, aes(x=nfc, y=score, color=cond))+
  geom_point()

ggplot(test, aes(x=nfc_cat, y=score, fill=cond))+
  geom_boxplot()+
  xlab("Need for Cognition")+ylab("Score")+ggtitle("Fig. 3.2 - Score Distribution across Need for Cognition and Perceived Difficulty")+
  labs(fill = "Perceived Difficulty")

x <- mean(variables$nfc)

low <- x - sd(variables$nfc)
high <- x + sd(variables$nfc)

test = test %>% 
  mutate(nfc_cat = "temp")

# 2 categories
test$nfc_cat = ifelse(test$nfc <= x, "low", "high")

# 3 categories
test$nfc_cat = ifelse(test$nfc <= low, "low", 
                      ifelse(test$nfc < high, "med", "high"))

ggplot(test) +
  aes(x=nfc_cat, y=score)+
  geom_boxplot() 
ggplot(test) +
  aes(x=cond, y=score)+
  geom_boxplot()

# main effects
result2_1 <- t.test(score ~ cond, data=test)
report(result2_1)

sd(test$score[test$cond=="easy"])
sd(test$score[test$cond=="hard"])

ggplot(test, aes(x=score, y=cond)) + 
  geom_boxplot() +
  xlab("Score")+ylab("Difficulty")+ggtitle("Fig. 2 - Score Distribution Across Conditions")


result2_2 <- t.test(score ~ nfc_cat, data=test)
report(result2_2)

sd(test$score[test$nfc_cat=="low"])
sd(test$score[test$nfc_cat=="high"])

# accidents
extra1 <- t.test(nfc ~ cond, data=test)      # statistically significant!!!
report(extra1)
extra2 <- lm (challenge ~ nfc, data = test)
report (extra2)

sd(test$nfc[test$cond=="easy"])
sd(test$nfc[test$cond=="hard"])

extra1_2 <- aov(nfc ~ score, data=test)
report(extra1_2)

ggplot(test, aes(x=nfc, y=score))+
  geom_point()

ggplot(test, aes(x=nfc, y=cond, fill=cond))+
  geom_boxplot()+
  theme(legend.position="none")+
  xlab("Need for Cognition")+ylab("Perceived Difficulty")+ggtitle("Fig. 5 - Need for Cognition Distribution across Experimental Conditions")

# interaction effect 2
ggplot(test, aes(x=as.character(freq), y=score, fill=cond))+
  geom_boxplot()+
  xlab("Gaming Frequency")+ ylab("Score") + 
  ggtitle("Fig. 4 - Score Distribution across Experimental Conditions and Gaming Frequency")+
  labs(fill = "Perceived Difficulty")

result2_3 <- aov(score ~ cond * nfc_cat,data=test, )
report(result2_3)

#accounting for frequency
result2_4 <- aov(score ~ cond*freq, data=test)   # statistically significant
report(result2_4)

result2_41 <- aov (score ~ freq, data = test[test$cond == "easy",])
report (result2_41)
result2_42 <- aov (score ~ freq, data = test[test$cond == "hard",])   # statistically significant
report (result2_42)

result2_43 <- t.test (score ~ cond, data = test[test$freq == 1,])
report (result2_43)
result2_44 <- t.test (score ~ cond, data = test[test$freq == 2,])
report (result2_44)
result2_45 <- t.test (score ~ cond, data = test[test$freq == 3,])
report (result2_45)
result2_46 <- t.test (score ~ cond, data = test[test$freq == 4,])
report (result2_46)
result2_47 <- t.test (score ~ cond, data = test[test$freq == 5,])
report (result2_47)
result2_48 <- t.test (score ~ cond, data = test[test$freq == 6,])
report (result2_48)

ggplot(test[test$cond=="hard",], aes(x=freq, y=score))+
  geom_point
ggplot(test[test$cond=="easy",], aes(x=freq, y=score))+
  geom_point()

mean(test$score[test$cond=="easy"&test$freq==1])
mean(test$score[test$cond=="hard"&test$freq==1])
sd(test$score[test$cond=="easy"&test$freq==1])
sd(test$score[test$cond=="hard"&test$freq==1])

mean(test$score[test$cond=="easy"&test$freq==6])
mean(test$score[test$cond=="hard"&test$freq==6])
sd(test$score[test$cond=="easy"&test$freq==6])
sd(test$score[test$cond=="hard"&test$freq==6])


### H4: "Enjoyment" ~ Perceived Difficulty

# Correlation Matrix: Score, Positive Affect, Negative Affect, Tension/Annoyance
matrix <- cor(data.frame(test$score,test$pos_affect,test$neg_affect,test$tension))

colnames(matrix) = rownames(matrix) = c("Score", "Positive Affect", "Negative Affect", "Tension/Annoyance")
  
`
corrplot(matrix, method = "color", type = "full", addCoef.col = "black", 
         tl.col = "black", cl.pos = "n")            


# Perceived Difficulty ~ Interest components t-tests
result4_1 <- t.test(pos_affect ~ cond, data=test)
report(result4_1)
result4_11 <- aov(pos_affect ~ cond * freq, data=test)
report(result4_11)

sd(test$pos_affect[test$cond=="easy"])
sd(test$pos_affect[test$cond=="hard"])

result4_2 <- t.test(neg_affect ~ cond, data=test)
report(result4_2)
result4_21 <- aov(neg_affect ~ cond * freq, data=test)
report(result4_21)

sd(test$neg_affect[test$cond=="easy"])
sd(test$neg_affect[test$cond=="hard"])

result4_3 <- t.test(tension ~ cond, data=test)
report(result4_3)
result4_31 <- aov(tension ~ cond * freq, data=test)
report(result4_31)

sd(test$tension[test$cond=="easy"])
sd(test$tension[test$cond=="hard"])


### Exploratory Hypotheses

# (1) Gaming Frequency and Score

result5_1 <- aov(score ~ freq_cat,data=test, )
report(result5_1)
result5_2 <- aov(score ~ cond * freq, data=test,)       # statistically significant!!!
report(result5_2)

# interaction effect

ggplot(test, aes(x=as.character(freq), y=score, color=cond))+
  geom_jitter()

scatter_easy <- 
  ggplot(test[test$cond=="easy",], aes(x=as.character(freq), y=score))+
  geom_jitter()+
  stat_summary(fun=mean, geom="line", aes(group=1))+
  ylim(2,8)

scatter_hard <- 
  ggplot(test[test$cond=="hard",], aes(x=as.character(freq), y=score))+
  geom_jitter()+
  stat_summary(fun=mean, geom="line", aes(group=1))+
  ylim(2,8)

grid.arrange(scatter_easy,scatter_hard,ncol=2)

test %>% 
  ggplot(aes(x=as.character(freq), y=score, fill = cond))+
  geom_boxplot() +
  stat_summary(fun=mean, geom="line", aes(group=1), linetype = "dashed")

x <- mean(variables$freq)

test = mutate(test, freq_cat = "temp")

# 2 categories
test$freq_cat = ifelse(test$freq <= x, "low", "high")

ggplot(test, aes(x=freq_cat, y=score))+
  geom_boxplot()


# (2) Perceived Difficulty and Self-Efficacy ~ Performance

result5_3 <- aov(score~cond*efficacy, data=test)
report(result5_3)

result5_4 <- lm(score~efficacy, data=test)        # statistically significant
report(result5_4)


x <- mean(variables$efficacy)

test = mutate(test, efficacy_cat = "temp")

# 2 categories
test$efficacy_cat = ifelse(test$efficacy <= x, "low", "high")

result5_5 <- aov(score~cond*efficacy_cat, data=test)
report(result5_5)

# (3) Score as a predictor of enjoyment?

# enjoyment predicted by Perceived Difficulty and Performance
result5_6 <- aov(pos_affect ~ cond*score, data=test)
report(result5_6)
result5_7 <- aov(neg_affect ~ cond*score, data=test)
report(result5_7)
result5_8 <- aov(tension ~ cond*score, data=test)
report(result5_8)

result5_9 <- lm(score ~ challenge, data=test)       # statistically significant!!!
report(result5_9)                                   # aov vs lm?

ggplot(test, aes(x=challenge, y=score))+
  geom_point()+
  stat_summary(fun=mean, geom="line", color="red", aes(group=1))+
  xlab("Challenge") + ylab("Score")+ggtitle("Fig. 6 - Linear Regression Between Challenge and Performance")



# follow-up
result5_10 <- lm(pos_affect ~ score, data=test)     # statistically significant
report(result5_10)
result5_11 <- lm (tension ~ score, data = test)     # statistically significant
report(result5_11)

# (4) NfC and Performance?

result6_1 <- aov(score~pos_affect*nfc, data=test)
report(result6_1)

