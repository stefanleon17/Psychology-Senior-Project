# Descriptive Statistics

count(variables[variables$cond=="easy",])

count(variables[variables$cond=="hard",])

#Gender Distribution
male = count(variables[variables$gender==1,])/67*100
female = count(variables[variables$gender==2,])/67*100
nb = count(variables[variables$gender==3,])/67*100
not_mentioned = count(variables[variables$gender==4,])/67*100

#Score Distribution
ggplot(variables, aes(x=score)) + 
  geom_freqpoly() + 
  xlab("Score") + ylab("Frequency") + ggtitle("Fig. 1.2 - Score Distribution")

# Normalizing Score Distribution
test <- variables
test$score <- log(test$score)

ggplot(test, aes(x=score)) + 
  geom_freqpoly() + 
  xlab("Score") + ylab("Frequency") + ggtitle("Fig. 1.2 - Score Distribution after Logarithmic Normalization")

skew(variables$score)
skew(test$score)

ggplot(test, aes(x=score, y=cond, fill=cond)) + 
  geom_boxplot() +
  xlab("Score")+ylab("Difficulty")+ggtitle("Fig. 2 - Score Distribution Across Conditions")+
  theme(legend.position="none")

# Age
mean(variables$age)
sd(variables$age)

# Gaming Frequency Distribution
count(variables[variables$freq==1,])/67*100
count(variables[variables$freq==2,])/67*100
count(variables[variables$freq==3,])/67*100
count(variables[variables$freq==4,])/67*100
count(variables[variables$freq==5,])/67*100
count(variables[variables$freq==6,])/67*100

# Experience with Task
count(variables[variables$exp==2,])/67*100
count(variables[variables$exp==3,])/67*100

# Experimental Group Distribution
count(variables[variables$easy==TRUE,])
count(variables[variables$easy==FALSE,])
