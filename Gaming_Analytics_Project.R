library(dplyr)
library(tidyr)
# install.packages('lubridate')
library(lubridate)
library(ggplot2)
library(gridExtra)


### Data Loading 
# 1. users
users <- read.csv("dataset/users.csv")
str(users) # all are factors
names(users)
# "udid","install_date","lang","country","hw_ver","os_ver"
head(users)
summary(users) 
colSums(is.na(users)) # 5 NAs from country

prop.table(table(users$install_date)) 

# 2. sessions
sessions <- read.csv("dataset/sessions.csv")
names(sessions)
# "udid","ts","date","session_num","last_session_termination_type"
head(sessions)
tail(sessions)
str(sessions) 
summary(sessions)
colSums(is.na(sessions))
# last_session_termination_type all NA, means users are all still playing, or the information not record


# 3. iaps (in-app-purchased)
iaps <- read.csv("dataset/iaps.csv")
names(iaps)
# "udid","ts","date","prod_name","prod_type","rev"
# product name means what type of product does a users purchased in game, it has gems, ChapterPasses and valuePack
head(iaps) 
str(iaps) 
summary(iaps)
# "rev" is the target variable, the users paid to buy "prod_type"
# product name means what type of product does a users purchased in game, it has gems, ChapterPasses and valuePack
colSums(is.na(iaps))


# 4. spendevent
spendevents <- read.csv("dataset/spendevents.csv")
names(spendevents)
# "udid","ts","date","story","chapter","spendtype","currency","amount"
head(spendevents) # each chapter has many story, when all story done, this chapter is over
str(spendevents)
summary(spendevents)
# negative sign in amount means spend currency, positive means earn currency from daily activities or special event,etc.
# amount: min -999999, max 165 earn is more difficult, spend is easy
# all the currency spends in event are gems
colSums(is.na(spendevents)) 


#str(spendevents) # character
#spendevents <- read.csv("dataset/spendevents.csv", stringsAsFactors = TRUE)


### Data Pre-processing ###

colSums(is.na(users))
# remove all NA values
users = users[complete.cases(users),]

# rename the column ts, date
names(iaps)
names(sessions)
names(spendevents)
iaps = rename(iaps, iaps_ts = ts, iaps_date = date)
sessions = rename(sessions, sessions_ts = ts, sessions_date = date)
spendevents = rename(spendevents, spend_ts = ts, spend_date = date)

# deal with the time & timestamp column
str(users)
users$install_date = as.Date(users$install_date, format = '%m/%d/%Y')
str(iaps)
iaps$iaps_ts = as.POSIXct(iaps$iaps_ts, format = '%m/%d/%y %H:%M') 
iaps$iaps_date = as.Date(iaps$iaps_date, format = '%m/%d/%Y')
str(sessions)
sessions$sessions_ts = as.POSIXct(sessions$sessions_ts, format = '%m/%d/%y %H:%M')
sessions$sessions_date = as.Date(sessions$sessions_date, format = '%m/%d/%Y')
str(spendevents)
spendevents$spend_ts = as.POSIXct(spendevents$spend_ts, format = '%m/%d/%y %H:%M')
spendevents$spend_date = as.Date(spendevents$spend_date, format = '%m/%d/%Y')


# add a column story_chapter
head(spendevents)
spendevents$story_chapter <- paste(spendevents$story, spendevents$chapter, sep = "_")

## converted_users
# the users in iapscopy are the converted users (rev means they paid money to buy product in game)
# we could create a column in users, if udid in iaps, it's converted-users, if udid not in iaps, it's un-converted users.
# select udid from iaps
purchased <- iaps %>% distinct(udid) %>% select(udid)
purchased_user <- purchased$udid
# Add a new column 'converted' to users_copy so that we can determine which user did purchased which did not
users$converted <- as.numeric(ifelse(users$udid %in% purchased_user,'1','0'))
converted_users <- users %>% filter(converted == 1)

## non_payers
non_payers <- users %>% filter(converted == 0) %>% select(udid)


### Data Exploration ###

# Part 1: Who are the users?

# 1. Overall converstion rate
mean(users$converted)
## In total 22571 users, only 6.76% of them are payers

# 2. By country
df2 <- users %>% select(country, converted) %>% group_by(country) %>%
  summarise(num_of_users = n(), conversion = mean(converted)) %>% 
  arrange(desc(num_of_users)) %>% head(14)
p21 <- ggplot(df2, aes(country, num_of_users, fill=country)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") +
  ggtitle('Distribution of Users by Country') +
  theme(plot.title = element_text(hjust = 0.5))
p22 <- ggplot(df2) + geom_line(aes(country, conversion), stat = 'identity', group=1) +
  geom_hline(yintercept=0.0676, linetype="dashed", color = "red")
grid.arrange(p21,p22,nrow=2)
## In terms of percentage of payers,
## Above average: US, GB, Australia
## About Average: Canada, Norway, Germany
## Below Average: Others(East & Middle East Asia), Indonesia, etc.

# 3. By language
df3 <- users %>% select(lang, converted) %>% group_by(lang) %>%
  summarise(num_of_users = n(), conversion = mean(converted)) %>% 
  arrange(desc(num_of_users)) %>% head(14)
p31 <- ggplot(df3, aes(lang, num_of_users, fill=lang)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") +
  ggtitle('Distribution of Users by Language') +
  theme(plot.title = element_text(hjust = 0.5))
p32 <- ggplot(df3) + geom_line(aes(lang, conversion), stat = 'identity', group=1) +
  geom_hline(yintercept=0.0676, linetype="dashed", color = "red")
grid.arrange(p31,p32,nrow=2)
## Majority of users are English-speaker. They also contribute the most to the number of payers
## Non-English speakers are more likely to be non-payers than avg 

# 4. By device type
df4 <- users %>% select(hw_ver, converted) %>% group_by(hw_ver) %>%
  summarise(num_of_users = n(), conversion = mean(converted)) %>% 
  arrange(desc(num_of_users)) %>% head(14)
p41 <- ggplot(df4, aes(hw_ver, num_of_users, fill=hw_ver)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") +
  ggtitle('Distribution of Users by Device Type') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p42 <- ggplot(df4) + geom_line(aes(hw_ver, conversion), stat = 'identity', group=1) +
  geom_hline(yintercept=0.0676, linetype="dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(p41,p42,nrow=2)

## Most users play the game on iPhone
## Users with new model device are more likely to pay for gaming comparing to those holding old models

# 5. By os version
df5 <- users %>% select(os_ver, converted) %>% group_by(os_ver) %>%
  summarise(num_of_users = n(), conversion = mean(converted)) %>% 
  arrange(desc(num_of_users)) %>% head(14)
p51 <- ggplot(df5, aes(os_ver, num_of_users, fill=os_ver)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") +
  ggtitle('Distribution of Users by Device Type') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p52 <- ggplot(df5) + geom_line(aes(os_ver, conversion), stat = 'identity', group=1) +
  geom_hline(yintercept=0.0676, linetype="dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(p51,p52,nrow=2)
## Majority of users are using iOS 9.x. They also contribute the most to the number of payers
## The percentage of payers among users using old version of OS are far below average

# 6. By install_date
df6 <- users %>% select(install_date, converted) %>% group_by(install_date) %>%
  summarise(num_of_users = n(), conversion = mean(converted)) %>% 
  arrange(desc(num_of_users)) 
p61 <- ggplot(df6, aes(as.factor(install_date), num_of_users, fill=install_date)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") +
  ggtitle('Distribution of Users by Installation Date') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p62 <- ggplot(df6) + geom_line(aes(as.factor(install_date), conversion), stat = 'identity', group=1) +
  geom_hline(yintercept=0.0676, linetype="dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(p61,p62,nrow=2)

## More installations occur on weekend (Mar 5 &6) than on weekdays

# Part 2: What do the users purchase?

# 7. By product
df7 <- iaps %>% group_by(prod_type) %>% summarise(number_of_orders = n(), 
                                                  revenue = sum(rev))
p71 <- ggplot(df7, aes(prod_type, number_of_orders)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") +
  ggtitle('Number of Orders by Product Type') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip() 
p72 <- ggplot(df7, aes(prod_type, revenue)) + 
  geom_bar(stat = 'identity') + theme(legend.position = "none") +
  ggtitle('Revenue by Product Type') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip() 
grid.arrange(p71,p72,nrow=2)
## Gems are the most popular product in both number of orders and revenue

# Part 3: When do the users purchase?
  
# 8. By server time
p81 <- ggplot(iaps) + geom_bar(aes(iaps_date, fill=prod_type)) + 
  ggtitle('Number of Orders over Time') + theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Number of orders')
p82 <- ggplot(iaps) + geom_bar(aes(iaps_date, rev,fill=prod_type), stat = 'identity') + 
  ggtitle('Revenue over Time') + theme(plot.title = element_text(hjust = 0.5))  
grid.arrange(p81,p82,nrow=2)
## Daily revenue peaked on March 7,  then dropped quickly till March 16 when it started dropping in a much slower manner. 
## In terms of product type being purchased, Gems become less and less popular as time goes by


# 9. First time of purchase
df9 <- iaps %>% select(udid, iaps_date) %>% group_by(udid) %>% summarise(converted_date = min(iaps_date))
converted_users <- inner_join(converted_users, df9, by = 'udid')
converted_users$days_n <- difftime(converted_users$converted_date, converted_users$install_date, units = 'days')
summary(as.numeric(converted_users$days_n))
## median users took 1 day to pay
ggplot(converted_users) + geom_bar(aes(as.numeric(days_n))) + ylab('Number of users') + 
  ggtitle('Days taken to Convert') + theme(plot.title = element_text(hjust = 0.5))  
converted_users %>% group_by(days_n) %>% summarise(count = n(), 
                                                   perc = count/nrow(converted_users))
## Half payers make their first payment on day 1 of playing
## Only 20% of payers remain as non-payer till day 5 

## Conclusion: Is there a pattern?
## 1) Payers are minority
## 2）Who are more likely to be payers? 
## - US, GB
## - English
## - iphone
## - New model
## - Up-to-date OS
## 3）What product generates the most revenue
## - Early game: gems
## - Late game: chapter passes
## 4）When are purchase most likely to happen?
## -  On the first few days (user time)


# Part 4: What is the users' behavior in game?

# 10. converted users
p101 <- sessions %>% filter(udid %in% purchased_user) %>% 
  group_by(sessions_date) %>% count() %>% 
  ggplot() + geom_line(aes(sessions_date, n), stat = 'identity') + 
  ggtitle('Number of Sessions over Time of Converted Users') + theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Number of sessions')
p102 <- spendevents %>% filter(udid %in% purchased_user) %>% 
  select(spend_date, story_chapter) %>% group_by(spend_date) %>% 
  summarise(n = n_distinct(story_chapter)) %>%
  ggplot() + geom_line(aes(spend_date, n), stat = 'identity') + 
  ggtitle('Number of Story_Chapter over Time of Converted Users') + theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Number of story_chapter')
summary(spendevents$amount) # -999999 probably an outlier
p103 <- spendevents %>% filter(udid %in% purchased_user) %>% 
  filter(amount != -999999) %>% select(spend_date, amount) %>% group_by(spend_date) %>% 
  summarise(total_amount = sum(amount)) %>%
  ggplot() + geom_line(aes(spend_date, total_amount)) + 
  ggtitle('Spending Amount over Time of Converted Users') + theme(plot.title = element_text(hjust = 0.5)) 
p104 <- iaps %>% group_by(iaps_date) %>% summarise(revenue = sum(rev)) %>%
  ggplot() + geom_line(aes(iaps_date, revenue)) + 
  ggtitle('Revenue over Time of Converted Users') + theme(plot.title = element_text(hjust = 0.5)) 
grid.arrange(p101,p102,p103,p104,nrow=4)
## Daily revenue peaked on March 6, then dropped quickly till March 16 when it started dropping in a much slower manner. 
## Number of sessions peaked on March 6, then dropped quickly till March 16 when it started dropping in a much slower manner. 
## Amount spending peaked on March 6, then dropped quickly till March 16 when it started dropping in a much slower manner. 

# 11. non_payers
p111 <- sessions %>% filter(udid %in% non_payers$udid) %>% 
  group_by(sessions_date) %>% count() %>% 
  ggplot() + geom_line(aes(sessions_date, n), stat = 'identity') + 
  ggtitle('Number of Sessions over Time of Non Payers') + theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Number of sessions')
p112 <- spendevents %>% filter(udid %in% non_payers$udid) %>% 
  select(spend_date, story_chapter) %>% group_by(spend_date) %>% 
  summarise(n = n_distinct(story_chapter)) %>%
  ggplot() + geom_line(aes(spend_date, n), stat = 'identity') + 
  ggtitle('Number of Story_Chapter over Time of Non Payers') + theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Number of story_chapter')
summary(spendevents$amount) # -999999 probably an outlier
p113 <- spendevents %>% filter(udid %in% non_payers$udid) %>% 
  filter(amount != -999999) %>% select(spend_date, amount) %>% group_by(spend_date) %>% 
  summarise(total_amount = sum(amount)) %>%
  ggplot() + geom_line(aes(spend_date, total_amount)) + 
  ggtitle('Spending Amount over Time of Non Payers') + theme(plot.title = element_text(hjust = 0.5)) 
grid.arrange(p111,p112,p113,nrow=3)
## Same trend as the converted users

## Converted Users vs Non Payers
## The amount per day went down and up during the first week, then stayed stable after the first week
## Although converted users accouts for only 6.67%, the total sessions per day are higher than non payers
## In March, the number of stroy-chapter per day is above 40 for converted users while for non payers the number is under 40


## A one-week Test Data ##
# Test Sessions Number

users$test_date = users$install_date + 7; head(users)

# adding session info 
sessions_num = count(sessions, udid, sessions_ts, sessions_date)
sessions_num %>% select(n) %>% arrange(desc(n))

test_data <- left_join(users, sessions_num, by = "udid")
colSums(is.na(test_data))
head(test_data)
# 'udid','install_date','test_date','sessions_ts','sessions_date','n'

# see whether session date is before the test date (1 week later)
# return a logical value, whether the user has session avtivity during 1 week
test_sessions <- test_data$sessions_date < test_data$test_date 
test_data$test_sessions = test_sessions
# extract those who are 'TRUE' on test_session from test_data
ti <- test_data[test_data$test_sessions == 'TRUE',]

# Add the count of session activitives for each udid
ts = mutate(group_by(ti,udid), t_sessions_n = sum(n))
ts = distinct(ts, udid, t_sessions_n)
# during the test week, how many session activities for each distinct udid


# Add the Revenue info from iapcopy
# revenue data before 1 week

test_data1 <- right_join(users, iaps, by = 'udid')

head(test_data1)
test_rev <- test_data1$iaps_date < test_data1$test_date 
test_data1$test_rev = test_rev
# extract those who are 'TRUE' on test_rev from test_data1
test_data1 <- test_data1[test_data1$test_rev == 'TRUE',]
iaps_test <- test_data1[-(2:8)]

tr <- left_join(ti, iaps_test, by = 'udid')
tr = mutate(group_by(tr,udid), rev_sum = sum(rev))
tr = distinct(tr, udid, rev_sum)
head(tr,10)

tr %>% select(udid, rev_sum) %>% arrange(desc(rev_sum)) %>% head()
# during the 1 week, the accumulated sum of the revenue for each distinct udid


# Test Chapter Number and mergeing test dataset to explore more insights
test_data2 <- left_join(ti, spendevents, by = 'udid')
head(test_data2)

test_spend <- test_data2$spend_date < test_data2$test_date
test_data2$test_spend = test_spend
# Extract those who are 'TRUE' on test_spend from test data
ti <- test_data2[test_data2$test_spend == 'TRUE',]

# Extract udid, story_chapter from ti and named 'time story count number'
t_sc_num = count(ti, udid, story_chapter)
tsc = count(t_sc_num, udid) # extract user id from data frame 'time story count number' 
tsc = na.omit(rename(tsc, t_story_chapter_n = n)) # Wipe na values and rename count number(how many story-chapter the user has played)

# Extarct user id and count total amount from ti and named 'Test Amount Spent'
ta = mutate(group_by(ti, udid), t_amount_n = sum(amount))
ta = na.omit(distinct(ta, udid, t_amount_n)) # Wipe out na values and extarct only distinct values of user id

# Sessions that Each Story_Chapter Takes
# t_sc_num = mutate(group_by(ti, story_chapter), sum(amount))
# t_sc_num = na.omit(distinct(t_sc_num, story_chapter, sum(amount)))
# write.csv(t_sc_num, file = "t_sc_num.csv", row.names = F)

# Data Manipulation
# To calculate distinct user id with their sum of revenue, story, spendtype, time session number, time amount number and time story chapter
ti1 <- full_join(ta, tsc, by = 'udid')
ti2 <- full_join(ts, ti1, by = 'udid')
ti <- full_join(tr, ti2, by = 'udid')
# ti = ti[is.na(ti$t_sessions_n) == FALSE & is.na(ti$t_story_chapter_n) == FALSE & is.na(ti$t_amount_n) == FALSE,]
ti[is.na(ti)] = 0 # rev_sum = 0
colSums(is.na(ti))
head(ti)

users1 <- users %>% select(c(1:6)) 

# country_dis
country_dis <- function(country) {
  if (country == 'US') {
    return('US')
  } else if (country == 'GB') {
    return('GB')
  } else if (country == 'CA') {
    return('CA')
  } else if (country == 'AU') {
    return('AU')
  } else if (country == 'PH') {
    return('PH')
  } else if (country == 'ID') {
    return('ID')
  } else if (country == 'NL') {
    return('NL')
  } else if (country == 'RU') {
    return('RU')
  } else {
    return('other')
  }
}
users1$country <- factor(sapply(users1$country, country_dis))

# lang_dis
lang_dis <- function(lang) {
  if (lang == 'en') {
    return('en')
  } else if (lang == 'es') {
    return('es')
  } else if (lang == 'fr') {
    return('fr')
  } else if (lang == 'de') {
    return('de')
  } else if (lang == 'nl') {
    return('nl')
  } else if (lang == 'ru') {
    return('ru')
  } else if (lang == 'pt') {
    return('pt')
  } else if (lang == 'nb') {
    return('nb')
  } else {
    return('other')
  }
}
users1$lang <- factor(sapply(users1$lang, lang_dis))

# hw_ver_dis
hw_ver_dis <- function(hw_ver) {
  if (hw_ver == 'iPhone7,2') {
    return('iPhone7,2')
  } else if (hw_ver == 'iPhone8,1') {
    return('iPhone8,1')
  } else if (hw_ver == 'iPhone6,1') {
    return('iPhone6,1')
  } else if (hw_ver == 'iPhone6,2') {
    return('iPhone6,2')
  } else if (hw_ver == 'iPhone5,2') {
    return('iPhone5,2')
  } else if (hw_ver == 'iPhone5,3') {
    return('iPhone5,3')
  } else if (hw_ver == 'iPad2,5') {
    return('iPad2,5')
  } else {
    return('other')
  }
}
users1$hw_ver <- factor(sapply(users1$hw_ver, hw_ver_dis))

# os_ver_dis
os_ver_dis <- function(os_ver) {
  if (os_ver == '9.2.1') {
    return('9.2.1')
  } else if (os_ver == '9.2') {
    return('9.2')
  } else if (os_ver == '9.1') {
    return('9.1')
  } else if (os_ver == '7.1.2') {
    return('7.1.2')
  } else if (os_ver == '8.3') {
    return('8.3')
  } else {
    return('other')
  }
}
users1$os_ver <- factor(sapply(users1$os_ver, os_ver_dis))

ti_full <- inner_join(users1, ti, by = 'udid') 
colSums(is.na(ti_full))

## Data Modeling ##
library(randomForest)
library(caTools)
ti_full$level = as.factor(ifelse(ti_full$rev_sum > 0, 'Converted_User', 'Non_Payer'))
prop.table(table(ti_full$level))

set.seed(123)
split = sample.split(ti_full$level, SplitRatio = 0.75)

training_set = subset(ti_full, split == TRUE)
test_set = subset(ti_full, split == FALSE)

set.seed(123)
#
model <- randomForest(level ~ install_date + lang + country + hw_ver + os_ver + 
                        t_sessions_n + t_amount_n + t_story_chapter_n, 
                      data = training_set,
                      ntree = 500,
                      importance = TRUE)
plot(model)

y_pred = predict(model, newdata = test_set, type = 'class')
head(y_pred)

table = table(Predicted = y_pred, Actual = test_set$level)
print(paste('Random Forest Accuracy', sum(diag(table)/ sum(table))))

varImpPlot(model)
