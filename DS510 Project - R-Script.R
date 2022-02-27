
df = read.csv('Who_Suicide_Statistics.csv')

head(df)
dim(df)
# 1)
str(df)
summary(df)
# country

table(df$country)
unique(df$country)
# there are 141 countries in the dataset

# year
table(df$year)
unique(df$year)

# 38 years
# most observations belongs to year 2002 and lest number of observations belongs to 2016

# sex

table(df$sex)
# there are a similar number of mails and females in the study

# age
table(df$age)

# there 6 types of age groups
# similar number of individuals are belong to each age group

# suicide no

summary(df$suicides_no)
# mean 193.3
# min = 0
# max = 22338
# there are 2256 missing values

# population

summary(df$population)


#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#     259    85113   380655  1664091  1305698 43805214     5460 

# 2) 

table(is.na(df))
# there are 7716 missing values in the dataset

# rows with missing values

sum(!complete.cases(df)) 


summary(df)

dim(df)
df = na.omit(df)
# 3)
library(dplyr)
df = df %>%  mutate(pct_change_population = (population/lag(population) - 1) * 100)


df = df %>%  mutate(pct_change_suicides = (suicides_no /lag(suicides_no ) - 1) * 100)


df_new = df[df$country == 'Sri Lanka',]
df_new
cor(df_new$pct_change_population, df_new$pct_change_suicides)


# plot

library(ggplot2)
ggplot(df_new, aes(x=pct_change_suicides, y=pct_change_population)) + 
  geom_point()+
  geom_smooth(method=lm)


# 4)
str(df)
sui = aggregate(df$suicides_no, by=list(Category=df$age), FUN=sum)
pop = aggregate(df$population, by=list(Category=df$age), FUN=sum)
sui_by_age = sui$x
pop_by_age = pop$x
age_group =pop$Category

data <- data.frame(unlist(age_group), unlist(pop_by_age), unlist(sui_by_age)) 
data
cor(data$unlist.pop_by_age., data$unlist.sui_by_age.)
data$ratio = data$unlist.sui_by_age./data$unlist.pop_by_age.
data$unlist.age_group. =as.factor(data$unlist.age_group.)
levels(data$unlist.age_group.)



# kruksala wallis test

k <- kruskal.test(ratio ~ unlist.age_group., data = data)
summary(k)
k

# 5)

library("ggpubr")
ggboxplot(df, x = "sex", y = "suicides_no", 
          color = "red", palette = c("#00AFBB", "#E7B800"),
          ylab = "suicides", xlab = "sex")

res <- t.test(suicides_no ~ sex, data = df, var.equal = TRUE)
res

data_S = df %>%
  group_by(sex, age) %>%
  summarise(p_sum=sum(population  ),
            p_mean=(mean(population  )))

data_S
ggplot(data_S, aes(x = age, y = p_sum   , fill = sex)) + 
  geom_bar(stat = "identity") +
  xlab("\nage") +
  ylab("count\n") +
  theme_bw()


# 6)

df_year = df[df$year== 1992,]
df_year

data_year = aggregate(df_year$suicides_no, by=list(Category=df_year$country), FUN=sum)
data_year = data_year[order(-data_year$x),]
data_year


# graph
data_year = data_year[1:10,]
ggplot(data=data_year, aes(x=x, y=Category, group=1)) +
  geom_line(color = "blue")+
  geom_point()


# 7)

cor(df$population, df$suicides_no)
ggplot(df, aes(x=population, y=suicides_no)) + 
  geom_point()+
  geom_smooth(method=lm)


# 8)
unique(df$country)

df_new = aggregate(df$suicides_no, by=list(Category=df$year), FUN=sum)



df_romania = df[df$country =="Romania",]
df_SA = df[df$country =="South Africa",]
df_SL = df[df$country =="Sri Lanka",]
df_UAE = df[df$country =="United Arab Emirates",]
df_TURKEY = df[df$country =="Turkey",]

df_romania = aggregate(df_romania$suicides_no, by=list(Category=df_romania$year), FUN=sum)
df_SA = aggregate(df_SA$suicides_no, by=list(Category=df_SA$year), FUN=sum)
df_SL = aggregate(df_SL$suicides_no, by=list(Category=df_SL$year), FUN=sum)
df_UAE = aggregate(df_UAE$suicides_no, by=list(Category=df_UAE$year), FUN=sum)
df_TURKEY = aggregate(df_TURKEY$suicides_no, by=list(Category=df_TURKEY$year), FUN=sum)


model1 = lm(x ~Category, data = df_romania)          
model1$fitted.values
predict(model1)


model2 = lm(x ~Category, data = df_SA)          
model2$fitted.values

model3 = lm(x ~Category, data = df_SL)          
model3$fitted.values

model4 = lm(x ~Category, data = df_UAE)          
model4$fitted.values[1:10]
predict(model4)
model5 = lm(x ~Category, data = df_TURKEY)          
model5$fitted.values

