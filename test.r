x = sample(1:6, size = 100 , replace = TRUE)
xS = sum(x==6)/100


f  = function(){
  mean(sample(1:6, size = 100 , replace = TRUE)==6)
}
r = rbinom(200,100,1/6)
t.test(r)
sum(replicate(10000,f()))/10000

-
pbinom(6,10,1/6,lower.tail = F)
sum()

y = rbinom(10000,100,prob = 1/6)

prop.table(table(y))
barplot(prop.table(table(y)))




probs <- dbinom(x = 0:100, size = 100, prob = 1 / 6)
names(probs) <- 0:100
barplot(probs)




n_of_sixes <- rbinom(n = 1000, size = 30, prob = 1 / 6)

# Как ще намерим емпиричната вероятност да се паднат по-малко от 5 шестици?
mean(n_of_sixes < 5)


x = rnorm(1000, 5,2)
hist(x, probability = T)
lines(density(x))

t.test(x, conf.level = 0.95)




prop.test(x = 87, n = 150, conf.level = 0.95)
prop.test(x = 870, n = 1500, conf.level = 0.95)


x = rnorm(100,2,2)
t.test(x,mu=3)

library("UsingR")


prop.test(42,100,alternative = "less")
prop.test(420,1000,alternative = "less")


t = c(52.8, 53.5, 52.9, 59.4, 8.7, 0.7, 50.2, 2.8, 51.9, 2.8, 3.1, 5.8)


wilcox.test(t,mu=5,alternative = "greater")


x = c(4, 1, 7, 9)
y = c(10, 3, 2, 11)

shapiro.test(x)
shapiro.test(y)

hist(x)

var(x)
var(y)

t.test(x,y)


x = c(125, 410, 310, 300, 318, 298, 148)

chisq.test(x)

data(pi2000)
chisq.test(table(pi2000[1:200]))

t = table( pi2000[ 1 : 200 ] )



x = c(44, 74, 79, 72, 31,14, 25, 27, 24, 10, 15, 20, 20, 23, 9,3, 5, 5, 0, 0)
m = matrix(x, nrow = 4, byrow = T)
chisq.test(m)


pexp( 1/8, 2)



x = c(1,7,10,2,3,6,0,8,5,9,4,0,2,5,1,5,1)
t = table(x)
y = dbinom(0:10,10,1/8)
chisq.test(t,p=y)



library(tidyverse)
library(MASS)

patients_df <- data.frame(
  age = c(18, 23, 25, 35, 65, 54, 34, 56, 72, 19, 23, 42, 18, 39, 37),
  max_pulse = c(202, 186, 187, 180, 156, 169, 174, 172, 153, 199, 193, 174, 198, 183, 178)
)
n <- nrow(patients_df)

# построяване на модел
model <- lm(max_pulse ~ age, data = patients_df)

# рисуване на модел и данни
plot(patients_df)
abline(model, col = "red")

model_summary  = summary(model)
beta0_est = model_summary$coefficients[1, 1]
beta1_est = model_summary$coefficients[2, 1]

beta0_se = model_summary$coefficients[1, 2]

# Стандартно отклонение на оценката за бета1
beta1_se <- model_summary$coefficients[2, 2]



t_statistic  =  (beta1_est + 1 ) / beta1_se

pval <- pt(t_statistic, n - 2, lower.tail = FALSE)

ages_to_predict = c(30,40,50)

beta0_est + beta1_est * ages_to_predict



predict.lm(
  model,
  # newdata e data.frame с променливите за които искаме да прогнозираме
  newdata = data.frame(age = ages_to_predict),
  # доверителен интервал
  interval = "confidence",
  level = 0.9
)
confint(model, level = 0.9)


data(cats)
cats

model = lm(Hwt~Bwt,data = cats[cats$Sex=="M",])

summary(model)
confint(model)


model_summary  = summary(model)
beta0_est = model_summary$coefficients[1, 1]
beta1_est = model_summary$coefficients[2, 1]

beta0_se = model_summary$coefficients[1, 2]

# Стандартно отклонение на оценката за бета1
beta1_se <- model_summary$coefficients[2, 2]



t_statistic  =  (beta1_est - 5 ) / beta1_se
n= nrow(cats[cats$Sex=="M",])
pval <- pt(t_statistic, n - 2, lower.tail = TRUE)


predict.lm(model,newdata = data.frame(Bwt=2.6),interval = "confidence")
predict.lm(
  model,
  # newdata e data.frame с променливите за които искаме да прогнозираме
  newdata = data.frame(age = ages_to_predict),
  # доверителен интервал
  interval = "confidence",
  level = 0.9
)

df= data.frame(height=c(100, 200, 300, 450, 600, 800, 1000),
               len =c(253, 337, 395, 451 ,495, 534, 574))
model = lm(len~log(height),data=df)
  
summary(model)
plot(df)
abline(model, col="blue")





exdf = data.frame(ex1=c(5, 4, 4, 6, 4, 6, 3, 3, 4, 5),
                  ex2 = c(3, 2, 4, 5, 3, 4, 3, 4, 2, 4),
                  ex3 = c(4, 6, 4, 2, 4, 5, 5, 3, 6, 4))
shapiro.test(exdf$ex3)

exdf
stdf = stack(exdf)
model = lm(values~ind,data = stdf)
anova(model)
oneway.test(values~ind,data = stdf)


shapiro.test(InsectSprays$count[InsectSprays$spray=="A"])
#hist(), boxplot(), qqnorm(), qqline(), shapiro.test()
hist(InsectSprays$count[InsectSprays$spray=="A"])
boxplot(InsectSprays$count[InsectSprays$spray=="A"])
qqnorm(InsectSprays$count[InsectSprays$spray=="A"])
qqline(InsectSprays$count[InsectSprays$spray=="A"])


hist(InsectSprays$count[InsectSprays$spray=="E"])
boxplot(InsectSprays$count[InsectSprays$spray=="E"])
qqnorm(InsectSprays$count[InsectSprays$spray=="E"])
qqline(InsectSprays$count[InsectSprays$spray=="E"])


shapiro.test(InsectSprays$count[InsectSprays$spray=="B"])
shapiro.test(InsectSprays$count[InsectSprays$spray=="C"])
shapiro.test(InsectSprays$count[InsectSprays$spray=="D"])
shapiro.test(InsectSprays$count[InsectSprays$spray=="E"])

kruskal.test(count~spray,data = InsectSprays)

In2 = InsectSprays[InsectSprays$spray=="C" | InsectSprays$spray=="D"| InsectSprays$spray=="E"]


In2 = InsectSprays[(InsectSprays$spray=="C" |InsectSprays$spray=="D")|InsectSprays$spray=="E" ,]

kruskal.test(count~spray,data=In2)
  




f = function(a0, a1,n){
  a = c(a0,a1)
  for(i in 2:n){
    aTemp = a1
    a1 = 2*a1 + a0
    a0  = aTemp 
    a[i+1] = a1
  }
  
  return (a)
}


sum(f(0,1,10))


MoreThanYear = Aids2[(Aids2$death-Aids2$diag)>=365,]

t = Aids2[order(Aids2$age,decreasing = T),]
t$state[1:6]




boxplot(Aids2$age ~ Aids2$T.categ)

a = Aids2$age[Aids2$T.categ=="het"]
t.test(a, mu=35)




x= rbinom(200,10, 1/4)
t.test(x , conf.level = 0.9)
mean(x)
prop.table(table(x))
dbinom(4,10,1/4)


x = c(31, 50, 17, 2)
y = dbinom(0:3, 3,1/3)

chisq.test(x,p = y)



x= rnorm(200,15,5)
hist(x,probability = T)
lines(density(x))


data = data.frame(state.x77)

state.under1 = data[data$Illiteracy < 1,]$Murder
state.under2 = data[data$Illiteracy >= 1,]$Murder


length(state.under1)
length(state.under2)

shapiro.test(state.under1)
shapiro.test(state.under2)
t.test(state.under1,state.under2, alternative = "less")



d = data.frame(x3 = anscombe$x3, y3 = anscombe$y3);

r = d[-which.max(d$y3),]
boxplot(d$x3,d$y3)
boxplot(r$x3,r$y3)

plot(r$x3~r$y3)





s = seq(0, 30,length.out = 60)




x = rnorm(1000,10,9)
hist(x,probability = T)

lines(density(x))