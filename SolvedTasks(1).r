#TASK1
roll_n_times <- function(N) {
  rolls<-sample(1:6,N,replace=TRUE)
  x <- table(unlist(rolls))
  probs <- rep(1/6, 6)
  chisq.test(x, p=probs)$p.value
}

solution_task_one <- function(N){
  x<-replicate(10000, roll_n_times(N))
  othvyrlqm_x_pyti<-length(x[x<0.05])
  othvyrlqm_x_pyti/10000
}

solution_task_one(100)
solution_task_one(200)
solution_task_one(300)
solution_task_one(400)

#TASK2
gen_n_numbers <- function(N){
  data <- runif(N, 5, 12)
}

solution_task_two <- function(N){
  x<-replicate(10000, shapiro.test(gen_n_numbers(N))$p.value)
  othvyrlqm <- length(x[x<0.05])
  othvyrlqm/10000
}

solution_task_two(15)
solution_task_two(25)
solution_task_two(50)
solution_task_two(90)


#TASK3
#3.a
generate_xi_yi_a<-function(n){
  e1 <- rnorm(n,mean = 0,sd=2)
  xi <- runif(n,1,10)
  yi <- 5*xi+e1
  lm(yi~xi)
}

generate_xi_yi_b<-function(n){
  e2<- runif(n,-3.5, 3.5)
  xi <- runif(n,1,10)
  yi <- 5*xi+e2
  lm(yi~xi)
}

generate_xi_yi_c<-function(n){
  v1 <- rexp(n,rate=0.7)
  w1 <- rexp(n,rate=0.7)
  e3 <- v1 - w1
  xi <- runif(n,1,10)
  yi <- 5*xi+e3
  lm(yi~xi)
}

generate_xi_yi_d<-function(n){
  u <- rexp(n,rate=0.5)
  e4 <- u - 2
  xi <- runif(n,1,10)
  yi <- 5*xi+e4
  lm(yi~xi)
}

generate_xi_yi_a(30)
generate_xi_yi_a(50)
generate_xi_yi_a(100)
generate_xi_yi_a(500)

generate_xi_yi_b(50)
generate_xi_yi_c(100)
generate_xi_yi_d(500)


solution_task_1 <- function(a0,a1,n){
  nm1 = a1
  nm2 = a0
  while(n > 1){
    an = 2*nm1 + nm2
    n = n - 1
    nm2 = nm1
    nm1 = an
  }
  an
}

sum_seq <- function(a0,a1,n){
  nm1 = a1
  nm2 = a0
  sq <- c(a0,a1)
  while(n > 1){
    an = 2*nm1 + nm2
    n = n - 1
    nm2 = nm1
    nm1 = an
    append(sq,an)
  }
  sum(sq)
}


solution_task_1(1,0,5)
sum_seq(1,0,5)


install.packages("MASS")
library("MASS")
View(Aids2)
#21
print(nrow(filter(Aids2[Aids2$death - Aids2$diag > 365,])))
#22
Aids2$state[sort(Aids2$age, decreasing = TRUE)[1:6]]
#23
boxplot(Aids2$age ~ Aids2$T.categ)

#31
x = Aids2$age[Aids2$T.categ == 'het']
t.test(x, 35)


mean(Aids2$age[Aids2$T.categ == 'het'])

#41

sim.cards <- function(k) {
  x <- sample( c(1:52), k, replace=T )
  d <- sum(x<=13)
  print(x)
  d
}

prob.cards <- function(Nrep, k) {
  rs <- replicate( Nrep, sim.cards(k) )
  t.test(rs, conf.level=0.90)$conf.int[1:2]
}
prob.cards(200, 10)
dbinom(4, 10, 0.25)

sim.cards(10)


#5
X <- c(0, 1, 2, 3)
x <- c(31, 50, 17, 2)
probs <- dbinom(X, 3, 1/3)

chisq.test(x,p=probs)
#X-squared = 2.7687, df = 3, p-value = 0.4287 -> не отхвърляме
library("MASS")
#6
View(cats)
malecats <- cats[cats$Sex =="M",]
malecats
plot(malecats$Bwt ~ malecats$Hwt)
plot(malecats$Bwt[malecats$Bwt - malecats$Bwt == 1])


m1 <- lm(malecats$Hwt ~ malecats$Bwt, data = malecats)
summary(m1)
coef(m1)
confint(m1)
#y = тегло на котката
#x = тегло на сърцето

#модел:
y = beta_0 + beta_1 * x + eps

#оцеneno regresionno urawnenie:
y.hat = -1.184 + 4.313 * x

fitted.values(m1)

h0: beta_1 = 5
h1: beta_1 != 5

summary(m1)$coef
confint(m1)

#Доверителен интервал:  [3.637904 4.9874540]
#5 не е в интервала 
#ОТХВЪРЛЯМЕ

#predict(m1, data.frame(Bwt=2.6), interval="confidence", level = 0.95)
predict(m1, data.frame(Bwt = c(2.6)), interval="confidence", level=0.95)

#Доверителен интервал за пропорция (вероятност за успех)
prop.test(x=2700, n=25000, conf.level=0.95, correct=F)$conf.int[1:2]
#__________________________________________________________________________

#__________________________________________________________________________

#__________________________________________________________________________


library('UsingR');

data <- data.frame(mtcars$carb, mtcars$cyl);
plot(data)
model <- lm(mtcars$cyl ~ mtcars$carb);
abline(model);


beers <- c(5,2,9,8,3,7,3,5,3,5);
alcohol <- c(0.1, 0.03, 0.18, 0.12, 0.04, 0.11, 0.07, 0.06, 0.02, 0.05);
data <- data.frame(beers, alcohol);
plot(data);
model1 <- lm(data$alcohol ~ data$beers);
abline(model1);
s = summary(model1)
s
beta0 <- s$coefficients[1,1];
beta0

beta0_err <- s$coefficients[1,2];
beta0_err
data
boxplot(data, horizontal = T)
hist(data$beer, probability = T);
lines(density(data$beers, na.rm  = T))
lines(density(data, na.rm = TRUE))
qqnorm(data$beers);
qqline(data$beers)
data$beers
library('UsingR');
morley;

morley$Expt
expt1 <- morley[morley$Expt == 1,3];
mean(expt1, na.rm = T)

expt2 <- morley[morley$Expt == 2,3];
mean(expt2, na.rm = T)


expt3 <- morley[morley$Expt == 3,3];
mean(expt3, na.rm = T)


expt4 <- morley[morley$Expt == 4,3];
mean(expt4, na.rm = T)

expt5 <- morley[morley$Expt == 5,3];
mean(expt5, na.rm = T)



library("UsingR");

s = 0;
for(i in 1:1000) {
  current_probability <- pnbinom(q = 7, size = 3, prob = 1/6, lower.tail = FALSE);
  s = s + current_probability;
}
s / 1000;


pnbinom(q = 7, size = 3 , prob = 1/6, lower.tail = FALSE);

group_probability <- 0
for(i in 1:1000) {
  count <- 0;
  attempts <- 0;
  while(count < 3) {
    curr <- sample(1:6, size=1, replace = TRUE, prob = c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6));
    if(curr == 6){
      count <- count + 1;
    }
    attempts <- attempts + 1;
  }
  if(attempts >= 10){
    group_probability =  group_probability + 1;
  }
}
group_probability / 1000 
#-------------------------------------------------------
#                   HOMEWORK 2
#-------------------------------------------------------
#Task 1
diceThrowN <- function(n) {
  dices <- round(runif(n, min=0, max=6), digits = 0)
  return(ceiling(sum(dices)) %% 21 == n || floor(sum(dices)) %% 21 == n)
}
diceThrowN(100)

probOfSucces <- function(repetitions, n) {
  res <- replicate( repetitions, diceThrowN(n))
  return(sum(res)/length(res))
}

probOfSucces(10000,100)
probOfSucces(10000,200)
probOfSucces(10000,400)

#Task 2

generateData <- function(n) {
  dices <- round(runif(n, min=5, max=12), digits = 0)
  return(shapiro.test(dices)$p.value <= 0.05)
}

probOfSucces <- function(repetitions, n) {
  res <- replicate( repetitions, generateData(n))
  return(sum(res)/repetitions)
}

probOfSucces(10000,15)
probOfSucces(10000,25)
probOfSucces(10000,50)
probOfSucces(10000,90)

#Task 3
n = 500
Xi = round(runif(n,min=1,max=10), digits=0)

E1 <- rnorm(n,mean = 0,sd=2)
Y1i = 5 * Xi + E1

E2<- runif(n,min=-3.5,max=3.5)
Y2i = 5 * Xi + E2

plot(Xi, Y1i, pch = 20, xlab="Xi", ylab="Yi")
abline(lm(Y1i ~ Xi), col = "red", lwd = 3)
plot(Xi, Y2i, pch = 20, xlab="Xi", ylab="Yi")
abline(lm(Y2i ~ Xi), col = "red", lwd = 3)

#-------------------------------------------------------
#                   HOMEWORK 1
#-------------------------------------------------------

#Task 1
p <- 0.5
shots = c(
  8, 5, 12, 11, 12, 8, 6, 7, 11, 7, 11, 13, 15,
  12, 17, 12, 9, 15, 8, 11, 11, 13, 10, 8, 12, 12, 11,
  13, 12, 14, 9, 11, 13, 10, 10, 12, 13, 10, 15, 12, 15, 12
)
df = dbinom(shots, 30, p)
dbinom(shots, 30, p)
plot(shots,df,xlab="Center Hits", ylab="Probability",pch = 20)

#Task 2
mtcars[1:5, ]

mtcars[which.max(mtcars$hp), ]
sum(mtcars[order(mtcars$mpg, decreasing = TRUE)[1:5], ]$wt)/5


boxplot(mpg ~ cyl, data = mtcars, ylab="Miles per gallon", xlab="Cylinders")
plot(mtcars$mpg, mtcars$hp, pch = 20, xlab="Miles per gallon", ylab="Hourse power")
abline(lm(mtcars$hp ~ mtcars$mpg), col = "red", lwd = 3)

quantile(mtcars$hp,0.8)

x <- sum(mtcars$hp<100)/length(mtcars$hp)*100
paste(x,"%")

x <- length(mtcars$gear[mtcars$gear == 5 & mtcars$cyl == 8])

y <- length(mtcars$gear[mtcars$gear == 5])
paste(x/y*100,"%")

#Task 3
boughtGum <- function(k) {
  x <- sample( c(1:20), k, replace=T )
  v <- c(1:20) %in% x
  return(sum(v) == 20)
}
probOfSucces <- function(repetitions, k) {
  res <- replicate( repetitions, boughtGum(k) )
  sum(res)/repetitions
}
gum = 100
while (probOfSucces(10000,gum) != 1){
  gum <- gum + 1
}
paste("bought gums: ",gum)

#-------------------------------------------------------
#                   OTHER TASKS
#-------------------------------------------------------

gen <- function(n){
  return(c(sample(x=1:6, size=n ,replace= TRUE)))
}

prob.dice <- function(Nrep, n){
  res <- replicate(Nrep, gen(n))
  probs <- rep(1/6, 6)
  chisq.test(table(res), p=probs)
}

prob.dice(10000,100)

sort(some_vector)
some_vector[order(some_vector)]
mean(survey$Height[survey$Sex == "Male"], na.rm = TRUE)

prop.table(table(survey$Smoke))
prop.table(table(survey$Sex, survey$Smoke))
prop.table(table(survey$Sex, survey$Smoke), 1)
prop.table(table(survey$Smoke, survey$Sex), 2)
sample(c(TRUE, FALSE), size = 1000, replace = TRUE, prob = c(0.3, 0.7))
plot(sample(1:6, size = 10, replace = TRUE), type = "l")
abline(h = 4, col = "red")
x <- rnorm(1000, mean = 176, sd = 7)
x = rnorm(30, 2, 2)
t.test( x, mu = 3, alternative = 'two.sided')
prop.test(42, 100, p =0.5, alternative = 'less')

pow <- function(number,power) {
  p = 1;
  for(i in 1:power) {
    p = p * number; 
  }
  return(p);
}
calculateF <- function(n,x) {
  sumF = 0;
  for(s in 1:n) {
    current <- (pow(x,s)*pow(s,n))/(pow(x,n) + n*n);
    sumF = sumF + current;
  }
  return(sumF)
}

calculateF(20,0.01)

data <- data.frame(mtcars$gear, mtcars$cyl);
plot(data)
model <- lm(mtcars$cyl ~ mtcars$carb);
abline(model);
plot(mtcars$cyl, mtcars$gear, pch = 20, xlab="Cylinders", ylab="Gears")
sum(mtcars[order(mtcars$mpg, decreasing = TRUE)[1:5], ]$wt)/5

plot(mtcars$cyl, mtcars$carb)
abline(lm(mtcars$carb ~ mtcars$cyl), col = "red")

x <- length(mtcars$cyl[mtcars$cyl == 6 & mtcars$carb == 1])

y <- length(mtcars$gear[mtcars$carb == 1])
paste(x/y*100,"%")
rep(1/6,6)

chisq.test(table(mtcars$cyl), p=c(0.2,0.2,0.6))

morley;
morley$Expt

mean(morley[morley$Expt == 1,3], na.rm = T)
mean(morley[morley$Expt == 2,3], na.rm = T)
mean(morley[morley$Expt == 3,3], na.rm = T)
#----------------------------------------------------------
#----------------------------------------------------------
#Решени задачи
#----------------------------------------------------------
#----------------------------------------------------------
#1a
sum(Aids$age < 20);
Aids2
#1b
i = order(Aids2$diag)[Aids2$T.categ =='spin']
Aids2[i[1:5],]

#1v
men = Aids2[Aids2$sex == 'M',];
men.blood = sum(men$T.categ == 'blood');
p = men.blood/length(men[,1]); # 0.02069717
#1g
barplot(prop.table(table(status,state), 2), legend = T);

#Вярно ли е,че при жените смъртносста е по-ниска, хипотеза?
#zad2
prop.test(c(p[2,1],p[2,2]), c(sum(Aids2$sex == 'F'), sum(Aids2$sex == 'M')), alternative = "less");
#poneje e proporciq i vzimam murtvite jeni i muje i gi supostqvam s obshtiq broi i alternativa
#ne?

#можем ли да приемем че средната възраст на починалите е 38г
#zad3
x = Aids2[Aids2$status == 'D',];
y = (x$death - x$diag)/365 + x$age
# datata na koqto sa umreli - datata na koqto sa diagnosticirani /365 + godinite kogato sa diagnostirirani.
wilcox.test(y,mu = 38);
#p-value = 27% moje da priemem truvdenieto.


#Нека Х е случайна величина с хи-квадрат с разпределение 10 степени на свобода
#генерирайте 100 наблюдения -> хист , плътност
#zad4
x = rchisq(100,10);
hist(x, probability = T);
curve(dchisq(x,10), add = T);

#zad5
#kak zavisi tegloto na surceto ot obshtoto teglo
men.cats = cats[cats$Sex == 'M',];
x= men.cats$Bwt;
y = men.cats$Hwt;
l = lm(y ~ x);
plot(x,y);
abline(l);
s = summary(l);
s
#p.value e < 2,2e-16, malko e toest ima vruzka , a i Multiple R-squared e 62%.

#vqrno li e che pri kotki po-tejki s 1kg , surceto e po - tejko s 5gr
#h0 : beta1 = 5
#h1 : beta1 != 5
beta1 = s$coefficients[2,1]
se = s$coefficients[2,2]
t = (beta1 - 5)/se
p = 2*pt(t,df = 95)
#p = 0.04.5 => h1 othvurlqme hipotezata;

#Postroite 95% doveritelen interval za surceto na kotka s teglo 2.6kg
d = data.frame(x = 2.6);
predict.lm(l,d,interval = "confidence",conf.level = 0.95);

# дата файл предложение тип разпределение интервам 0 10
#zad6
data = read.csv("E:/Data1.txt", header = FALSE); # header e false shtoto imah nqkuv X pred purvto chislo
x = data$V1;
shapiro.test(x); # p-valueto e 81% => normalno razpredelni sa
y = cut(x,breaks = c(0,2.5,5,7.5,10)); # razdelqme stoinostite po intervali da vidim dali sa ravnomerno rapredeleni v int [0,10]
m = table(y); # pravq tablica sus suotvetstiq kolko ot chislata v koi interval vlizat.
chisq.test(m) # p-valueto mnogo malko => othvurlqme hipotezata da sa ravnomerno razpredeleni v tozi interval.

#----------------------------------------------------------
#----------------------------------------------------------
#po qko Решени задачи
#----------------------------------------------------------
#----------------------------------------------------------

zad1rows = function(a0, a1, n) {
  if (n == 1) {
    return(a0)
  }
  else if (n == 2) {
    return(a1)
  }
  ak2 = a0
  ak1 = a1
  ak = 1
  for(i in 1:(n-2)) {
    
    ak = 2 * (ak1 + ak2)
    ak2 = ak1
    ak1 = ak
  }
  return(ak)
}

zad1rowsSum = function(a0, a1, n) {
  
  sum = 0
  
  for (i in 1:n) {
    sum = sum + zad1rows(a0, a1, i)
  }
  
  return(sum)
}

# Това е an на редицата, зададена с (1,2,20):
zad1rows(1, 2, 20)
# Сумата на редицата, зададена с (1,2,20):
zad1rowsSum(1, 2, 20)


#5 Герой фантастичен сериал носят три цвята
#Хипотеза Н0: Не съществува зависимост (независими са) 
#Алтернатива Н1: Съществува зависимост (зависими са)

x = c(43, 96, 123, 12, 32, 35, 8, 18, 44)
m = matrix(x, nrow = 3, byrow = T)
m
#     [,1] [,2] [,3]
#[1,]   43   96  123
#[2,]   12   32   35
#[3,]    8   18   44

chisq.test(m)

#p-value = 0.1498 > 0.05 => Приемаме хипотезата Н0 – независими са.

#4_____ Функцията, която симулира 200 опити (връща масив от броя на изтеглените карти на всеки опит):

zad4cards = function(n) {
  
  res = rep.int(0,n)
  
  for (i in 1:n) {
    
    cards = sample(1:52, 52, replace = F)
    count = 1
    j = 1
    card = cards[j]
    while (card != 1 & card != 14 & card != 27 & card != 40) {
      count = count + 1
      j = j + 1
      card = cards[j]
    }
    res[i] = count
  }
  return(res)
}

x = zad4cards(200)

#Проверяваме дали данните са нормални чрез ф-иите от горната задача.
#Данните не са нормални => wilcox.test()

wilcox.test(x, conf.int = T, conf.level = 0.9)

#90 percent confidence interval:
#  9.000052 11.499975

#Искаме да пресметнем вероятността P(X = 10) = ?

dgeom(10, 1/13)* 100
#[1] 3.454901    # Така е в проценти, затова умножихме по 100 => 3.45% 

#5________
#Можем ли да твърдим, че жените родили бебе под нормалните 2.5 кг. са по-млади? Постройте #графично. Формулирайте и проверете хипотеза за това твърдение.

library(MASS)
attach(birthwt)
#X: Възрастите на жените, родили бебе под 2,5 кг.
#Y: Възрастите на жените, родили бебе над 2,5 кг.
#H0: X = Y
#H1: X < Y

x = age[low == 1]
# [1] 28 29 34 25 25 27 23 24 24 21 32 19 25 16 25 20 21 24 21 20
#[21] 25 19 19 26 24 17 20 22 27 20 17 25 20 18 18 20 21 26 31 15
#[41] 23 20 24 15 23 30 22 17 23 17 26 20 26 14 28 14 23 17 21
y = age[low == 0]
#  [1] 19 33 20 21 18 21 22 17 29 26 19 19 22 30 18 18 15 25 20 28
# [21] 32 31 36 28 25 28 17 29 26 17 17 24 35 25 25 29 19 27 31 33
# [41] 21 19 23 21 18 18 32 19 24 22 22 23 22 30 19 16 21 30 20 17
# [61] 17 23 24 28 26 20 24 28 20 22 22 31 23 16 16 18 25 32 20 23
# [81] 22 32 30 20 23 17 19 23 36 22 24 21 19 25 16 29 29 19 19 30
#[101] 24 19 24 23 20 25 30 22 18 16 32 18 29 33 20 28 14 28 25 16
#[121] 20 26 21 22 25 31 35 19 24 45

#Проверяваме дали данните са нормални:
#Първо провеяваме x:

hist(x)
boxplot(x)
qqnorm(x)
qqline(x)

#Данните в x изглеждат нормални. Проверяваме и с Шапиро тест:

shapiro.test(x)
#p-value = 0.521 > 0.05 => Приемаме хипотезата, че данните в x са нормални.

#Провеяваме и y:

hist(y)
boxplot(y)
qqnorm(y)
qqline(y)

#Данните в y не изглеждат нормални. Проверяваме и с Шапиро тест:

shapiro.test(y)

#p-value = 0.000108 < 0.05 => Отхвърляме хипотезата, че данните в y са нормални => данните не са #нормални. => Ще използваме wilcox.test().

wilcox.test(x, y, alternative = 'less')

#p-value = 0.1236 > 0.05 => Приемаме хипотезата H0. Не можем да твърдим, че жените, родили бебе под 2.5 кг са по-млади.


#2__________

sum(low == 1 & smoke == 1)

sum(bwt < 2500 & smoke == 1)


race2 = factor(race, labels = c("white", "black", "other"))
t = prop.table(table(smoke, race2), 2)
barplot(t, legend.text = c("no", "yes"), main =   "Smoking during pregnancy")

#2____

library(MASS)
attach(Aids2)

sum(death - diag > 365)

ord = order(age, decreasing = T)
Aids2[ord[1:6],]

Aids2[ord[1:6],1]

age2 = cut(age, breaks = c(0,20,40,60,100))
t = table(T.categ, age2)
t
age2

probT = prop.table(table(T.categ, age2), 2)
barplot(probT, beside = T, legend.text = T)

#3__Трябват ни възрастите на всички хора, заразени при хетеросексуален акт
#Хипотеза Н0: μ = 35
#Алтернатива Н1: μ ≠ 35

#Трябват ни възрастите на всички хора, заразени при хетеросексуален акт:
x = age[T.categ == 'het']
x
#[1] 26 39 35 31 74 63 42 49 50 34 35 58 33 27 65 42 29 37 25
#[20] 37 25 47 33 35 40 30 21 50 24 25 49 38 37 30 54 52 26 29
#[39] 40 46 34

#Трябва да проверим дали данните са нормални (hist(), boxplot(), qqnorm(), qqline(), shapiro.test())
#Данните не са нормални.

wilcox.test(x, alternative = "two.sided", mu = 35)

#p-value = 0.1171 > 0.05 => Приемаме хипотезата Н0.Средната възраст на хората е 35 год.

#4____ теглим купи
x = rbinom(200, 10, 1/4)

#Проверяваме дали данните са нормални по добре познатия начин -> не са нормални => wilcox.test()

wilcox.test(x, conf.int = T, conf.level = 0.9)

#90 percent confidence interval:
#  2.499958 2.500032

#Пресмятаме теоретичната вероятност P(X = 4)

dbinom(4, 10, 1/4)
#[1] 0.145998
dbinom(4, 10, 1/4) * 100
#[1] 14.599   # в проценти

#5____ табличка с стойности
#Хипотеза Н0: X е Bi(3, 1/3)
#Алтернатива H1: X не е Bi(3, 1/3)

#P(X = k) = ? за k = 0..3 можем да ги намерим. 
#Ще сравним теоретичните вероятности, които получаваме с вероятностите от опита.

#Верояностите от опитите:
x = c(31,50,17,2)
x / 100
#[1] 0.31 0.50 0.17 0.02
#Теоретичните вероятности:
pr = dbinom(0:3, 3, 1/3)
pr
#[1] 0.29629630 0.44444444 0.22222222 0.03703704

#Сега трябва да сравним вероятностите:
chisq.test(x, p = pr)
#X-squared = 2.7687, df = 3, p-value = 0.4287

#p-value = 0.4287 > 0.1 => Приемаме хипотезата H0, X е Bi(3, 1/3)
#(по условие нивото на значимост е 0.1

#6___    
#Задачата със котките ADVANCED

library(MASS)
attach(cats)

male_cats = cats[Sex == "M", ]

x = male_cats$Bwt
y = male_cats$Hwt

plot(y, x)

l = lm(y ~ x)
d = data.frame(x = 2.6)

# 95 доверителен интервал за сърцето на котка, която има телесно тегло 2.6кг
predict.lm(l, d, interval = 'confidence', level = 0.95)

# искаме да видим дали
s = summary(l)
s

# oт коефициента пред х виждаме, че при увеличение на телесното тегло с 1кг
# се получава увеличение в теглото на сърцето с 4.3127 грама
# нас ни питат можем ли да твърдим че се получава увеличение с 5 грама

# H0: b1 = 5; 2-sided
# H1: b1 != 5

# t = (coefEstX - 5) / coefErX

t = (s$coefficients[2,1] - 5) / s$coefficients[2,2]
t
# [1] -2.022163

# oказа се отрицателно =>
pt(t, length(x) - 2) * 2
# [1] 0.04597144 # това е p-val => не може да твърдим, че при котки по-тежки с 1кг, сърцето по-тежко с 5грама

#1______________ 
library(MASS)
attach(Aids2)

sum(age < 20)

ord = order(diag)
sex[ord[1:5]]
#[1] M M M M M
#Levels: F M

t = table(T.categ[sex == 'M'])
t
#hs   hsid     id    het   haem  blood mother  other 
#2464     72     28     21     46     57      3     63

prop.table(t)
#hs        hsid          id         het        haem 
#0.894698620 0.026143791 0.010167030 0.007625272 0.016702977 
#blood      mother       other 
#0.020697168 0.001089325 0.022875817

#=> 0.02 от мъжете са се заразили по кръвен път.


t = table(status, state)
t
#state
#status  NSW Other  QLD  VIC
#A  664   107   78  233
#D 1116   142  148  355
pt = prop.table(t, 2)
barplot(pt, legend.text = T)


#2______при жениет смуртнноста е по ниска?

#Данните са булеви -> “D” или “А“ => Ще използваме prop.test()
#Хипотеза H0: Pж = Pм (Вероятността смъртността при жените да е същата като тази при мъжете)
#Алтернатива H1: Pж < Рм (Вероятността смъртността при жените да е по-ниска)
#prop.test(x, n, alternative = ‘less’), където:
# x – вектор с успешни опити – бр. починали жени и бр. починали мъже
#n – вектор с всички опити – бр. жени общо и бр. мъже общо

sum(sex == 'F' & status == 'D')  # брой починали жени
#[1] 53
sum(sex == 'M' & status == 'D')  # брой починали мъже
#[1] 1708
sum(sex == 'F')   # брой жени общо
#[1] 89
sum(sex == 'M')   # брой мъже общо
#[1] 2754
prop.test(x = c(53, 1708), n = c(89, 2754), alternative = 'less')

#p-value = 0.359 > 0.05 => Приемаме хипотезата H0.

# Problem 3
shapiro.test(age[status == "D"])
qnorm(age[status == "D"])
qqline(age[status == "D"])

# seem normal..

# H0: myu = 38
# H1: myu != 38
t.test(age[status == "D"], mu = 38)

# p-value 0.4017 => H0

#3___________Средната възраст е 38г

#Хипотеза H0: μ = 38
#Алтернатива H1: μ < 38  (Защото ни интересува дали средната възраст на починалите не е даже под 38)

x = age[status == 'D']

#Проверяваме дали данните са нормални. Данните не са нормални, но са мнооогооо => по Централната гранична теорема -> можем да използваме t.test().

t.test(x, mu = 38, alternative = 'less')

#p-value = 0.2008 > 0.05 => Приемаме хипотезата Н0.

#4_________ хи-квадрант
x = rchisq(100, df = 10)
hist(x)

#За да начертаем ф-ята на теоретичната плътност, първо трябва да направим хистограмата с вероятности:
hist(x, probability = T)
s = seq(0, 30, length.out = 50)
s
ds = dchisq(s, 10)
lines(s, ds, col = 'red')

# Problem 4
observations = rchisq(100, 10)
hist(observations)

minobs = min(observations)
maxobs = max(observations)

z = seq(2.2, 30.2, 0.2)
y = dchisq(z, df = 10)

hist(observations, probability = T)
lines(z, y, col = 'red')

# Problem 2

# H0: smurtnostta e ednakva
# alternative: smurtnostta pri mujete e po-golqma

dead_men = sum(sex == "M" & status == "D")
dead_women = sum(sex == "F" & status == "D")

prop.test(c(dead_men, dead_women),
          c(sum(sex == "M"), sum(sex == "F")),
          alternative = "greater")

#Задача 1___________

#а)
sum(status == 'D')
#[1] 1761
#б)
ord = order(diag)
Aids2[ord[1:5],]
#state sex diag death status T.categ age
#37     NSW   M 8302  8469      D      hs  51
#2129   VIC   M 8499  8568      D      hs  43
#2130   VIC   M 8582  8630      D      hs  38
#38     NSW   M 8711  8850      D      hs  29
#2131   VIC   M 8720  9045      D      hs  47
Aids2[ord[1:5],'age']  # ако искаме само колоната с възрастите
#[1] 51 43 38 29 47

#в)
t = table(state[T.categ == 'blood'])
t

#NSW Other   QLD   VIC 
#70     5    15     4 

#Ако ги искаме в проценти:
prop.table(t) * 100

#NSW     Other       QLD       VIC 
#74.468085  5.319149 15.957447  4.255319
#=> в NSW 74.46% от хората, болни от СПИН са се заразили при кръвопреливане. Това е най-високият процент.

#г) 
#Същата е като в изпита „2019 - IzpitR2“.

#Задача 2_________

age2 = cut(age, breaks = c(0,30,60,100))
t = table(sex, age2)
#age2
#sex (0,30] (30,60] (60,100]
#F     38      37       13
#M    698    1997       56

prop.table(t ,2)
#age2
#sex     (0,30]    (30,60]   (60,100]
#F 0.05163043 0.01819076 0.18840580
#M 0.94836957 0.98180924 0.81159420

prop.table(t, 2) * 100   # ако го искаме в проценти
#age2
#sex    (0,30]   (30,60]  (60,100]
#F  5.163043  1.819076 18.840580
#M 94.836957 98.180924 81.159420

prop.table(t, 2)[1,] * 100   # ако искаме само жените
#(0,30]   (30,60]  (60,100] 
#5.163043  1.819076 18.840580

#Задача 3_____--
#построийте 90% доверителен интервал за възрастта на заразените
x = age[state == 'NSW' & T.categ == 'blood']
x
#[1] 37 54 25 44 25 55 70 16 69 52 56 57 17 57 71 58 12 11 58 47
#[21] 67 57 60 29 22 39 50 39 40  3 59 69 27 41 63 65 65  5 30 51
#[41] 32 18 46 24  5 35 69 58 44 69 46 42 52 26 53 64 16 56 42 63
#[61] 55 34 72 73 80 59 73 46 39 50

#Проверяваме дали данните са нормални. Не са нормални => wilcox.test()

wilcox.test(x, conf.int = T, conf.level = 0.9)

#90 percent confidence interval:
#  42.49999 51.00001

#Задача 4___________
#броят на неуспехите до първия успех
x = rgeom(100, 1/10)
x
#[1] 20  6  7  7  7  1  0  2  8  5  0  4  3 24  0  1  5 15  8  7
#[21]  2  1 23 14  4  2  4  5  3 29  3 16  3  3  7  0 40  1  1 52
#[41]  9  1  0 26  8  3  6  0  0  7 25 16  8  7  9  0  6  1 11  7
#[61]  7 21  1 13  6  5  6  5  6 10 12  0  4  1 36  0 21  1  2  6
#[81] 18  6 19  3  6  1  0  6  3  4  1  0 23  3 16  9 17  7  2  0

c = cut(x, breaks = c(0,4,15,100))
table(c)
#c
#(0,4]   (4,15] (15,100] 
#31       38       18 

prop.table(table(c))
#c
#(0,4]    (4,15]  (15,100] 
#0.3563218 0.4367816 0.2068966

prop.table(table(c))[2] * 100
#(4,15] 
#43.67816    # 43.67% от наблюденията са в този интервал

sum(dgeom(5:15, 1/10))
#[1] 0.405188

#Задача 5_______

library(MASS)
attach(Rubber)

#hard – твърдост на гумите
#tens – още някакъв параметър на гумите
#loss – загуби, т.е. колко бързо се изтънява гумата

#loss зависи от hard и tens. Правим подходящ линеен модел:
  
l = lm(loss ~ hard + tens, data = Rubber)
summary(l)

#Call:
#  lm(formula = loss ~ hard + tens, data = Rubber)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-79.385 -14.608   3.816  19.755  65.981 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 885.1611    61.7516  14.334 3.84e-14 ***
#  hard         -6.5708     0.5832 -11.267 1.03e-11 ***
#  tens         -1.3743     0.1943  -7.073 1.32e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 36.49 on 27 degrees of freedom
#Multiple R-squared:  0.8402,	Adjusted R-squared:  0.8284 
#F-statistic:    71 on 2 and 27 DF,  p-value: 1.767e-11

#Задача 6______
f = read.csv("Изпит R/2017 - IzpitR1 - Data2.txt", header = T, sep = ';', dec = ',')
boxplot(f)  # така ги представяме графично

#Някакви изводи по графиката:
#  Дисперсията в x1 е най-голяма, данните са най-скупчени в x3. Жените в група x2 са отслабнали най-много. Приблизително еднакво са отслабналите в група х1 и група х3. 

#За всяка от групите трябва да се провери дали жените са отслабнали с 3.5 кг. Ще го направя само за една от групите – х2:
  
#  Хипотеза H0: μ = 3.5 (отслабнали са с 3.5 кг)
#Алтернатива H1: μ ≠ 3.5 (не са отслабнали с 3.5 кг)
#Това са верни хипотези... обаче... тъй като става дума за отслабване, логично е, че би ни интересувало дали жените са отслабнали с 3.5 кг или с по-малко. Защото ако са отслабнали с по-малко килограми, то тогава ще стигнем до извода, че дадената диета не върши работа.
#Следователно ще използвам следните хипотези:
#  Хипотеза H0: μ = 3.5 (отслабнали са с 3.5 кг)
#Алтернатива H1: μ < 3.5 (не са отслабнали с 3.5 кг)

#Проверяваме дали данните в x2 са нормални. Не са нормални => wilcox.test().
wilcox.test(x2, mu = 3.5, alternative = 'less')
#V = 160, p-value = 0.9809
#p-value = 0.9809 > 0.05 => Приемаме хипотезата Н0 – отслабнали са с 3.5 кг.

