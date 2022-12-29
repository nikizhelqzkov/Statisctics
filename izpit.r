f = function(n,x){
  r= 0
  for (s in 1:n) {
    r = r + (x^s*s^n)/x^n + n^2
  }
  r
}
f(20,0.01)


#2
barplot(table(mtcars$gear,mtcars$cyl),legend=T)
prop.table(table(mtcars$cyl==6,mtcars$carb==1),2)[2,2]

i = order(mtcars$mpg)
m  = mean(mtcars[i[1:6],]$wt)


#3
prop.table(table(mtcars$cyl==6))




model  = lm(mtcars$carb ~ mtcars$cyl)
summary(model)


#4
a= morley[morley$Expt==1,]$Speed
b= morley[morley$Expt==2,]$Speed
c= morley[morley$Expt==3,]$Speed
d= morley[morley$Expt==4,]$Speed
e= morley[morley$Expt==5,]$Speed
mean(a)
mean(b)
mean(c)
mean(d)
mean(e)

#5

x = rgeom(1000,1/6)
summary(x)
t  = table(x>=10)
prop.table(t)
sum(x>=10)/1000
pgeom(10,1/6, lower.tail = F)




#6
df = data.frame(beer = c(5,2,9,8,3,7,3,5,3,5),
                alcInB = c(0.1,0.03,0.18,0.12,0.04,0.11,0.07,0.06,0.02,0.05))

model  = lm(df$alcInB~df$beer)
s = summary(model)

beta1 = s$coefficients[2,1]
se = s$coefficients[2,2]
t = (beta1 - 0.01)/se
p = pt(t,df = 95)
#p = 0.04.5 => h1 othvurlqme hipotezata;
#
s$


d = data.frame(x = c(7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5))
predict.lm(model,d,interval = "confidence",conf.level = 0.92)

plot(model)
abline(model)
prop.test(c(df$beer,df$beer+1),c(df$alcInB,df$alcInB+0.01),alternative = "less")
