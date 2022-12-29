install.packages("UsingR")
x=c(1,2,3)

y = 50
s = sum((y^(1:100)))


pp = data.frame(
  age = c(18, 23, 25, 35, 65, 54, 34, 56, 72, 19, 23, 42, 18, 39, 37),
  pulse = c(202, 186, 187, 180, 156, 169, 174, 172, 153, 199, 193, 174, 198, 183, 178)
)
model  = lm(pulse ~ age,data = pp)
summary(model)


tscore  = (-0.79773 + 1)/0.06996
pvalue = pt(tscore,df = 13,lower.tail = F)
pvalue

