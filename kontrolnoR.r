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

Ndice <- function(k) {
  x <- sample( c(1:6), 10, replace=T )
  v <- c(6,6,6) %in% x
  return(sum(v) == 20)
}
c(6,6,6) %in% c(6,3,5,4,2,5,2)

probOfSucces <- function(repetitions, k) {
  res <- replicate( repetitions, boughtGum(k) )
  sum(res)/repetitions
}
gum = 100
while (probOfSucces(10000,gum) != 1){
  gum <- gum + 1
}
paste("bought gums: ",gum)

beers <- c(5,2,9,8,3,7,3,5,3,5);
alcohol <- c(0.1, 0.03, 0.18, 0.12, 0.04, 0.11, 0.07, 0.06, 0.02, 0.05);
plot(beers,alcohol, pch = 20, xlab="Beers", ylab="Alcohol")
abline(lm(alcohol ~ beers), col = "red")
summary(lm(alcohol ~ beers))
data <- data.frame(beers, alcohol);
plot(data);
model1 <- lm(data$alcohol ~ data$beers);
abline(model1);
s = summary(model1)
s
beta0 <- s$coefficients[2,1];
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


