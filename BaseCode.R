m <- mean(mtcars$mpg)
s <- sd(mtcars$mpg)
z <- qnorm(0.05)
mu0 <- mn - z * s /sqrt(nrow(mtcars))
mu0

x <- 1:4
p <- x/sum(x)
temp <- rbind(x, p)
rownames(temp) <- c("x", "prob")
temp

## what is the mean?
sum(x * p)