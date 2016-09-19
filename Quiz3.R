## Question 4 - In a study of emergency room waiting times, investigators consider a new and the standard triage systems. To test the systems, administrators selected 20 nights and randomly assigned the new triage system to be used on 10 nights and the standard system on the remaining 10 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 3 hours with a variance of 0.60 while the average MWT for the old system was 5 hours with a variance of 0.68. Consider the 95% confidence interval estimate for the differences of the mean MWT associated with the new system. Assume a constant variance. What is the interval? Subtract in this order (New System - Old System). 

quantile <- 0.975 # is 95% with 2.5% on both sides of the range
n_new <- 10 ## nights new system
n_old <- 10 ## nights old system
var_new <- 0.60 ## variance of new system obs
var_old <- 0.68 ## variance of old system obs
avg_new <- 3 ## average wait hours new system
avg_old <- 5 ## average wait hours old system

## Calculated pooled standard deviation
sd <- sqrt(((n_old - 1) * var_old + (n_new - 1) * var_new)/(n_old + n_new -2))

## confidence interval
confInterval <- avg_new - avg_old + c(-1,1) * qt(quantile, df=n_new + n_old -2) * sd * (1 / n_new + 1 / n_old)^.5
round(confInterval, 2)

## Question 6
## To further test the hospital triage system, administrators selected 200 nights and randomly assigned a new triage system to be used on 100 nights and a standard system on the remaining 100 nights. They calculated the nightly median waiting time (MWT) to see a physician. The average MWT for the new system was 4 hours with a standard deviation of 0.5 hours while the average MWT for the old system was 6 hours with a standard deviation of 2 hours. Consider the hypothesis of a decrease in the mean MWT associated with the new treatment. What does the 95% independent group confidence interval with unequal variances suggest vis a vis this hypothesis? (Because there’s so many observations per group, just use the Z quantile instead of the T.)

quantile <- 0.975 # is 95% with 2.5% on both sides of the range


n_y <- 100 # nights new system
n_x <- 100 # nights old system
σ_y <- 0.50# σ new 
σ_x <- 2# σ old 
μ_y <- 4# average hours new system
μ_x <- 6# average hours old system
# calculate pooled standard deviation
σ_p <- sqrt(((n_x - 1) * σ_x^2 + (n_y - 1) * σ_y^2)/(n_x + n_y - 2))

confidenceInterval <-  μ_x - μ_y + c(-1, 1) * qnorm(quantile) * σ_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,2)

## Question 7
## Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo. Subjects’ body mass indices (BMIs) were measured at a baseline and again after having received the treatment or placebo for four weeks. The average difference from follow-up to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2 for the treatment group and 1.8 kg/m2 for the placebo group. Does the change in BMI over the four week period appear to differ between the treated and placebo groups? Assuming normality of the underlying data and a common population variance, calculate the relevant *90%* t confidence interval. Subtract in the order of (Treated - Placebo) with the smaller (more negative) number first.

quantile = 0.95 # is 90% with 5% on both sides of the range

n_y <- 9   # subjects treated
n_x <- 9   # subjects placebo
σ_y <- 1.5 # kg/m2 std.dev. treated 
σ_x <- 1.8 # kg/m2 std.dev. placebo 
μ_y <- -3  #  kg/m2 average difference treated
μ_x <- 1   #  kg/m2 average difference placebo

# calculate pooled standard deviation
σ_p <- sqrt(((n_x - 1) * σ_x^2 + (n_y - 1) * σ_y^2)/(n_x + n_y - 2))

confidenceInterval <-  μ_y - μ_x + c(-1, 1) * qt(quantile, df=n_y+n_x-2) * σ_p * (1 / n_x + 1 / n_y)^.5
round(confidenceInterval,3)
