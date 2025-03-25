# Good Code Example: Calculating the mean of a vector
x <- c(1, 2, 3, 4, 5)
mean_x <- mean(x)
print(mean_x)


# Bad Code Example:  Calculating the mean with unnecessary steps and unclear variable names
a <- 1:5 # less descriptive variable name
b <- sum(a)
n <- length(a)
c <- b / n #unclear variable names, breaks readability
print(c)


# Good Code Example: Linear Regression
data("mtcars")
model <- lm(mpg ~ wt, data = mtcars)
summary(model)


# Bad Code Example: Linear Regression with poor variable naming and no summary
dat <- mtcars #poor variable name
mod <- lm(mpg~wt,data=dat) #poor variable name, no summary
mod


#Good Code Example: Data Frame Manipulation
df <- data.frame(a = 1:3, b = letters[1:3])
df_subset <- subset(df, a > 1)
print(df_subset)


#Bad Code Example: Data Frame Manipulation with hard to read code.
data.frame(a=1:3,b=letters[1:3]) -> data #poor variable name, using -> for assignment is uncommon
data[data$a>1,] -> subset_data #poor readability and variable name
print(subset_data)
