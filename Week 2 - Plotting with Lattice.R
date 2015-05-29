# Week 2 Lecture - Plotting with Lattice
# Lecture 1
library(lattice)
library(datasets)

# Basic xyplot
xyplot(Ozone ~ Wind,data = airquality)

# Basic xyplot conditioned by Month (factor)
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))

# Save plot, do not auto-print
p <- xyplot(Ozone ~ Wind,data = airquality)
print(p)

# Lecture 2
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y = x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c(2, 1))

# Modify the panel function to add a median line
xyplot(y ~ x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.abline(h = median(y), lty = 2)
})

# Modify the panel to add a regression line
xyplot(y ~ x | f, panel = function(x, y, ...) {
    panel.xyplot(x, y, ...)
    panel.lmline(x, y, col = 2)
    panel.rug(x)
})



