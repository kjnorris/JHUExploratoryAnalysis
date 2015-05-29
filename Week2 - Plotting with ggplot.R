# Week 2 - Plotting with ggplot
library(ggplot2)

# qplot intro - Lecture 1 & 2
# Basic plot
qplot(displ, hwy, data = mpg)

# Separate class by color
qplot(displ, hwy, data = mpg, color = drv)

# Add a smoother
qplot(displ, hwy, data = mpg, geom =c("point", "smooth"))

# Create a histogram
qplot(hwy, data = mpg, fill = drv)

# Faceting
qplot(displ, hwy, data = mpg, facets = . ~ drv)
qplot(displ, hwy, data = mpg, facets = drv ~ class)

# faceted histogram
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)

# ggplot - Lecture 3 - 5
g <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(color = "orange", size = 2) +
    geom_smooth(method = "lm") +
    facet_grid(. ~ drv) +
    xlab("Engine Displacement") +
    ylab("Highway Mileage") +
    theme_bw(base_family = "Times")

print(g)

# Axis Limits
testdata <- data.frame(x=1:100, y=rnorm(100))
# Add an outlier
testdata[50,2] <- 100
# Base plot crops outlier out
plot(testdata$x, testdata$y, type = "l", ylim = c(-3, 3))
# ggplot scale is wrong to accommodate outlier
ggplot(testdata, aes(x=x, y=y)) + geom_line()

# Skips outlier in subset of data
ggplot(testdata, aes(x=x, y=y)) +
    geom_line() +
    ylim(-3, 3)

# Correctly handles outlier as base plot does
ggplot(testdata, aes(x=x, y=y)) +
    geom_line() +
    coord_cartesian(ylim = c(-3, 3))


