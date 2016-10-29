getwd()

ERWaiting <- read.csv("ERWaiting.csv")

ERWaiting <- melt(ERWaiting, variable.name = "hospital", value.name = "waitingTime", id.vars = "day")
install.packages(data.table)
library(data.table)
names(ERWaiting)[1] <- "id"
names(ERWaiting)[2] <- "hospital"
names(ERWaiting)[3] <- "waitingTime"

library(foreign)

library(reshape)
library(Hmisc)
library(car)
library(pastecs)
library(reshape2) # for restructuring the data
library(dplyr) # for data preparation
library(e1071) #skewness and kurtosis
library(car) # for levenes test
library(ggplot2) # for fancy plotting 
library(knitr)
summary(ERWaiting)

#global Setting 
alpha <- 0.05

# Descriptive summary

stat.desc(ERWaiting)
qqnorm(ERWaiting$waitingTime)
qqline(ERWaiting$waitingTime)
ggsave("HW2 graph/Anovaqq.pdf")


table(stat.desc(ERWaiting$waitingTime))

#anova
options(scipen = 100)
options(digits=2)
fit <- lm(data = ERWaiting, waitingTime~hospital)
ERWaiting.aov <- aov(fit)
ERWaiting.aov.summary <- summary(ERWaiting.aov)
ERWaiting.tukey <- TukeyHSD(ERWaiting.aov)
plot(ERWaiting.tukey)
ggsave("HW2 graph/confidencelevel.pdf")
#check normality
ERWaiting.normal <- data.frame(skew = ERWaiting$skew, kurt = ERWaiting$kurtosis)
## HOV
ERWaiting.levene <- leveneTest(fit)

#degree of Freedom
df <- anova(fit)[, "Df"]
names(df) <- c("between", "within")
#getting f value
ERWaiting.f.crit <- qf(alpha, df["between"], df["within"], lower.tail = FALSE)
ERWaiting.f.value <- ERWaiting.aov.summary[[1]]$F[1]
# Plot ANOVA results 
ST <- shapiro.test(ERWaiting$waitingTime)
ST
SC <- plot(ERWaiting$hospital)
SC
ggsave("HW2 graph/Anovascatterplot.pdf")
HS <- hist(ERWaiting$waitingTime)
HS
ggsave("HW2 graph/Anovahistogram.pdf")
#boxplot

bp <- ggplot(ERWaiting, aes(x = hospital, y = waitingTime)) +
  geom_boxplot() + 
  labs(y = "Average Waiting Time", x = "Hospitals") 
bp
# Save plot
ggsave("HW2 graph/Anovaboxplot.pdf")

