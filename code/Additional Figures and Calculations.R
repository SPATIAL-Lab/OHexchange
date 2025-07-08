library(ggplot2)

teethdata <- read.csv("data/Data for R.csv")

#Required Components
t1diff <- (teethdata$UT.T1) * -1
t2diff <- (teethdata$UT.T2) * -1
t3diff <- (teethdata$UT.T3) * -1
t4diff <- (teethdata$UT.T4) * -1
t5diff <- (teethdata$UT.T5) * -1
t6diff <- (teethdata$UT.T6) * -1
t7diff <- (teethdata$UT.T7) * -1

#Average D18O of t1-4, t6
(mean(t1diff) + mean(t2diff) + mean(t3diff) + mean(t4diff) + mean(t6diff))/5



# New Plot With Everything

data_list2 <- list(t1diff, t2diff, t3diff, t4diff, t5diff, t6diff, t7diff)
par(mar = c(5, 7, 4, 9))
boxplot(data_list2, 
        main = expression("Treatment Results"), 
        ylab = expression("Δ"^18 * "O (‰)"), 
        xlab = "Treatment Number",
        ylim = c(-0.5, 4.5),
        col = c("lightblue", "salmon", "#EFD58A", "salmon", "salmon", "salmon", "salmon"),
        border = c("#841618", "#841618", "#841618", "#297770", "black","#297770", "#8E5722"),
        xaxt = "n")
# Dotted Line
abline(h = 0, col = "black", lty = 2)
#X-axis numbers
axis(1, at = 1:7, labels = c("1", "2", "3", "4", "5", "6", "7"))
# First Legend
legend("topright", 
       legend = c("Light Acid", "Medium Acid", "Heavy Acid", "Extra Heavy Acid"),
       fill = c("lightblue", "salmon", "#F6E5C3", "#EFD58A"),          
       bty = "y",                   
       title = "Fill",          
       inset = c(-0.70, 0),
       xpd = NA,
       cex = 0.9) 
# Second Legend
legend("bottomright",
       legend = c("Light Rinse", "Medium Rinse", "Heavy Rinse", "Extra Heavy Rinse"),
       fill = c("#297770", "#841618", "#8E5722", "black"),
       bty = "y",
       title = "Outline",
       inset = c(-0.74, -0.15),
       xpd = NA,
       cex = 0.9)


#Plot with just treatments 1-4 and the T4 redo

data_list <- list(t1diff, t2diff, t3diff, t4diff, t4rdiff)
par(mar = c(5, 7, 4, 9))
boxplot(data_list, xaxt = "n",
        main = expression("T1 through T4 with T4 redo"), 
        ylab = expression("Δ"^18 * "O (‰)"), 
        xlab = "Treatment Number",
        ylim = c(-0.25, 1.75),
        col = c("lightblue", "salmon", "#F6E5C3", "salmon", "salmon"),
        border = c("#841618", "#841618", "#841618", "#297770", "#297770"))
# Dotted Line
abline(h = 0, col = "black", lty = 2)
#X-axis markers
axis(1, at = 1:5, labels = c("1", "2", "3", "4", "4 redo"))
# First Legend
legend("topright", 
       legend = c("Light Acid", "Medium Acid", "Heavy Acid"),
       fill = c("lightblue", "salmon", "#F6E5C3"),          
       bty = "y",                   
       title = "Fill",          
       inset = c(-0.4, 0),
       xpd = NA) 
# Second Legend
legend("right",
       legend = c("Light Rinse", "Medium Rinse", "Heavy Rinse"),
       fill = c("#297770", "#841618", "#8E5722"),
       bty = "y",
       title = "Outline",
       inset = c(-0.425, 0),
       xpd = NA)

#T4 vs T4R vs T4A

data_list <- list(t4diff, t4rdiff, t4adiff)
par(mar = c(5, 7, 4, 9))
boxplot(data_list, xaxt = "n",
        main = expression("Original T4, T4 Redo, and T4 Average"), 
        ylab = expression("Δ"^18 * "O (‰)"), 
        xlab = "Treatment Number",
        ylim = c(-0.25, 1.75),
        col = c("salmon", "salmon", "salmon"),
        border = c("#297770","#297770","#297770"))
# Dotted Line
abline(h = 0, col = "black", lty = 2)
#X-axis markers
axis(1, at = 1:3, labels = c("T4", "T4 redo", "T4 avg"))
# First Legend
legend("topright", 
       legend = c("Light Acid", "Medium Acid", "Heavy Acid"),
       fill = c("lightblue", "salmon", "#F6E5C3"),          
       bty = "y",                   
       title = "Fill",          
       inset = c(-0.4, 0),
       xpd = NA) 
# Second Legend
legend("right",
       legend = c("Light Rinse", "Medium Rinse", "Heavy Rinse"),
       fill = c("#297770", "#841618", "#8E5722"),
       bty = "y",
       title = "Outline",
       inset = c(-0.425, 0),
       xpd = NA)

#anova testing
anova <- read.csv("ANOVA sans 5.csv")

anovatooth <- aov(T1.Data + T2.Data + T3.Data + T4.Data + T5.Data + T4.Redo.Data + T5.Redo.Data ~ Tooth.Number, data = teethdata)
summary(anovatooth)

anovatreatment <- aov(Tooth.1 + Tooth.2 + Tooth.3 + Tooth.4 + Tooth.5 + Tooth.6 + Tooth.7 + Tooth.8 + Tooth.9 + Tooth.10 ~ Treatment, data = anova)
summary (anovatreatment)

#T-Test for 4 vs 4r (see if the difference is significant)

t.test(t4diff, t6diff)
#The p-value is greater than 0.05, and thus we can conclude that the difference between these two groups is not statistically significant.
