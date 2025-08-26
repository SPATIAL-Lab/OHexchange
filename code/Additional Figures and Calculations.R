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
(mean(t1diff) + mean(t2diff) + mean(t3diff) + mean(t4diff) + mean(t6diff))/5 #Average is ~0.79

SD1 <- c(mean(t1diff), mean(t2diff), mean(t3diff), mean(t4diff), mean(t6diff))
sd(SD1) # Standard deviation is ~0.13

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

# Rinse d18O vs D18O
mmat <- rbind(t1diff, t2diff, t3diff) #first getting the averages for each tooth for each treatment with each rinse
lmat <- rbind(t4diff, t6diff)
mr_avg <- colMeans(mmat)
lr_avg <- colMeans(lmat)

data_list3 <- list(lr_avg, mr_avg, t7diff, t5diff)
boxplot(data_list3, at = x_positions, names = x_positions,
        boxwex = 20,
        xaxt = "n",
        xlim = c(-50, 260),
        xlab = expression("Rinse δ"^18 * "O (‰)"),
        ylab = expression("Δ"^18 * "O (‰)"),
        main = expression("Correlation Between Rinse δ"^18 * "O and Shift Magnitude"))
axis(side = 1, at = seq(-45, 260, by = 25))
abline(v = 0, lty = 2, col = "black")

# Acid d18O vs D18O
mamat <- rbind(t2diff, t4diff, t5diff, t6diff, t7diff)
ma_avg <- colMeans(mamat)
x_positions2 <- c(-46.90, -10.29, 250)

data_list4 <- list(t1diff, ma_avg, t3diff)
boxplot(data_list4, at = x_positions2, names = x_positions2,
        boxwex = 20,
        xaxt = "n",
        xlim = c(-50, 260),
        xlab = expression("Acid δ"^18 * "O (‰)"),
        ylab = expression("Δ"^18 * "O (‰)"),
        main = expression("Correlation Between Acid δ"^18 * "O and Shift Magnitude"))
axis(side = 1, at = seq(-45, 260, by = 25))
abline(v = 0, lty = 2, col = "black")

# Figure to show lack of correlation between D18O and untreated enamel value
# Big Delta vs Little Delta
# Increase margins for legend
par(mar = c(6, 4, 4, 8))
# Make plot
par(mar = c(5, 5, 5, 7))
plot(teethdata$UT.Data, -1* (teethdata$UT.T5), col = "salmon", 
     pch = 19, 
     xlab = expression("δ"^18 * "O of Control Teeth (‰)"), 
     ylab = expression("Δ"^18 * "O (‰)"), 
     main = expression("Differences in δ"^18 * "O between Control, Treatments"), 
     ylim = c(0.2, 5))
points(teethdata$UT.Data, -1* (teethdata$UT.T2), col = "#7eb0d5", pch = 19)
points(teethdata$UT.Data, -1* (teethdata$UT.T3), col = "#297770", pch = 19)
points(teethdata$UT.Data, -1* (teethdata$UT.T4), col = "#841618", pch = 19)
points(teethdata$UT.Data, -1* (teethdata$UT.T1), col = "#ffb400", pch = 19)
points(teethdata$UT.Data, -1* (teethdata$UT.T6), col = "#776bcd", pch = 19)
points(teethdata$UT.Data, -1* (teethdata$UT.T7), col = "#b2e061", pch = 19)
# Legend
legend("topright",
       legend = c("Treatment 1", "Treatment 2", "Treatment 3", "Treatment 4", "Treatment 5", "Treatment 6", "Treatment 7"),
       fill = c("#ffb400", "#7eb0d5", "#297770", "#841618", "salmon", "#776bcd", "#b2e061"),
       bty = "y",
       title = "Legend",
       inset = c(-0.425, 0),
       xpd = NA,
       cex = 0.9)


#anova testing
library(tidyr)
anova <- read.csv("data/ANOVA.csv")

anovatooth <- aov(T1.Data + T2.Data + T3.Data + T4.Data + T5.Data + T6.Data + T7.Data ~ Tooth.Number, data = teethdata)
summary(anovatooth)

# ANOVA of all treatments
longformat1 <- pivot_longer(anova,  #must reorient the data so the ANOVA runs properly
                           cols = starts_with("Tooth"),  
                           names_to = "Tooth.ID",
                           values_to = "Value")

anova_treatment <- aov(Value ~ Treatment, data = longformat1)
summary(anova_treatment) # P-value 0.333

#Anova of treatments 1-4, 6
subset_data <- subset(anova, Treatment %in% c(1, 2, 3, 4, 6))

longformat2 <- pivot_longer(subset_data,
    cols = starts_with("Tooth"),
    names_to = "Tooth.ID",
    values_to = "Value")
 
anova_sub_t <- aov(Value ~ Treatment, data = longformat2)
summary(anova_sub_t) # P-value = 0.747, indicating that the difference between these groups is not statistically significant

# Anova of all trials except 7
all_but_7 <- subset(anova, Treatment %in% c(1, 2, 3, 4, 5, 6))

longformat3 <- pivot_longer(all_but_7,
                            cols = starts_with("Tooth"),
                            names_to = "Tooth.ID",
                            values_to = "Value")

anova_ab7 <- aov(Value ~ Treatment, data = longformat3)
summary(anova_ab7) #P-value 0.0278

# Anova of all trials except 5
all_but_5 <- subset(anova, Treatment %in% c(1, 7))

longformat4 <- pivot_longer(all_but_5,
                            cols = starts_with("Tooth"),
                            names_to = "Tooth.ID",
                            values_to = "Value")

anova_ab5 <- aov(Value ~ Treatment, data = longformat4)
summary(anova_ab5)

#T-Test for 4 vs 4r (see if the difference is significant)

t.test(t4diff, t6diff)
#The p-value is greater than 0.05, and thus we can conclude that the difference between these two groups is not statistically significant.
