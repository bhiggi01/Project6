# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Math8009 - Project
# Brian Higgins
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Libraries
# -----------------------------------------------------------------------------
# Libraries are commented out in case you need to load them

#install.packages("dplyr")
library(dplyr)

# Library used with creating a table
#install.packages("kableExtra")
library(kableExtra)

#install.packages("e1071")
library(e1071)

## library for plots
#install.packages("ggplot2")
library(ggplot2)

# Library used with ggpiars to see plot/correlation score.
#install.packages("GGally")
library(GGally)


# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

data <- read.csv("births.csv")

# -----------------------------------------------------------------------------
# Look over data
# -----------------------------------------------------------------------------
# structure of data set
str(data)

# look at summary of data
summary(data)
View(data)

table_head <- head(data)

table_head %>%
  kbl(caption = "Head of the Data") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# -----------------------------------------------------------------------------
# 1. Get Mean, Median, Standard Deviation and IQR for each Numerica values
# -----------------------------------------------------------------------------

# ---------------------------------------------
#  0. Check on missing values and make sure ID is unique
# ---------------------------------------------
# Missing values in columns
for (i in colnames(data)){
  cat("Missing values in", i, ":", sum(is.na(data[,i])), "\n")
} 
# zero missing data

# Duplicate values
for (i in colnames(data)){
  cat("Duplicate values in", i, ":", sum(duplicated(data[,i])), "\n")
}
# Given the data type it is not surprising that some columns have many duplicates
# the most important column in this case, is the ID to make sure each entry is unique.

# clean workspare
rm(i)

# ---------------------------------------------
#  1. Explore numeric columns for mean, median, SD and IQR
# ---------------------------------------------

# bweight
bweight_stat <- data %>%
  select(bweight) %>%
  summarise_each (funs(mean, median, sd, IQR))

rownames(bweight_stat) <- "bweight"

# gestwks
gestwks_stat <- data %>%
  select(gestwks) %>%
  summarise_each (funs(mean, median, sd, IQR))

rownames(gestwks_stat ) <- "gestwks"

# matage
matage_stat <- data %>%
  select(matage) %>%
  summarise_each (funs(mean, median, sd, IQR))

rownames(matage_stat ) <- "matage"

# combine all together into a single data frame
summary_total <-rbind(bweight_stat, gestwks_stat,matage_stat)

# round all the number to 2 decimal places
summary_total <- summary_total %>% 
                        mutate(across(1:4, round, 2))

colnames(summary_total) <- c("Mean", "Medain", "Standard Deviation", "IQR")

# make the table in the same style as used across report. Table added to report.
summary_total %>%
  kbl(caption = "Summary of Numeric Columns") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# clear workspace
rm(bweight_stat, gestwks_stat,matage_stat, lowbw_stat, preterm_stat,hyp_stat,sex_stat)


# skewness test
skewness(data$bweight) #-0.9830387
skewness(data$gestwks) #-2.136681
skewness(data$matage) #-0.232371

# ---------------------------------------------
# 1.2 Graphical methods to see 
# ---------------------------------------------

# bweight
par(mfrow=c(1,2))
hist(data$bweight,main="Histogram of bweigth column", ylab="Count", xlab = "Body Weight (g)")
boxplot(data$bweight,main="Boxplot of bweigth column", xlab = "Body Weight (g)")

# gestwks
par(mfrow=c(1,2))
hist(data$gestwks,main="Histogram of gestwks column",ylab="Count",  xlab = "Time (weeks)")
boxplot(data$gestwks,main="Boxplot of gestwks column", xlab = "Time (weeks)")

# matage
par(mfrow=c(1,2))
hist(data$matage, main="Histogram of matage column",ylab="Count",  xlab = "Age (years)")
boxplot(data$matage,main="Boxplot of matage column", xlab = "Age (years)")


# return to plot at a time
par(mfrow=c(1,1))
# ---------------------------------------------
# 1.3 Shapiro Wilks test
# ---------------------------------------------
# bweight
shapiro.test(data$bweight) # 1.256e-12

# gestwks
shapiro.test(data$gestwks) #2.2e-16

# gestwks
shapiro.test(data$matage) #1.517e-05


# -----------------------------------------------------------------------------
# What proportion of babies are born prematurely? 
# What proportion of babies are born below 2,500 g? 
# What proportion of babies are born prematurely and below 2,500 g?
# -----------------------------------------------------------------------------

# ---------------------------------------------
# 1. What proportion of babies are born prematurely? 
# ---------------------------------------------

# change discrete columns into as factor
data$lowbw <- as.factor(data$lowbw)
data$preterm <- as.factor(data$preterm)
data$hyp <- as.factor(data$hyp)
data$sex <- as.factor(data$sex)
 
# What proportion of babies are born prematurely?  
prop.table(table(data$preterm)) # 0.862 0.138

# pie chart # not sure I will use this #piechartsareevil
pie(prop.table(table(data$preterm)), 
    main="Piechart of Term Percentage",
    labels = c("Normal Term 86%", "Pre Mature 14%"), col = c("#56B4E9", "#E69F00"))


# ---------------------------------------------
# 2 What proportion of babies are born below 2,500 g?
# ---------------------------------------------

# percentage of babies under 1500g
prop.table(table(data$lowbw))

# didnt notice the lowbw column at first. Below code jsut confirms the above result
# get count of babies under 2500
baby_below_2500 <- data %>% 
                    filter(bweight < 2500)%>%
                    count
# 60 babys
baby_below_2500_per <- (60/500) * 100
# 12% are below 2500



# ---------------------------------------------
# 3. What proportion of babies are born prematurely and below 2,500 g?
# ---------------------------------------------

# preterma and under 2500
data %>% 
  filter(preterm == 1) %>%
  filter(bweight < 2500)%>%
  count

# 38  babys
baby_perterm_and_below_2500<- (38/500) * 100
# 7.6% are below 2500

# -----------------------------------------------------------------------------
# Include the following boxplots and comment on the relationships (if any) between 
# birthweight and the variables.
# -------------------------------------------------------------------

# ---------------------------------------------
# 1. Birthweight by premature age
# ---------------------------------------------

#boxplot
ggplot(data, aes(preterm, bweight)) +
  geom_boxplot() +
  ggtitle("Boxplot of Birth Weight by Premature Status") +
  labs(x = "Premature Status", y=" Birth Weight (g) ") +
  scale_x_discrete(labels=c("Normal Term", "Permature Term")) +
  theme_bw() +
  geom_jitter(color="blue", size=0.4, alpha=0.9)

table(data$preterm)
prop.table(table(data$preterm))

# ---------------------------------------------
# 2. Birthweight by hypertension
# ---------------------------------------------

#boxplot
ggplot(data, aes(hyp, bweight)) +
  geom_boxplot() +
  ggtitle("Boxplot of Birth Weight by Hypertension") +
  labs(x = "Hypertension", y=" Birth Weight (g) ") +
  scale_x_discrete(labels=c("No", "Yes" )) +
  theme_bw()+
  geom_jitter(color="red", size=0.4, alpha=0.9)


table(data$hyp)
prop.table(table(data$hyp))

# -----------------------------------------------------------------------------
# Include the following scatter plots and comment on the relationships (if any) 
# between the birthweight and the variables:
# ----------------------------------------------------------------------------

# ---------------------------------------------
# 1. Birthweight versus gestational weeks
# ---------------------------------------------


ggplot(data, aes(gestwks, bweight)) +
  geom_point() +
  ggtitle("Scattter plot of Birth Weight by Gestational Weeks") +
  labs(y = "Birth Weight (g)", x=" Gestational Weeks (weeks)") +
  theme_bw()

# ---------------------------------------------
# 2. Birthweight versus maternal age
# ---------------------------------------------

ggplot(data, aes(matage, bweight)) +
  geom_point() +
  ggtitle("Scattter plot of Birth Weight by Maternal Age") +
  labs(y = "Birth Weight (g)", x=" Maternal Age (years) ") +
  theme_bw()


# ---------------------------------------------
# 3. State the correlation coefficients
# ---------------------------------------------

cor(data$gestwks, data$bweight)
cor(data$matage, data$bweight)

summary(data$gestwks, data$bweight)


# -----------------------------------------------------------------------------
# State the following linear regression models with the appropriate regression coefficients from R.
# ----------------------------------------------------------------------------

# ---------------------------------------------
# Model 1: gestwks as the independent variable
# ---------------------------------------------

Model1<-lm(data$bweight~data$gestwks)
Model1
summary(Model1)
# gestwks
#-4465.2         196.4 
# bweight = -4465.2  + 196.4 gestwks 

# ---------------------------------------------
# Model 2: matage as the independent variable
# ---------------------------------------------
Model2<-lm(data$bweight~data$matage)
Model2
# matage
#3053.87         2.44
# bweight = 3053.87 + 2.44 matage
summary(Model2)

# ---------------------------------------------
# Model 3: hyp as the independent variable
# ---------------------------------------------
Model3<-lm(data$bweight~data$hyp)
Model3
# hyp
# 3198.9       -430.7
# bweight = 3198.9 + -430.7 matage
summary(Model3)
# ---------------------------------------------
# Model 4: preterm as the independent variable
# ---------------------------------------------
Model4<-lm(data$bweight~data$preterm)
Model4
#preterm
# 3279          -1028 
#bweight = 3279 + -1028 matage
summary(Model4)


# -----------------------------------------------------------------------------
# Show the regression lines for Model 1 and Model 2 on the scatter plots for 
# birth weight versus gestational weeks and birthweight weight versus maternal age, respectively.
# Model 1: gestwks as the independent variable
# Model 2: matage as the independent variable
# ----------------------------------------------------------------------------

# plot two plots side by side
# with regression lines
par(mfrow=c(1,2))

plot(data$gestwks,data$bweight, main="Gestational Weeks by Birth Weight",
     xlab="Gestational Weeks (weeks)",ylab="Body Weight (g)")
abline(Model1)

plot(data$matage,data$bweight,main="Materal Age Weeks by Birth Weight",
     xlab="Materal Age (years)",ylab="Body Weight (g)")
abline(Model2)

# turn plot back into one plot
par(mfrow=c(1,1))


# Other models
plot(data$gestwks,data$hyp, main="Hypertension by Birth Weight",
     xlab="Hypertension",ylab="Body Weight (g)")
abline(Model3)


plot(data$matage,data$preterm,main="Permature Birth by Birth Weight",
     xlab="Premature Birth",ylab="Body Weight (g)")
abline(Model4)


# -----------------------------------------------------------------------------
# Show the summary output from R for Model 1 and explain each of the following:
# 1. The regression coefficients in the context of the data
# 2. The hypothesis tests associated with the regression coefficients. 
# Explain the relevant p-values and state the results of the hypothesis tests.
# 3. Multiplied R-squared
# 4. F-test and its p-value
# ----------------------------------------------------------------------------

# explain
summary(Model1)
# explain in report.


# -----------------------------------------------------------------------------
# Which of the four models would you use to predict birthweight? 
# Provide several reasons for your choice using the summary output in R. 
# Discuss your answer using at least three measures. 
# (It may be helpful to present your results in a table.)
# ----------------------------------------------------------------------------

summary(Model1) 
# Coefficent:  <2e-16
# Multiple R-squared:  0.5104
# F-stat: 519.2
model1 <- data.frame()
model1 <- c("<2e-16", 0.5104, 0.5094, "< 2.2e-16")

summary(Model2) 
# Coefficent:  0.739
# Multiple R-squared:  0.0002226
# F-stat: 0.1109
model2 <- data.frame()
model2 <- c(0.739, 0.0002226, -0.001785, 0.7393)

summary(Model3) 
# Coefficent:  7.73e-08
# Multiple R-squared:  0.05638
# F-stat: 29.76
model3 <- data.frame()
model3 <- c("7.73e-08", 0.05638, 0.05449 , 7.729e-08)

summary(Model4) 
# Coefficent:  <2e-16
# Multiple R-squared:  0.3098
# F-stat: 223.5
model4 <- data.frame()
model4 <- c("<2e-16", 0.3098, 0.3084 , "< 2.2e-16")


# table
model_comparsion_table <- data.frame()
model_comparsion_table <- rbind(model1, model2, model3, model4)
colnames(model_comparsion_table) = c("Coefficent:P-Value",
                                     "R-Squared","Adjusted R-Square", 
                                     "F-Statistic P-Value")
rownames(model_comparsion_table) <- c("Model 1: gestwks", "Model 2: matage ", 
                                      "Model 3: hyp", "Model 4: preterm ")

# table for report
model_comparsion_table %>%
  kbl(caption = "Model Results Table") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# -----------------------------------------------------------------------------
# Create a multiple linear regression model that includes the gestswks, matage, hyp and preterm variables.
# 1. State the model with the appropriate coefficients.
# 2. Using the R summary output, is this a better model than the simple linear regression model you selected in the previous question?
# 3. How could the multiple linear regression model be improved?
# ----------------------------------------------------------------------------

# model with all the columns
full_lm_model_v1 <- lm(data$bweight~data$gestwks + data$matage + data$hyp + data$preterm)
summary(full_lm_model_v1)
# Multiple R-squared:   0.52
# Adjusted R-squared:  0.5161 
# F statistic: < 2.2e-16
mul_model1 <- c("0.52", "0.5161", "< 2.2e-16" )


# Multiple Model 2 remove matage, preterm
full_lm_model_v2 <- lm(data$bweight~data$gestwks + data$hyp)
summary(full_lm_model_v2)
# Multiple R-squared:   0.52
# Adjusted R-squared:  0.5161 
# F statistic: < 2.2e-16
mul_model2 <- c("0.5165", "0.5146", "< 2.2e-16" )


# table
single_multiple_comparsion_table <- data.frame()

# update model1: to remvoe the coeffiecnt
model1_update <- c( 0.5104, 0.5094, "< 2.2e-16")

single_multiple_comparsion_table <- rbind(model1_update, mul_model1, mul_model2)
colnames(single_multiple_comparsion_table) = c("R-Squared","Adjusted R-Square", 
                                     "F-Statistic P-Value")
rownames(single_multiple_comparsion_table) <- c("Model 1: gestwks",
                                                "Model 5: gestwks / matage / hyp / preterm",
                                                "Model 6: geskwk / hyp")


single_multiple_comparsion_table


# table for report
single_multiple_comparsion_table %>%
  kbl(caption = "Model Results Table") %>%
  kable_classic(full_width = F, html_font = "Cambria")





