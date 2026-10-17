#===============================================================================
    #Portfolio Project: One-Way Analysis of Variance (ANOVA) & Data Tidying
#Concepts: One-Way ANOVA, Tukey HSD, Data Wrangling (Tidying), Variance Testing
#===============================================================================

#load required library for data wrangling
library(tidyr)


            #1.Textbook Costs by Field of Study (External Dataset)
# ------------------------------------------------------------------------------

#Fitting One-way ANOVA model with dataset variable
one.way.Book <- aov(TextbookCosts$Cost~TextbookCosts$Field)



#Assessing Normality Assumptions via Visual Diagnostics
hist(one.way.Book$residuals, main = "Histogram of Textbook Residuals", 
     xlab = "Residuals", 
        col = "lightblue")


qqnorm(TextbookCosts$Cost, main = "Normal Q-Q Plot (Textbook Costs)")
qqline(TextbookCosts$Cost, col = "red")



# Assess Homoscedasticity (Equal Variance)
sd_by_category <- tapply(TextbookCosts$Cost, TextbookCosts$Field, sd)
vector <- sd_by_category
result <- FALSE



# Evaluate if the largest standard deviation is more than twice the smallest
for (i in 1:length(sd_by_category)) {
  for (j in 1:length(sd_by_category)) {
    if (i != j && sd_by_category[i] > 2 * sd_by_category[j]) {
      result <- TRUE
      break
    }
  }
  if (result) break
}
print(paste("Equal Variance Violation Detected (Max SD > 2x Min SD):", result))



#ANOVA and Post-Hoc Pairwise Comparisons
summary(one.way.Book)
TukeyHSD(one.way.Book)


                #2.Paramedic Call Volume by Shift(Data Provided)
# ------------------------------------------------------------------------------

# Manipulating raw observations into a data frame format
ShiftData <- data.frame(
  Morning=c(2,3,2,4,3,1,3),
          Afternoon=c(3,4,4,5,3,2,5),
                        Night=c(5,4,5,3,2,5,6))



# Tidying data layout from wide format to long format for structural modeling
long_ShiftData <- pivot_longer(ShiftData, 
        col=everything(), 
                  names_to = 'shift', 
                        values_to = 'Number of Calls')



#Fitting One-way ANOVA model with variables
one.way.Shift <- aov(long_ShiftData$`Number of Calls`~long_ShiftData$shift)



# Assess Normality Assumptions
hist(one.way.Shift$residuals, main = "Histogram of Shift Residuals", 
     xlab = "Residuals", 
        col = "lightgreen")

qqnorm(long_ShiftData$`Number of Calls`, main = "Normal Q-Q Plot (Call Volume)")
qqline(long_ShiftData$`Number of Calls`, col = "red")



#Assessing Homoscedasticity (Equal Variance)
sd_by_shift <- tapply(long_ShiftData$`Number of Calls`, long_ShiftData$shift, sd)
vector <- sd_by_shift
result_shift <- FALSE



# Evaluate if the largest standard deviation is more than twice the smallest
for (i in 1:length(sd_by_shift)) {
  for (j in 1:length(sd_by_shift)) {
    if (i != j && sd_by_shift[i] > 2 * sd_by_shift[j]) {
      result_shift <- TRUE
      break
    }
  }
  if (result_shift) break
}
print(paste("Equal Variance Violation Detected (Max SD > 2x Min SD):", result_shift))



#ANOVA and Post-Hoc Pairwise Comparisons
summary(one.way.Shift)
TukeyHSD(one.way.Shift)






