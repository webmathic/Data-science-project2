library('tidyverse')
library('mice')
library('ISLR')
library('tidyr')
library('dplyr')
library('corrplot')
library("faraway")
library("olsrr")

lifeesxpdata <- read.csv('C:/Users/HP/Desktop/uni essex/Model experimental data/Assignment/Group_coursework_task_and_data-20221111/life_expectancy_data1.csv', header=T)
head(lifeesxpdata)

set.seed(101231)
#summary of the datasets
summary(lifeesxpdata)

#checking the total number of values in each column
missing.values <- lifeesxpdata %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))
View(missing.values)

#viewing the missing data graphically
missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing values", 
       title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#mean imputation of Life expectancy at birth, total (years)
mean1 <- mean(lifeesxpdata$SP.DYN.LE00.IN, na.rm = TRUE)

#relacing n/a with the mean 
lifeesxpdata[is.na(lifeesxpdata$SP.DYN.LE00.IN), "SP.DYN.LE00.IN"] <- mean1

#mean imputation of Access to electricity (\% of population)
mean2 <- mean(lifeesxpdata$EG.ELC.ACCS.ZS, na.rm = TRUE)

#replacing n/a with the mean 
lifeesxpdata[is.na(lifeesxpdata$EG.ELC.ACCS.ZS), "EG.ELC.ACCS.ZS"] <- mean2

#instead of imputing mean in the column one after the other, we can do it at once
#using the for loop.
for(i in 4:ncol(lifeesxpdata)){
  lifeesxpdata[, i][is.na(lifeesxpdata[, i])]<-mean(lifeesxpdata[, i], na.rm = TRUE)
}

#view the data set
View(lifeesxpdata)

#summary of the dataset to view if there are still n/a values
summary(lifeesxpdata)

#there are no data giving for Renewable energy consumption (\% of total final energy consumption).
#therefore, we will delete the column to make our computation easier.
lifeesxpdata <- lifeesxpdata[, ! names(lifeesxpdata) %in% c("EG.FEC.RNEW.ZS")]

#summary of the dataset to check if there are still n/a values
summary(lifeesxpdata)

# Exploratory data analysis
# Calculating average life expectancy across continents
avg.cont <- lifeesxpdata %>%
  select(Continent, SP.DYN.LE00.IN) %>%
  group_by(Continent) %>%
  summarise(avg_life_expectancy = mean(SP.DYN.LE00.IN))

# Display the average life expectancy by continent
print(avg.cont)

# Create a bar plot of average life expectancy by continent
bar_plot <- ggplot(avg.cont, aes(x = Continent, y = avg_life_expectancy)) +
  geom_bar(stat = "identity") +
  labs(x = "Continent", y = "Average Life Expectancy") +
  ggtitle("Average Life Expectancy by Continent")

# Display the bar plot
print(bar_plot)


# Let's find the most significant predictor variable and also buils a model with linear regression
#correlation plot
corrplot.mixed(correlation, lower.col = "black", number.cex = .8)


# Select the numeric columns for correlation analysis
numeric_cols <- sapply(lifeesxpdata, is.numeric)
data_numeric <- lifeesxpdata[, numeric_cols]

# Exclude the response variable
response_variable <- "SP.DYN.LE00.IN"
data_numeric <- data_numeric[, !(colnames(data_numeric) %in% response_variable)]

# Calculate the correlation matrix
cor_matrix <- cor(data_numeric)

# View the correlation matrix
print(cor_matrix)

# Install and load the "car" package
install.packages("car")
library(car)

# Calculate VIF
vif_results <- vif(data_numeric)

# View the VIF values
print(vif_results)



# Set the threshold for maximum acceptable VIF
vif_threshold <- 5

# Find variables with VIF above the threshold
high_vif_vars <- names(vif_results[vif_results > vif_threshold])

# Remove variables with high VIF
lifedata_filtered <- data_numeric[, !(colnames(data_numeric) %in% high_vif_vars)]

# View the filtered dataset
print(lifedata_filtered)

# Extract the response variable data
life_expectancy <- lifeesxpdata[, response_variable]

# Add the response variable back to the dataset
lifedata_filtered <- cbind(lifedata_filtered, life_expectancy)



# View the updated dataset
head(lifedata_filtered)

# Create a linear regression model
model1 <- lm(life_expectancy ~ ., data = lifedata_filtered)

# Print the summary of the linear regression model
summary(model1)
plot(model1)

# we can also try our variable selection method to check our result
model2<-step(model1,method="backward")
summary(model2)
plot(model2)
print(model2)
anova(model1,model2)


# Based on the table, Model 2 has a higher residual sum of squares (RSS) compared to Model 1.
# However, the F-statistic is relatively low (0.4798), and the corresponding p-value is high (0.9249).
# This indicates that the difference between Model 1 and Model 2 is not statistically significant.
# Therefore, it suggests that the simpler Model 2, which includes fewer predictor variables,
# may be preferred as it achieves a similar fit to the data without the need for the additional variables present in Model 1.
# Therefore, we reject the null hypothesis

#lets print the coefficient and confident interval
coefficients(model2)
confint(model2)

#For the sake of neatness
round(cbind("Effect" = coefficients(model2),confint(model2) ), digits=3)

# The following table displays the effects and corresponding 95% confidence intervals for the coefficients in Model 2:

# The interpretation of the coefficients is as follows:

# - The estimated intercept (73.087) represents the estimated mean life expectancy when all predictor variables are at zero.
# - A one-unit increase in EG.ELC.ACCS.ZS (access to electricity) is associated with a mean life expectancy increase of 0.062 years, with a 95% confidence interval of [0.036, 0.087].
# - A one-unit increase in SE.PRM.CUAT.ZS (completion rate of primary education) is associated with a mean life expectancy decrease of 0.063 years, with a 95% confidence interval of [-0.126, 0.001].
# - A one-unit increase in SP.DYN.IMRT.IN (infant mortality rate) is associated with a mean life expectancy decrease of 0.211 years, with a 95% confidence interval of [-0.249, -0.174].
# - A one-unit increase in SH.XPD.CHEX.PC.CD (current health expenditure per capita) is associated with a mean life expectancy increase of 0.000 years, with a 95% confidence interval of [0.000, 0.001].
# - A one-unit increase in SH.XPD.CHEX.GD.ZS (current health expenditure as a percentage of GDP) is associated with a mean life expectancy increase of 0.136 years, with a 95% confidence interval of [-0.021, 0.294].
# - A one-unit increase in NY.GDP.PCAP.CD (GDP per capita) is associated with a mean life expectancy increase of 0.000 years, with a 95% confidence interval of [0.000, 0.000].
# - A one-unit increase in SH.HIV.INCD (HIV incidence) is associated with a mean life expectancy increase of 0.000 years, with a 95% confidence interval of [0.000, 0.000].
# - A one-unit increase in SH.H2O.SMDW.ZS (access to improved water sources) is associated with a mean life expectancy increase of 0.037 years, with a 95% confidence interval of [0.015, 0.059].

# These interpretations are based on the coefficients and confidence intervals obtained from Model 2.