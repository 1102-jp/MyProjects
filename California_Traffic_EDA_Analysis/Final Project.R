#####This is the Dataset for state of California

#clears global evn
rm(list = ls())
#clears packages
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE)
#clears plots
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
#disables scientific notation for entire R session
options(scipen = 100)
options(digits = 2)
#clears console
cat("\014")

install.packages("pacman")
library(pacman)
library(tidyverse)

filename <- file.choose()
data <- readRDS(filename)

#reduce my table to feature only relevant columns
data <- data[, c("date", "location", "subject_race", "subject_age", "subject_sex", "type", "reason_for_stop", "search_conducted", "contraband_found", "outcome")]

#change column as factor
data$reason_for_stop <- factor(data$reason_for_stop)

#examples of other ways to change columns types. these already
data$date <- as.Date(data$date)
data$subject_race <- factor(data$subject_race)
data$subject_sex <- factor(data$subject_sex)
data$type <- factor(data$type)
data$search_conducted <- as.logical(data$search_conducted)
data$contraband_found <- as.logical(data$contraband_found)
data$outcome <- factor(data$outcome)


library(dplyr)
library(lubridate)
#add new column with day of week
data <- data |> mutate(dayofweek = wday(date, label = TRUE))

#frequency table of stops by race
table(data$subject_race)
race <- data |> group_by(subject_race) |> summarize(counts = n())
view(race)

#cross-tabulation of race and search conducted
table(data$subject_race, data$search_conducted)

#remove other, unknown and NA from table
data <- data |> filter(subject_race != 'unknown', subject_race != 'race', subject_race != 'NA', subject_race != 'other')

#more advanced cross-tabulation
install.packages('gmodels')
library(gmodels)

CrossTable(data$subject_race, data$search_conducted, prop.chisq = FALSE)

library(ggplot2)
#bar chart of stops by day of week
ggplot(data, aes(x = dayofweek)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Traffic Stops",
       x = "Day of the Week", y = "Frequency")

#bar plot of stops by race
ggplot(data, aes(x = subject_race, fill = subject_race)) +
  geom_bar() +
  labs(title = "Traffic Stops by Race", x = "Race", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(tidyverse)
#calculate proportion of searches for each race
search_proportions <- data |>
  group_by(subject_race) |>
  summarise(search_rate = mean(search_conducted, na.rm = TRUE))

view(search_proportions)

install.packages("formattable")
library(formattable)
library(dplyr)
library(ggplot2)
ggplot(search_proportions, aes(x = subject_race, y = search_rate, 
                               fill = subject_race)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=percent(search_rate)), vjust=-0.3, size=3.5)+
  labs(title = "Search Rates by Race", 
       x = "Race", 
       y = "Proportion of Stops Resulting in Search") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent)

install.packages("plotly")
library(plotly)

library(dplyr)
# Display only the top 2 reasons for stop
top_reasons <- data %>%
  count(reason_for_stop) %>%
  top_n(2, n) %>%
  pull(reason_for_stop)

filtered_data <- data %>%
  filter(reason_for_stop %in% top_reasons)

library(ggplot2)

p <- ggplot(filtered_data, aes(x = subject_race, fill = reason_for_stop)) +
  geom_bar(position = "dodge") +
  labs(title = "Top Reasons for Stop by Race", x = "Race", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(p)


####222222######

install.packages("psych")

age <- data |> group_by(subject_age) |> summarize(counts = n())
data <- data |> filter(subject_age != "NA")
data <- data |> filter(subject_age > 17) #####legal driving age in california
data <- data |> filter(outcome != "NA")

##1 Descriptive Stats

library(dplyr)
library(ggplot2)
library(psych)

overall_stats <- t(psych::describe(data$subject_age))

group_stats <- data |> group_by(subject_race) |>
  summarise(
    mean_age <- mean(subject_age, na.rm = TRUE),
    sd_age <- sd(subject_age, na.rm = TRUE),
    min_age <- min(subject_age, na.rm = TRUE),
    max_age <- max(subject_age, na.rm = TRUE),
    N = n()
  )

print(knitr::kable(group_stats, format = "pipe"))

png("scatter_plot_california.png" ,width = 800, height = 600)
ggplot(data, aes(x = subject_age, y = outcome)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  labs(title = "Age vs Outcome", x = "Age", y = "Outcome")
dev.off()

outcome <- data |> group_by(outcome) |> summarize(counts = n())

data_outcome <- data[, c("outcome", "subject_race")] |> filter(outcome != 'NA')

data$subject_race <- as.character(data$subject_race)
data<- data |> filter(subject_race != 'other', subject_race != 'unknown')

png("scatter_plot_california_data_outcome.png", width = 800, height = 600)
ggplot(data_outcome, aes(x = subject_race, y = outcome)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  labs(title = "Race vs Outcome", x = "Race", y = "Outcome")
dev.off()

png("jitter_plot_california.png", width = 800, height = 600)
ggplot(data, aes(x = subject_race, y = search_conducted)) +
  geom_jitter(width = 0.3, alpha = 0.5) +
  labs(title = "Search Conducted by Race", x = "Race", y = "Search Conducted")
dev.off()

png("boxplot_california.png", width = 800, height = 600)
boxplot(subject_age ~ subject_race, data = data,
        main = "Age Distribution by Race",
        xlab = "Race", ylab = "Age")
dev.off()

data_age <- data[, c("subject_age", "subject_race")]
data_age$subject_race <- as.character(data_age$subject_race)
data_age <- data_age |> filter(subject_race != 'other', subject_race != 'unknown')
table(data_age$subject_race)

sample_means <- replicate(1000, mean(sample(data$subject_age[!is.na(data$subject_age)], 30, replace = TRUE)))
hist(sample_means, breaks = 30, main = "Distribution of Sample Means",
     xlab = "Sample Mean Age")

ci_mean_age <- t.test(data$subject_age)$conf.int
ci_mean_age

ci_prop_search <- prop.test(sum(data$search_conducted == TRUE), nrow(data), correct = FALSE)$conf.int
ci_prop_search

margin_of_error <- 0.05
z_score <- qnorm(0.975)
p <- mean(data$search_conducted == TRUE, na.rm = TRUE)
n <- (z_score^2 * p * (1-p)) / margin_of_error^2

print(paste("Minimum Sample Size needed: ", ceiling(n)))

#########Hypotheses Testing##########

library(dplyr)

t_test_age <- t.test(data$subject_age, mu = 35, alternative = "two.sided")
t_test_age

## Null Hypothesis (H₀): The mean age of individuals stopped is equal to 35 years.
## Alternative Hypothesis (H₁): The mean age of individuals stopped is not equal to 35 years.

# Interpretation:
cat("The one-sample t-test compares the sample mean age to a hypothesized population mean of 35 years. 
The null hypothesis assumes that the true mean age is equal to 35 years. 
If the p-value is less than our significance level (usually 0.05), we reject the null hypothesis and 
conclude that the mean age is significantly different from 35 years.\n")

if(t_test_age$p.value < 0.05){
  cat("Since, the p-value is less than 0.05, we reject the null hypothesis.
      There is evidence to suggest that the mean age of individuals stopped is significantly different from 35 years of age.\n")
}else{
  cat("Since, the p-value is greater than or equal to 35 years, we fail to reject the null hypothesis.
      There is no significant evidence to suggest that mean age differs from 35 years.\n")
}

"Hypotheses
Null Hypothesis (H₀): There is no association between race (Black vs. White) 
and the likelihood of being searched during a traffic stop. 
In other words, Black drivers are not more likely to be searched than White drivers.

Alternative Hypothesis (H₁): There is an association between race (Black vs. White) 
and the likelihood of being searched during a traffic stop. 
In other words, Black drivers are more likely to be searched than White drivers."

#Creating contingency table to know black race and white race frequencies.
race <- data |> group_by(subject_race) |> summarize(counts = n())
table(data$subject_race, data$search_conducted)

observed <- matrix(c(22006,119963,11151,338332), nrow = 2, byrow = TRUE,
                   dimnames = list(Race = c("Black", "White"), Search_Conducted = c("Yes", "No")))
observed

chi_test <- chisq.test(observed)
chi_test

if(chi_test$p.value < 0.05){
  cat("Since, the p-value is less than 0.05, we reject the null hypothesis.
      There is evidence to suggest that Black drivers are more likely to be searched than White drivers.\n")
}else{
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis.
      There is no significant evidence to suggest that Black drivers are more likely to be searched than White drivers.\n")
}


##Regression Module 4
rm(list = ls())
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE)
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
options(scipen = 100)
cat("\014")

library(tidyverse)
library(corrplot)
library(car)
library(caret)
library(scales)

data <- read.csv("/Users/jp/Desktop/ALY6010/Mod4_6010/ca_san_francisco_2020_04_01.csv")

traffic_data <- data %>%
  mutate(
    search_conducted = as.numeric(search_conducted),
    arrest_made = as.numeric(arrest_made),
    contraband_found = as.numeric(contraband_found),
    warning_issued = as.numeric(warning_issued),
    citation_issued = as.numeric(citation_issued),
    hour = as.numeric(substr(time, 1, 2)),
    subject_race = as.factor(subject_race),
    subject_sex = as.factor(subject_sex),
  ) %>%
  filter(!is.na(subject_age),
         subject_age >= 16,
         subject_age <= 100)

numeric_vars <- traffic_data %>%
  select(subject_age, search_conducted, arrest_made, 
         contraband_found, warning_issued, citation_issued)

corr_matrix <- cor(numeric_vars)
print(corr_matrix)

# 1. Correlation Heatmap
png('traffic_correlation.png', width = 800, height = 800)
corrplot(corr_matrix, 
         method = "color", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         title = "Correlation Matrix of Traffic Stop Variables",
         bg = "white",
         cl.ratio = 0.2,
         mar = c(0,0,2,0))
dev.off()

# MODEL SET 1: Core Analysis Model

#1. Contraband Discovery Model
contraband_model <- glm(contraband_found ~ subject_age + subject_race + subject_sex,
                        data = subset(traffic_data, search_conducted == 1),
                        family = binomial)

#Visualisations
install.packages("ggplot2")

library(ggplot2)
library(dplyr)

#1. Contraband Discovery Rate
p1 <- traffic_data %>%
  filter(!subject_race %in% c("other", "unknown")) %>%
  filter(search_conducted == 1) %>%
  group_by(subject_race) %>%
  summarise(
    contraband_rate = mean(contraband_found),
    searches = n()
  ) %>%
  ggplot(aes(x = reorder(subject_race, -contraband_rate), y = contraband_rate)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_text(aes(label = scales::percent(contraband_rate, accuracy = 0.1)), 
            vjust = -0.5) +
  labs(title = "Contraband Discovery Rates\n(When Searched)",
       x = "Race",
       y = "Contraband Discovery Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)

print(p1)

cat("\n 1. Contraband Discovery Model\n")
print(summary(contraband_model))