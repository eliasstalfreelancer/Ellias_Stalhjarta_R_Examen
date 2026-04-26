library(tidyverse)
library(ggplot2)

# -------Dataförståelse och Datastädning -------------------------------------

loaded_data <- read_csv("insurance_costs.csv")

#EDA 
print(summary(loaded_data))

na_summary <- colSums(is.na(loaded_data)) %>% 
  sort(decreasing = TRUE) %>% 
  .[. > 0]

cat("\n Missing vaule for each collum\n")
if(length(na_summary) > 0) {
  print(na_summary) # found missing values on  BMI,exercise_level, annual_checkups
} else {print("No missing vaules")} 

#checking for data abnormality
unique_values <- list()

for (col in names(loaded_data)) {
  if (col != "customer_id"&&is.character(loaded_data[[col]])) {
    unique_values[[col]] <- unique(loaded_data[[col]])
  }
}

print(unique_values) 
#abnormality  at: 
#$region(North, South should be north,south)
#$smoker(Yes should be yes)
#$plan_type(Premium,Standard should be standard,premium)

# cleaning data for missing data
# for BMI because i don't can come up with a reason for what why its missingvaules 
# im going for median to keep the relations between data.
# between data
loaded_data$bmi[is.na(loaded_data$bmi)] <- median(loaded_data$bmi, na.rm = TRUE) 

#same goes for
loaded_data$annual_checkups[is.na(loaded_data$annual_checkups)] <- median(loaded_data$annual_checkups, na.rm = TRUE)

#for exercise_level I see that there no "none" options in the selection for no exercise 
#so i give na the value of none for that reason.
loaded_data$exercise_level[is.na(loaded_data$exercise_level)] <- "none"


# fixing spell error
#changing all the rows in specif to lowercase.
columns_with_spel_error <- c("region", "smoker", "plan_type")

for (col in columns_with_spel_error) {
  loaded_data[[col]] <- tolower(loaded_data[[col]])
}


# sanity check:
na_summary <- colSums(is.na(loaded_data)) %>% 
  sort(decreasing = TRUE) %>% 
  .[. > 0]

cat("\n Saknade värden per kolumn\n")
if(length(na_summary) > 0) {
  print(na_summary) # found missing values on  BMI,exercise_level, annual_checkups
} else {print("Inga saknade värden")} 

#checking for data abnormality
unique_values <- list()

for (col in names(loaded_data)) {
  if (col != "customer_id"&&is.character(loaded_data[[col]])) {
    unique_values[[col]] <- unique(loaded_data[[col]])
  }
}

print(unique_values) 
#everything check out!







#----------Dataförberedelse-------------
print("----------Dataförberedelse-------------"  )
#For added features I am thinking of using bmi category easier analysts and 

loaded_data$bmi_category <- cut(loaded_data$bmi, 
                         breaks = c(0, 18.5, 25, 30, 35,40,Inf),
                         labels = c("underweight", "normal", "overweight", "c1 obesity","c2 obesity","c3 obesity")  
) #bmi category definition taken from https://www.cdc.gov/bmi/adult-calculator/bmi-categories.html

# risk score because it easier determine someones health 
loaded_data$bmi_score <- case_when(
  loaded_data$bmi < 25 ~ 0,
  loaded_data$bmi >= 25 & loaded_data$bmi < 30 ~ 1,
  loaded_data$bmi >= 30 & loaded_data$bmi < 35 ~ 2,
  loaded_data$bmi >= 35 & loaded_data$bmi < 40 ~ 3,
  loaded_data$bmi >= 40 ~ 4
)
loaded_data$exercise_score <- case_when(
  loaded_data$exercise_level == "high" ~ 0,
  loaded_data$exercise_level == "medium" ~ 1,
  loaded_data$exercise_level == "low" ~ 2,
  loaded_data$exercise_level == "none" ~ 3
)

loaded_data$risk_score <- 
  (loaded_data$smoker == "yes") * 3 +
  (loaded_data$chronic_condition == "yes") * 3 +
  loaded_data$bmi_score +
  loaded_data$exercise_score

# quick check of new data
print("BMI Score")
print(summary(loaded_data$bmi_score))
#
print("risk_score")
print(summary(loaded_data$risk_score))








#--------- Beskrivande analys---------
#risk_score boxplot

ggplot(loaded_data, aes(x = as.factor(risk_score), y = charges)) +
  geom_boxplot(fill = "red") +
  labs(
    title = "Charges by Risk Score",
    x = "Risk Score",
    y = "Charges")+
  theme_minimal()
"The figure shows a clear positive relationship between risk score and insurance charges.
As the risk score increases, the median cost consistently rises, 
indicating that individuals with higher health risks tend to incur higher insurance expenses."

# bmi_category box plot
ggplot(loaded_data, aes(x = as.factor(bmi_category), y = charges)) +
  geom_boxplot(fill = "purple") +
  labs(
    title = "Charges by BMI Category",
    x = "BMI Category",
    y = "Charges"
  ) +
  theme_minimal()
"A general upward trend can be observed where higher BMI categories are associated with increased insurance charges.
Individuals in the obesity categories, particularly class 3 obesity, 
tend to have noticeably higher median costs compared to those in lower BMI groups."

#Distribution av kostnader
ggplot(loaded_data, aes(x = charges)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(
    title = "Distribution of Insurance Charges",
    x = "Charges",
    y = "Frequency"
  ) +
  theme_minimal()
"The distribution of insurance charges is right-skewed, 
meaning that most individuals have relatively low costs, 
while a smaller number of individuals have very high expenses."

#Smoker effect on charges
ggplot(loaded_data, aes(x = smoker, y = charges)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Charges by Smoking Status",
    x = "Smoker",
    y = "Charges"
  ) +
  theme_minimal()
"The figure shows a clear and substantial difference in insurance charges between smokers and non-smokers. 
Smokers have significantly higher median costs, indicating that smoking is a major factor influencing insurance expenses."








#-------Regressionsanalys-------

model1 <- lm(charges ~ age + bmi + smoker + exercise_level + chronic_condition + prior_claims, data = loaded_data)
"The selected predictors include age, BMI, smoking status, exercise level, chronic condition, 
and prior claims, as these variables represent key demographic, lifestyle, 
and health-related factors that are expected to influence insurance costs."


model2 <- lm(charges ~ age + risk_score, data = loaded_data)
"A second model was estimated using a composite risk score to evaluate 
whether a simplified representation of health risk could explain variation in insurance charges."

summary(model1)
"Output: Residual standard error: 2426 on 1091 degrees of freedom
Multiple R-squared:  0.7208,	Adjusted R-squared:  0.7188 
F-statistic: 352.1 on 8 and 1091 DF,  p-value: < 2.2e-16"

"The regression model explains approximately 72% of the variation in insurance charges (R-squared = 0.72), indicating a strong model fit."

summary(model2)
"Output: Residual standard error: 2876 on 1097 degrees of freedom
Multiple R-squared:  0.6055,	Adjusted R-squared:  0.6048 
F-statistic: 841.9 on 2 and 1097 DF,  p-value: < 2.2e-16"

"The simplified model explains approximately 60% of the variation in insurance charges (R-squared = 0.61), which is lower than the full model."










# ---------- Model Result and Comparison ------
"The regression models show a clear and substantial effect of lifestyle and health-related factors on insurance charges. 
The results indicate that smoking is the single most important variable, with a strong positive effect on costs. 
Chronic conditions, BMI, and prior claims also have statistically significant effects, suggesting that both current health status and past history play an important role.
Age also has a positive effect, although smaller compared to the other variables.

The results appear reasonable and realistic, as they align with known patterns in healthcare and insurance. 
Individuals with poorer health or risk-related behaviors tend to generate higher costs. 
The strong effect of smoking further supports the credibility of the model.
However, the models assume linear relationships and may not capture more complex interactions between variables.

Two models were compared: a detailed model with individual variables and a simplified model based on a composite risk score. 
The detailed model explains a larger proportion of the variation in costs (R-sqr ≈ 0.72 compared to 0.61), indicating a more accurate representation of the data. 
At the same time, the simpler model shows that a combined risk variable can still capture the overall pattern, despite using fewer predictors.

The full model is therefore more suitable for analysis and prediction, while the risk score model may be useful in practical situations where simplicity and interpretability are more important than maximum accuracy."









#------Tolkning och slutsatser--------
"The analysis shows that insurance charges are strongly influenced by lifestyle and health-related factors. 
Among all variables, smoking has the clearest and strongest relationship with costs, followed by chronic conditions, BMI, and prior claims. 
Age also has a positive effect, but to a lesser extent. These results indicate that both current health status and past behavior are important in determining insurance costs.

The overall analysis demonstrates that individuals with higher health risks tend to incur higher insurance expenses. 
This is supported both by the descriptive analysis and the regression models, where clear differences between groups were observed. 
The strong relationship between the constructed risk score and charges further confirms that multiple risk factors combined can explain a large part of the variation in costs.

However, the model does not capture all aspects of reality. 
It assumes linear relationships between variables and may not account for more complex interactions between factors. 
Additionally, there may be other important variables not included in the dataset, such as genetic factors or more detailed medical history.

To improve the analysis, future work could include testing interaction effects between variables, using non-linear models, or optimizing the construction of the risk score based on data rather than assumptions. 
Transformations of the target variable could also be considered to better handle the skewed distribution of costs.

In conclusion, the models provide a strong and meaningful explanation of insurance costs, but there is still room for improvement in capturing the full complexity of the underlying relationships."









#-------Självreflektion--------
"I think I did a good job structuring the analysis and working systematically with the data. I was able to clean the dataset, create relevant new variables such as the risk score, and use both visualizations and regression models to identify important patterns. 
I also compared different model approaches, which helped me better understand how different variables affect insurance costs.

The most challenging part was deciding how to handle missing values and how to construct meaningful new variables, especially the risk score. 
It was also somewhat difficult to interpret the regression output in a clear and correct way.

I believe my work corresponds to a VG level. I have fulfilled the requirements by performing a complete analysis, creating and motivating new variables, building and comparing multiple regression models, and providing a clear interpretation of the results. 
I have also discussed the strengths and limitations of the models and reflected on possible improvements, which meets the higher-level criteria for the assignment."
