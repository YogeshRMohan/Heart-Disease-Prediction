# Heart-Disease-Prediction

# Objective

The objective of this study was to identify the common factors or characteristics that contribute to Coronary Heart Disease by following its development over a long period of time in a large group of participants who had not yet developed overt symptoms of CHD or suffered a heart attack or stroke.
As heart-related diseases continue to haunt the human race our team decided to analyze the data set from the study to better understand the incidence and prevalence of Coronary Heart Disease and its risk factors.

# Data Dictionary

The dataset studied under this project didn’t have any outliers or null value hence it did not require pre processing.
We have in total analysed 3658 subjects belonging to an age bracket of 32-70 years. Four educational levels; from no high school level to a college degree have been considered. Subjects smoking habits, history of stroke and hyper tension and glucose, cholesterol and BMI level are also included in the data set. And finally, whether the person has developed a heart disease between 1956 and 1966 or not is also noted.

# Variable conversion
Converted Categorical variables into factors

# Model Fitting - Fit the logistic regression in model r to the entire set of factors
Variable Selection – Discard variables with p values higher than 0.05. This process is done iteratively and the factors Male, Age, CigsPerDay, PrevalentStroke, SystolicBP, Glucose are finalized.

# Model Development 
Model developed with selected factors and model parameters are  evaluated. The first model had accuracy, Sensitivity and Specificity in higher 60s.

# Model iteration
Random Forest Model: In an attempt to increase the number of rightly predicted outcomes and there by increase the accuracy to 87%, second model was developed with A THRESHOLD VALUE OF 0.4. This resulted in a very high specificity of 96%, with a trade off in sensitivity value.

