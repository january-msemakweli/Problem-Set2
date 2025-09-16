# Biostatistics 140.621 - Problem Set 2 Analysis
# Vitamin A Supplementation to Prevent Children's Mortality in Nepal

library(tidyverse)

# Load the dataset
nepal621 <- read.csv("nepal621.csv")

cat("=== PROBLEM SET 2: VITAMIN A SUPPLEMENTATION ANALYSIS ===\n\n")

# ===============================================================================
# i. Create 2x2 contingency table of treatment vs status
# ===============================================================================
cat("i. Contingency Table of Treatment vs Status\n")
cat("============================================\n")
CT <- table(nepal621$trt, nepal621$status)
print("Raw contingency table:")
print(CT)
print("Contingency table with margins:")
print(addmargins(CT))
print("Proportions by treatment group:")
prop_table <- prop.table(CT, margin=1)
print(prop_table)

# Calculate mortality rates
mortality_placebo <- prop_table["Placebo", "Died"]
mortality_vita <- prop_table["Vit A", "Died"]
mortality_reduction <- (mortality_placebo - mortality_vita)/mortality_placebo * 100

cat("\nMortality Rates:\n")
cat("Placebo group:", round(mortality_placebo, 4), "\n")
cat("Vitamin A group:", round(mortality_vita, 4), "\n")
cat("Relative reduction:", round(mortality_reduction, 1), "%\n\n")

# ===============================================================================
# ii.a) Calculate probabilities from contingency table
# ===============================================================================
cat("ii.a) Probability Calculations\n")
cat("==============================\n")
total_n <- sum(CT)

# Marginal Probabilities
pr_vita <- sum(CT["Vit A", ]) / total_n
pr_died <- sum(CT[, "Died"]) / total_n

# Joint Probabilities
pr_died_vita <- CT["Vit A", "Died"] / total_n
pr_died_placebo <- CT["Placebo", "Died"] / total_n

# Conditional Probabilities
pr_died_given_vita <- CT["Vit A", "Died"] / sum(CT["Vit A", ])
pr_died_given_placebo <- CT["Placebo", "Died"] / sum(CT["Placebo", ])

cat("Marginal Probabilities:\n")
cat("Pr(VitA) =", round(pr_vita, 4), "\n")
cat("Pr(Died) =", round(pr_died, 4), "\n\n")

cat("Joint Probabilities:\n")
cat("Pr(Died and VitA) =", round(pr_died_vita, 4), "\n")
cat("Pr(Died and Placebo) =", round(pr_died_placebo, 4), "\n\n")

cat("Conditional Probabilities:\n")
cat("Pr(Died | VitA) =", round(pr_died_given_vita, 4), "\n")
cat("Pr(Died | Placebo) =", round(pr_died_given_placebo, 4), "\n\n")

# ===============================================================================
# ii.b) Bayes' Theorem calculation
# ===============================================================================
cat("ii.b) Bayes' Theorem Calculation\n")
cat("=================================\n")
pr_vita_given_died <- (pr_died_given_vita * pr_vita) / pr_died
cat("Pr(VitA | Died) =", round(pr_vita_given_died, 4), "\n\n")

# Store values for hand calculation display
cat("Values for hand calculation:\n")
cat("Pr(Died | VitA) =", round(pr_died_given_vita, 4), "\n")
cat("Pr(VitA) =", round(pr_vita, 4), "\n")
cat("Pr(Died) =", round(pr_died, 4), "\n\n")

# ===============================================================================
# iii. Sex as effect modifier analysis
# ===============================================================================
cat("iii. Sex as Effect Modifier Analysis\n")
cat("====================================\n")
nepal_plac <- filter(nepal621, trt == "Placebo")
nepal_vit <- filter(nepal621, trt == "Vit A")

# Contingency tables by sex and treatment
cat("Placebo group - Sex vs Status:\n")
CT_plac <- table(nepal_plac$sex, nepal_plac$status)
print(addmargins(CT_plac))
prop_plac <- prop.table(CT_plac, margin=1)
print("Proportions:")
print(prop_plac)

cat("\nVitamin A group - Sex vs Status:\n")
CT_vit <- table(nepal_vit$sex, nepal_vit$status)
print(addmargins(CT_vit))
prop_vit <- prop.table(CT_vit, margin=1)
print("Proportions:")
print(prop_vit)

# Calculate mortality rates by sex and treatment
mortality_male_placebo <- CT_plac["Male", "Died"] / sum(CT_plac["Male", ])
mortality_female_placebo <- CT_plac["Female", "Died"] / sum(CT_plac["Female", ])
mortality_male_vita <- CT_vit["Male", "Died"] / sum(CT_vit["Male", ])
mortality_female_vita <- CT_vit["Female", "Died"] / sum(CT_vit["Female", ])

# Calculate treatment effects by sex
effect_male <- mortality_male_placebo - mortality_male_vita
effect_female <- mortality_female_placebo - mortality_female_vita
effect_difference <- abs(effect_male - effect_female)

cat("\nMortality rates by sex and treatment:\n")
cat("Males - Placebo:", round(mortality_male_placebo, 4), ", Vitamin A:", round(mortality_male_vita, 4), "\n")
cat("Females - Placebo:", round(mortality_female_placebo, 4), ", Vitamin A:", round(mortality_female_vita, 4), "\n")
cat("Treatment effect in males:", round(effect_male, 4), "\n")
cat("Treatment effect in females:", round(effect_female, 4), "\n")
cat("Difference in treatment effects:", round(effect_difference, 4), "\n\n")

# ===============================================================================
# iv. Evidence Relevant to Null Hypotheses (Logistic Regression)
# ===============================================================================
cat("iv. Evidence Relevant to Null Hypotheses\n")
cat("=========================================\n")

# Hypothesis 1: Overall Treatment Effect
cat("Hypothesis 1: Overall Treatment Effect\n")
cat("--------------------------------------\n")
model1 <- glm(status == "Died" ~ trt, data = nepal621, family = binomial)
summary1 <- summary(model1)

# Extract results for table
coef1 <- summary1$coefficients
or1 <- exp(coef1[, "Estimate"])
ci_lower1 <- exp(coef1[, "Estimate"] - 1.96 * coef1[, "Std. Error"])
ci_upper1 <- exp(coef1[, "Estimate"] + 1.96 * coef1[, "Std. Error"])

table1 <- data.frame(
  Category = c("Intercept (Placebo)", "Treatment (Vit A vs Placebo)"),
  Reference = c("-", "Placebo"),
  OR = round(or1, 3),
  CI_95_Lower = round(ci_lower1, 3),
  CI_95_Upper = round(ci_upper1, 3),
  p_value = round(coef1[, "Pr(>|z|)"], 4)
)

cat("Logistic Regression Results - Hypothesis 1:\n")
print(table1)
cat("\n")

# Summary of Hypothesis Test
cat("Summary of Hypothesis Test\n")
cat("==========================\n")
h1_p <- coef1["trtVit A", "Pr(>|z|)"]

summary_table <- data.frame(
  Hypothesis = "H1: No treatment effect",
  Test = "Treatment coefficient",
  p_value = round(h1_p, 4),
  Conclusion = ifelse(h1_p < 0.05, "Reject H0", "Fail to reject")
)
print(summary_table)

cat("\n")

# Hypothesis 2: Treatment Effect Modification by Sex
cat("Hypothesis 2: Treatment Effect Modification by Sex\n")
cat("--------------------------------------------------\n")
model2 <- glm(status == "Died" ~ trt * sex, data = nepal621, family = binomial)
summary2 <- summary(model2)

# Extract results for table
coef2 <- summary2$coefficients
or2 <- exp(coef2[, "Estimate"])
ci_lower2 <- exp(coef2[, "Estimate"] - 1.96 * coef2[, "Std. Error"])
ci_upper2 <- exp(coef2[, "Estimate"] + 1.96 * coef2[, "Std. Error"])

table2 <- data.frame(
  Category = c("Intercept (Placebo, Male)", "Treatment (Vit A vs Placebo)", 
               "Sex (Female vs Male)", "Interaction (Vit A × Female)"),
  Reference = c("-", "Placebo", "Male", "-"),
  OR = round(or2, 3),
  CI_95_Lower = round(ci_lower2, 3),
  CI_95_Upper = round(ci_upper2, 3),
  p_value = round(coef2[, "Pr(>|z|)"], 4)
)

cat("Logistic Regression Results - Hypothesis 2 (with interaction):\n")
print(table2)
cat("\n")

# Summary of Hypothesis Test for interaction
cat("Summary of Hypothesis Test for Interaction\n")
cat("==========================================\n")
h2_p <- coef2["trtVit A:sexMale", "Pr(>|z|)"]

summary_table2 <- data.frame(
  Hypothesis = "H2: No treatment effect modification by sex",
  Test = "Interaction coefficient (trt × sex)",
  p_value = round(h2_p, 4),
  Conclusion = ifelse(h2_p < 0.05, "Reject H0", "Fail to reject")
)
print(summary_table2)

cat("\n")

# ===============================================================================
# v. Family with 3 boys on placebo - binomial probabilities
# ===============================================================================
cat("\nv. Binomial Probabilities for 3 Boys on Placebo\n")
cat("===============================================\n")
n_boys <- 3
boys_placebo_mortality <- mortality_male_placebo

cat("Parameters: n =", n_boys, ", p =", round(boys_placebo_mortality, 4), "\n")

# Calculate binomial probabilities for 0, 1, 2, 3 boys dying
prob_0_bin <- dbinom(0, n_boys, boys_placebo_mortality)
prob_1_bin <- dbinom(1, n_boys, boys_placebo_mortality)
prob_2_bin <- dbinom(2, n_boys, boys_placebo_mortality)
prob_3_bin <- dbinom(3, n_boys, boys_placebo_mortality)

cat("Binomial probabilities:\n")
cat("P(0 boys die) =", round(prob_0_bin, 6), "\n")
cat("P(1 boy dies) =", round(prob_1_bin, 6), "\n")
cat("P(2 boys die) =", round(prob_2_bin, 6), "\n")
cat("P(3 boys die) =", round(prob_3_bin, 6), "\n")
cat("Sum =", round(prob_0_bin + prob_1_bin + prob_2_bin + prob_3_bin, 6), "\n\n")

# ===============================================================================
# vi. Poisson approximation
# ===============================================================================
cat("vi. Poisson Approximation\n")
cat("=========================\n")
lambda <- n_boys * boys_placebo_mortality
cat("Lambda (μ) = n × p =", n_boys, "×", round(boys_placebo_mortality, 4), "=", round(lambda, 4), "\n")

pois_0 <- dpois(0, lambda)
pois_1 <- dpois(1, lambda)
pois_2 <- dpois(2, lambda)
pois_3 <- dpois(3, lambda)

cat("Poisson probabilities:\n")
cat("P(0 boys die) =", round(pois_0, 6), "\n")
cat("P(1 boy dies) =", round(pois_1, 6), "\n")
cat("P(2 boys die) =", round(pois_2, 6), "\n")
cat("P(3 boys die) =", round(pois_3, 6), "\n")
cat("Sum =", round(pois_0 + pois_1 + pois_2 + pois_3, 6), "\n\n")

# ===============================================================================
# vii. Age analysis and triplet probabilities
# ===============================================================================
cat("vii. Age Analysis and Triplet Probabilities\n")
cat("===========================================\n")

cat("Placebo group - Age vs Status:\n")
CT_age_plac <- table(nepal_plac$age, nepal_plac$status)
print(addmargins(CT_age_plac))
prop_age_plac <- prop.table(CT_age_plac, margin=1)
print("Proportions:")
print(prop_age_plac)

cat("\nVitamin A group - Age vs Status:\n")
CT_age_vit <- table(nepal_vit$age, nepal_vit$status)
print(addmargins(CT_age_vit))
prop_age_vit <- prop.table(CT_age_vit, margin=1)
print("Proportions:")
print(prop_age_vit)

# Get mortality rate for 1-2 year olds on treatment (18 months falls in 1-2 category)
age_1_2_mortality <- CT_age_vit["1-2", "Died"] / sum(CT_age_vit["1-2", ])

cat("\nTriplet Analysis (18 months old, on Vitamin A treatment):\n")
cat("Mortality rate for 1-2 year olds on Vitamin A:", round(age_1_2_mortality, 4), "\n")

# a) Probability that J and K live and L dies
prob_jk_live_l_dies <- (1 - age_1_2_mortality)^2 * age_1_2_mortality

# b) Probability that exactly one of three children dies
prob_exactly_one_dies <- dbinom(1, 3, age_1_2_mortality)

cat("a) P(J and K live, L dies) =", round(prob_jk_live_l_dies, 6), "\n")
cat("b) P(exactly one of three dies) =", round(prob_exactly_one_dies, 6), "\n")
cat("Are they the same?", ifelse(abs(prob_jk_live_l_dies - prob_exactly_one_dies) < 1e-10, "Yes", "No"), "\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
