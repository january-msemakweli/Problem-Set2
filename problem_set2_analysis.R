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
# iv. Evidence Relevant to Null Hypotheses
# ===============================================================================
cat("iv. Evidence Relevant to Null Hypotheses\n")
cat("=========================================\n")

# Hypothesis 1: Overall Treatment Effect
cat("Hypothesis 1: Overall Treatment Effect\n")
cat("--------------------------------------\n")
print(addmargins(CT))
chi1 <- chisq.test(CT)
cat("Chi-square =", round(chi1$statistic, 3), ", p-value =", round(chi1$p.value, 4), "\n\n")

# Hypothesis 2: Treatment Effect Same for Both Sexes
cat("Hypothesis 2: Treatment Effect Same for Both Sexes\n")
cat("--------------------------------------------------\n")

# Create 3-way table: Treatment x Sex x Status
# This tests if treatment effect differs by sex
three_way_table <- table(
  Treatment = c(rep("Placebo", nrow(nepal_plac)), rep("Vit A", nrow(nepal_vit))),
  Sex = c(nepal_plac$sex, nepal_vit$sex),
  Status = c(nepal_plac$status, nepal_vit$status)
)

# Show the stratified tables for reference
cat("Males:\n")
CT_males <- table(
  c(rep("Placebo", sum(CT_plac["Male", ])), rep("Vit A", sum(CT_vit["Male", ]))),
  c(rep(c("Alive", "Died"), c(CT_plac["Male", "Alive"], CT_plac["Male", "Died"])),
    rep(c("Alive", "Died"), c(CT_vit["Male", "Alive"], CT_vit["Male", "Died"])))
)
colnames(CT_males) <- c("Alive", "Died")
print(addmargins(CT_males))

cat("\nFemales:\n")
CT_females <- table(
  c(rep("Placebo", sum(CT_plac["Female", ])), rep("Vit A", sum(CT_vit["Female", ]))),
  c(rep(c("Alive", "Died"), c(CT_plac["Female", "Alive"], CT_plac["Female", "Died"])),
    rep(c("Alive", "Died"), c(CT_vit["Female", "Alive"], CT_vit["Female", "Died"])))
)
colnames(CT_females) <- c("Alive", "Died")
print(addmargins(CT_females))

# Test for interaction: Does treatment effect differ by sex?
cat("\nTest for interaction (treatment effect differs by sex):\n")

# Create a combined 2x2x2 table: Treatment x Sex x Outcome
# We can use Breslow-Day test or chi-square test for homogeneity

# Calculate treatment effects in each stratum
risk_diff_males <- (CT_males["Placebo", "Died"]/sum(CT_males["Placebo", ])) - 
                   (CT_males["Vit A", "Died"]/sum(CT_males["Vit A", ]))
risk_diff_females <- (CT_females["Placebo", "Died"]/sum(CT_females["Placebo", ])) - 
                     (CT_females["Vit A", "Died"]/sum(CT_females["Vit A", ]))

cat("Risk difference in males:", round(risk_diff_males, 4), "\n")
cat("Risk difference in females:", round(risk_diff_females, 4), "\n")

# Simple chi-square test for homogeneity using Breslow-Day approximation
# Test if the odds ratios are the same in both strata

# Calculate odds ratios
or_males <- (CT_males["Placebo", "Died"] * CT_males["Vit A", "Alive"]) / 
            (CT_males["Placebo", "Alive"] * CT_males["Vit A", "Died"])
or_females <- (CT_females["Placebo", "Died"] * CT_females["Vit A", "Alive"]) / 
              (CT_females["Placebo", "Alive"] * CT_females["Vit A", "Died"])

cat("Odds ratio in males:", round(or_males, 3), "\n")
cat("Odds ratio in females:", round(or_females, 3), "\n")

# Mantel-Haenszel test for homogeneity (simplified)
# Using the mantelhaen.test function
mh_test <- mantelhaen.test(array(c(
  CT_males["Placebo", "Died"], CT_males["Placebo", "Alive"],
  CT_males["Vit A", "Died"], CT_males["Vit A", "Alive"],
  CT_females["Placebo", "Died"], CT_females["Placebo", "Alive"],
  CT_females["Vit A", "Died"], CT_females["Vit A", "Alive"]
), dim = c(2, 2, 2)))

cat("Mantel-Haenszel test for homogeneity:\n")
cat("Chi-square =", round(mh_test$statistic, 3), ", p-value =", round(mh_test$p.value, 4), "\n\n")

interaction_p <- mh_test$p.value

# Summary Table
cat("Summary of Evidence\n")
cat("===================\n")
summary_table <- data.frame(
  Hypothesis = c("H1: No treatment effect", "H2: Same effect in both sexes"),
  Test_Statistic = c(paste("Chi-sq =", round(chi1$statistic, 3)), 
                    paste("Chi-sq =", round(mh_test$statistic, 3))),
  p_value = c(round(chi1$p.value, 4), round(interaction_p, 4)),
  Conclusion = c(
    ifelse(chi1$p.value < 0.05, "Reject H0", "Fail to reject"),
    ifelse(interaction_p < 0.05, "Reject H0", "Fail to reject")
  )
)
print(summary_table)

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
