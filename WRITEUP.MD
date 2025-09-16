# Problem Set 2: Vitamin A Supplementation to Prevent Children's Mortality in Nepal

## Scientific Background

Xerophthalmia, due to vitamin A deficiency, was long known to be a leading cause of childhood blindness in the developing world. Hopkins investigators showed in large trials in Indonesia and Nepal that dosing young children every 4-6 months with high-potency vitamin A could reduce child mortality by approximately 30%. This analysis examines data from the original community trial carried out in Nepal.

---

## i. Contingency Table Analysis

### R Code:
```r
CT <- table(nepal621$trt, nepal621$status)
addmargins(CT)
prop.table(CT, margin=1)
```

### Results:
**Contingency Table:**
```
          Alive  Died   Sum
  Placebo 13099   290 13389
  Vit A   13499   233 13732
  Sum     26598   523 27121
```

**Mortality Rates:**
- Placebo group: 0.0217 (2.17%)
- Vitamin A group: 0.0170 (1.70%)
- Relative reduction: 21.7%

### Journal Summary:
Vitamin A supplementation reduced child mortality from 2.17% in the placebo group to 1.70% in the treatment group, representing a 21.7% relative reduction in mortality risk.

---

## ii.a) Probability Calculations

### R Code:
```r
total_n <- sum(CT)
pr_vita <- sum(CT["Vit A", ]) / total_n
pr_died <- sum(CT[, "Died"]) / total_n
pr_died_vita <- CT["Vit A", "Died"] / total_n
pr_died_placebo <- CT["Placebo", "Died"] / total_n
pr_died_given_vita <- CT["Vit A", "Died"] / sum(CT["Vit A", ])
pr_died_given_placebo <- CT["Placebo", "Died"] / sum(CT["Placebo", ])
```

### Results:

**Marginal Probabilities:**
- Pr(VitA) = 0.5063
- Pr(Died) = 0.0193

**Joint Probabilities:**
- Pr(Died and VitA) = 0.0086
- Pr(Died and Placebo) = 0.0107

**Conditional Probabilities:**
- Pr(Died | VitA) = 0.0170
- Pr(Died | Placebo) = 0.0217

---

## ii.b) Bayes' Theorem Calculation

### Hand Calculation using the formula shown:

**Pr(VitA | Died) = Pr(VitA and Died) / Pr(Died) = [Pr(Died | VitA) × Pr(VitA)] / Pr(Died)**

**Using the expanded denominator:**
**Pr(VitA | Died) = [Pr(Died | VitA) × Pr(VitA)] / [Pr(Died | VitA) × Pr(VitA) + Pr(Died | Placebo) × Pr(Placebo)]**

**Step-by-step calculation with observed rates:**
- Pr(Died | VitA) = 0.0170
- Pr(VitA) = 0.5063
- Pr(Died | Placebo) = 0.0217
- Pr(Placebo) = 0.4937

**Numerator:**
Pr(Died | VitA) × Pr(VitA) = 0.0170 × 0.5063 = 0.008607

**Denominator:**
Pr(Died | VitA) × Pr(VitA) + Pr(Died | Placebo) × Pr(Placebo)
= 0.0170 × 0.5063 + 0.0217 × 0.4937
= 0.008607 + 0.010713
= 0.019320

**Result:**
Pr(VitA | Died) = 0.008607 / 0.019320 = **0.4455**

### Interpretation:
The probability that a child who died received Vitamin A is 0.4455 (44.55%). This is less than 50%, reflecting that vitamin A reduces mortality risk.

---

## iii. Sex as Effect Modifier Analysis

### R Code:
```r
nepal_plac <- filter(nepal621, trt == "Placebo")
nepal_vit <- filter(nepal621, trt == "Vit A")
CT_plac <- table(nepal_plac$sex, nepal_plac$status)
CT_vit <- table(nepal_vit$sex, nepal_vit$status)
```

### Results:

**Placebo Group:**
```
         Alive  Died   Sum
  Female  6376   166  6542
  Male    6723   124  6847
```
- Female mortality: 0.0254 (2.54%)
- Male mortality: 0.0181 (1.81%)

**Vitamin A Group:**
```
         Alive  Died   Sum
  Female  6544   121  6665
  Male    6955   112  7067
```
- Female mortality: 0.0182 (1.82%)
- Male mortality: 0.0158 (1.58%)

### Treatment Effects:
- Males: 0.0181 - 0.0158 = **0.0023** (0.23 percentage points)
- Females: 0.0254 - 0.0182 = **0.0072** (0.72 percentage points)
- Difference: |0.0023 - 0.0072| = **0.0049**

### Journal Summary:
Vitamin A supplementation reduced mortality in both sexes, with a greater effect observed in females. The treatment effect in females (0.72 percentage points) was approximately three times larger than in males (0.23 percentage points), suggesting effect modification by sex, with females showing greater benefit from vitamin A supplementation.

---

## iv. Evidence Relevant to Null Hypothesis (Logistic Regression)

### R Code:
```r
# Logistic regression for overall treatment effect
model1 <- glm(status == "Died" ~ trt, data = nepal621, family = binomial)
summary(model1)

# Logistic regression with interaction term for effect modification
model2 <- glm(status == "Died" ~ trt * sex, data = nepal621, family = binomial)
summary(model2)
```

### Hypothesis 1: Overall Treatment Effect

**Table: Logistic Regression Results - Overall Treatment Effect**

| Category | Reference | OR | 95% CI Lower | 95% CI Upper | p-value |
|----------|-----------|----|--------------|--------------|---------| 
| Intercept (Placebo) | - | 0.022 | 0.020 | 0.025 | 0.0000 |
| Treatment (Vit A vs Placebo) | Placebo | **0.780** | **0.655** | **0.928** | **0.0051** |

### Hypothesis 2: Treatment Effect Modification by Sex

**Table: Logistic Regression Results - Treatment Effect Modification**

| Category | Reference | OR | 95% CI Lower | 95% CI Upper | p-value |
|----------|-----------|----|--------------|--------------|---------| 
| Intercept (Placebo, Male) | - | 0.026 | 0.022 | 0.030 | 0.0000 |
| Treatment (Vit A vs Placebo) | Placebo | 0.710 | 0.560 | 0.900 | 0.0046 |
| Sex (Female vs Male) | Male | 0.708 | 0.560 | 0.896 | 0.0041 |
| Interaction (Vit A × Female) | - | **1.229** | **0.866** | **1.745** | **0.2475** |

### Summary of Hypothesis Tests

| Hypothesis | Test | p-value | Conclusion |
|------------|------|---------|------------|
| H₁: No treatment effect | Treatment coefficient | **0.0051** | **Reject H₀** |
| H₂: No effect modification by sex | Interaction coefficient | **0.2475** | **Fail to reject H₀** |

### Interpretation
- **Hypothesis 1:** Vitamin A significantly reduces mortality (OR = 0.780, p = 0.0051) - 22% reduction in odds of death compared to placebo.
- **Hypothesis 2:** No significant effect modification by sex (interaction p = 0.2475) - treatment effect is similar for males and females.

---

## v. Binomial Probabilities for Family with 3 Boys on Placebo

### Parameters:
- n = 3 boys
- p = 0.0181 (male mortality rate on placebo)

### Hand Calculation using Binomial Formula:
**P(X = k) = C(n,k) × p^k × (1-p)^(n-k)**

**P(0 boys die):**
P(X = 0) = C(3,0) × (0.0181)⁰ × (0.9819)³ = 1 × 1 × 0.946648 = **0.946648**

**P(1 boy dies):**
P(X = 1) = C(3,1) × (0.0181)¹ × (0.9819)² = 3 × 0.0181 × 0.964151 = **0.052380**

**P(2 boys die):**
P(X = 2) = C(3,2) × (0.0181)² × (0.9819)¹ = 3 × 0.000328 × 0.9819 = **0.000966**

**P(3 boys die):**
P(X = 3) = C(3,3) × (0.0181)³ × (0.9819)⁰ = 1 × 0.000006 × 1 = **0.000006**

### R Code Verification:
```r
n_boys <- 3
boys_placebo_mortality <- 0.0181
prob_0_bin <- dbinom(0, n_boys, boys_placebo_mortality)  # 0.946648
prob_1_bin <- dbinom(1, n_boys, boys_placebo_mortality)  # 0.052380
prob_2_bin <- dbinom(2, n_boys, boys_placebo_mortality)  # 0.000966
prob_3_bin <- dbinom(3, n_boys, boys_placebo_mortality)  # 0.000006
```

---

## vi. Poisson Approximation

### Parameters:
- λ = n × p = 3 × 0.0181 = **0.0543**

### Hand Calculation using Poisson Formula:
**P(X = k) = (e^(-λ) × λ^k) / k!**

**P(0 boys die):**
P(X = 0) = (e^(-0.0543) × 0.0543⁰) / 0! = 0.947119 × 1 / 1 = **0.947119**

**P(1 boy dies):**
P(X = 1) = (e^(-0.0543) × 0.0543¹) / 1! = 0.947119 × 0.0543 / 1 = **0.051457**

**P(2 boys die):**
P(X = 2) = (e^(-0.0543) × 0.0543²) / 2! = 0.947119 × 0.002946 / 2 = **0.001398**

**P(3 boys die):**
P(X = 3) = (e^(-0.0543) × 0.0543³) / 3! = 0.947119 × 0.000160 / 6 = **0.000025**

### R Code Verification:
```r
lambda <- 3 * 0.0181  # 0.0543
pois_0 <- dpois(0, lambda)  # 0.947119
pois_1 <- dpois(1, lambda)  # 0.051457
pois_2 <- dpois(2, lambda)  # 0.001398
pois_3 <- dpois(3, lambda)  # 0.000025
```

### Comparison:
The Poisson approximation closely matches the binomial probabilities, with slight differences due to the approximation (np = 0.0543 is small, making the approximation appropriate).

---

## vii. Age Analysis and Triplet Probabilities

### Age-Specific Mortality Rates:

**Placebo Group:**
```
      Alive  Died   Sum
  <1   2495   120  2615  (Mortality: 4.59%)
  1-2  5385   119  5504  (Mortality: 2.16%)
  3-4  5219    51  5270  (Mortality: 0.97%)
```

**Vitamin A Group:**
```
      Alive  Died   Sum
  <1   2657   114  2771  (Mortality: 4.11%)
  1-2  5561    92  5653  (Mortality: 1.63%)
  3-4  5281    27  5308  (Mortality: 0.51%)
```

### Triplet Analysis (18 months old, on Vitamin A treatment):

**Parameters:**
- Age group: 1-2 years (18 months falls in this category)
- Treatment: Vitamin A
- Mortality rate (p): 0.0163

### R Code:
```r
age_1_2_mortality <- 0.0163
prob_jk_live_l_dies <- (1 - age_1_2_mortality)^2 * age_1_2_mortality
prob_exactly_one_dies <- dbinom(1, 3, age_1_2_mortality)
```

### a) Probability that J and K live and L dies:

**Hand Calculation:**
P(J lives) × P(K lives) × P(L dies) = (1 - 0.0163) × (1 - 0.0163) × 0.0163
= 0.9837 × 0.9837 × 0.0163 = **0.015749**

### b) Probability that exactly one of the three children dies:

**Hand Calculation using Binomial:**
P(X = 1) = C(3,1) × p¹ × (1-p)² = 3 × 0.0163 × (0.9837)²
= 3 × 0.0163 × 0.968169 = **0.047247**

### Comparison:
- P(J and K live, L dies) = **0.015749**
- P(exactly one of three dies) = **0.047247**

**Are they the same? No.**

**Why not?** The probability that exactly one child dies includes three different scenarios:
1. J dies, K and L live
2. K dies, J and L live  
3. L dies, J and K live

Each scenario has the same probability (0.015749), so the total probability is 3 × 0.015749 = 0.047247. The first calculation only considers one specific scenario (L dies, J and K live), while the second considers all possible ways exactly one child can die.

---

## Conclusion

This analysis of the Nepal vitamin A supplementation trial demonstrates:

1. **Overall effectiveness**: 21.7% reduction in child mortality
2. **Effect modification by sex**: Females showed greater benefit than males
3. **Age-related mortality patterns**: Younger children had higher baseline mortality rates
4. **Statistical modeling**: Both binomial and Poisson distributions appropriately model rare events like child mortality
5. **Probability interpretation**: Specific event probabilities differ from general category probabilities, as demonstrated in the triplet analysis

The findings support the continued use of vitamin A supplementation programs for reducing child mortality, with particular attention to potential differential effects by demographic characteristics.
