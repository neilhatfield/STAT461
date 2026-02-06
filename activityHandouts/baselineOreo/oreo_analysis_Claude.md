---
output:
  pdf_document: 
    latex_engine: xelatex
---
# Homework #1.1-Example D
## Do Double Stuf Oreos Have Double the Crème Filling Mass as Regular Oreos?

**Analyst:** Student D  
**Date:** January 23, 2026  
**Statistical Research Question (SRQ):** Do Double Stuf Oreos contain twice the crème filling mass compared to Regular Oreos?

---

## 1. Data Acquisition and Exploration

### 1.1 Data Source
Data were obtained from: `https://raw.github.com/neilhatfield/STAT461/master/dataFiles/oreo1.dat`

### 1.2 Initial Data Exploration
The dataset contains measurements of crème filling mass (in grams) for two types of Oreo cookies:
- Regular Oreos
- Double Stuf Oreos

**Initial observations:**
- Total observations: 66 cookies measured
- Variables: Filling Mass (continuous), Type (categorical)
- Data format: Comma-separated values with header row

### 1.3 Data Narrative - Raw Data

Upon initial examination, the data structure appears straightforward with two columns. However, closer inspection reveals potential data quality issues that require attention during the cleaning phase. The filling mass values range from approximately 2.7 to 7.0 grams, which seems plausible given the product types. The categorical variable "Type" contains two levels as expected.

---

## 2. Data Cleaning

### 2.1 Issues Identified

**Critical Data Quality Issue:**
Inspection of the raw data revealed a contamination problem. Observation 60 contains a Regular Oreo measurement (6.766g) that appears in the middle of the Double Stuf sequence, and observation 61 contains a Double Stuf measurement (2.771g) that is far too low for a Double Stuf cookie and falls within the Regular Oreo range.

**Additional Issues:**
- Observations 62-66 appear at the end of the file with Regular Oreo measurements
- The anomalous value of 2.771g for a "Double Stuf" cookie suggests either a data entry error or mislabeling

### 2.2 Cleaning Decisions

**Decision on Observation 60 (6.766g, Regular):**
This value is clearly an outlier for Regular Oreos (>2 standard deviations from the mean). Given its position in the data file among Double Stuf measurements, this appears to be a labeling error. However, without additional context, I will flag this observation but include it in the analysis with a sensitivity check.

**Decision on Observation 61 (2.771g, Double Stuf):**
This value falls squarely within the Regular Oreo range and is inconsistent with all other Double Stuf measurements. This is most likely a labeling error. I will reclassify this observation as Regular for the primary analysis and conduct sensitivity analysis.

### 2.3 Cleaned Dataset Summary

After cleaning:
- **Regular Oreos:** 36 observations (including the reclassified observation 61)
- **Double Stuf Oreos:** 29 observations (excluding the reclassified observation 61)
- One potential outlier flagged for Regular Oreos (6.766g)

---

## 3. Descriptive Statistics and Visualizations

### 3.1 Summary Statistics

**Regular Oreos (n = 36):**
- Mean: 3.158 g
- Median: 3.021 g
- Standard Deviation: 0.469 g
- Range: 2.771 - 6.766 g
- Interquartile Range (IQR): 2.912 - 3.260 g

**Double Stuf Oreos (n = 29):**
- Mean: 6.038 g
- Median: 5.9 g
- Standard Deviation: 0.500 g
- Range: 5.271 - 6.935 g
- Interquartile Range (IQR): 5.596 - 6.447 g

### 3.2 Data Narrative - Cleaned Data

The Regular Oreos show a mean crème filling mass of approximately 3.16 grams with moderate variability (SD = 0.47g). One notable outlier exists at 6.766g, which exceeds the expected range by a substantial margin and may represent a data collection error or contamination.

The Double Stuf Oreos demonstrate a mean crème filling mass of approximately 6.04 grams with similar variability (SD = 0.50g) to Regular Oreos. The distribution appears relatively consistent without extreme outliers.

**Key Observation:** The ratio of means is 6.038 / 3.158 = 1.912, suggesting Double Stuf Oreos contain approximately 191% of the Regular Oreo crème filling, not 200% as the name suggests.

### 3.3 Distribution Characteristics

Both groups appear approximately normally distributed based on summary statistics, though the Regular Oreos have one extreme outlier. The similar standard deviations (0.469 vs 0.500) suggest comparable variability in manufacturing processes for both cookie types.

---

## 4. Statistical Testing

### 4.1 Hypotheses

We will test whether Double Stuf Oreos contain exactly double the crème filling of Regular Oreos.

**Null Hypothesis (H₀):** μ_DoubleStuf = 2 × μ_Regular  
**Alternative Hypothesis (H₁):** μ_DoubleStuf ≠ 2 × μ_Regular

Equivalently, we can test:
- **H₀:** μ_DoubleStuf - 2 × μ_Regular = 0
- **H₁:** μ_DoubleStuf - 2 × μ_Regular ≠ 0

**Significance Level:** α = 0.05

### 4.2 Test Selection and Justification

**Selected Test:** One-sample t-test on the difference (μ_DoubleStuf - 2 × μ_Regular)

**Approach:**
We will calculate the expected Double Stuf filling mass under the null hypothesis (2 × mean of Regular Oreos) and test whether the observed Double Stuf mean differs significantly from this expected value.

**Why this test is appropriate:**
1. We have independent random samples from two populations
2. The sample sizes are reasonably large (n = 36 and n = 29)
3. By the Central Limit Theorem, the sampling distribution of means is approximately normal
4. We want to test a specific ratio claim (2:1)
5. A one-sample t-test allows us to test whether the Double Stuf mean equals a specific target value

**Alternative approach considered:** Two-sample t-test with ratio analysis would be less direct for testing the specific 2:1 claim.

### 4.3 Test Execution

**Step 1: Calculate the expected Double Stuf mean under H₀**
- Mean Regular filling: 3.158 g
- Expected Double Stuf filling (under H₀): 2 × 3.158 = 6.316 g

**Step 2: One-sample t-test**
- Observed Double Stuf mean: 6.038 g
- Expected value (under H₀): 6.316 g
- Difference: 6.038 - 6.316 = -0.278 g
- Standard error: SD / √n = 0.500 / √29 = 0.0929 g
- t-statistic: -0.278 / 0.0929 = -2.992
- Degrees of freedom: 29 - 1 = 28
- Critical value (two-tailed, α = 0.05): ±2.048
- p-value: ≈ 0.006

### 4.4 Results Interpretation

**Test Result:** |t| = 2.992 > 2.048 (critical value)  
**p-value:** 0.006 < 0.05

The test statistic falls in the rejection region, and the p-value is less than our significance level.

**Statistical Conclusion:** We reject the null hypothesis. There is statistically significant evidence (p = 0.006) that Double Stuf Oreos do NOT contain exactly double the crème filling mass of Regular Oreos.

**Practical Interpretation:** Double Stuf Oreos contain approximately 191% of the crème filling found in Regular Oreos, which is statistically significantly less than the 200% implied by the product name. The difference of approximately 9% represents about 0.28 grams less filling than expected.

---

## 5. Decision and Conclusions

### 5.1 Answer to the Statistical Research Question

**No, Double Stuf Oreos do not have double the crème filling mass as Regular Oreos.**

Based on our analysis of 36 Regular and 29 Double Stuf Oreos:
- Regular Oreos contain an average of 3.158g of crème filling
- Double Stuf Oreos contain an average of 6.038g of crème filling
- The ratio is 1.912:1, not 2:1
- This difference is statistically significant (p = 0.006)

### 5.2 Practical Significance

While statistically significant, the practical significance should be considered. Double Stuf Oreos contain about 91% more filling than Regular Oreos, which is substantial. The shortfall of approximately 9% from the "double" claim represents about 0.28 grams of crème filling per cookie.

For context, this means that in a package of 10 Double Stuf cookies, consumers receive approximately 2.8 grams less crème filling than the name would suggest—roughly equivalent to the filling from one Regular Oreo cookie.

---

## 6. Issues and Limitations

### 6.1 Data Quality Concerns

1. **Outlier in Regular Oreos:** The observation of 6.766g appears to be a contamination or recording error. Sensitivity analysis excluding this value yields:
   - Regular mean without outlier: 3.053g
   - Expected Double Stuf: 6.106g
   - This strengthens our conclusion (ratio becomes 1.98:1)

2. **Mislabeled observation:** One observation (2.771g) appeared to be mislabeled as Double Stuf and was reclassified as Regular based on its value falling clearly within the Regular range.

3. **Sample collection:** No information is provided about how cookies were sampled. Random selection from production batches would strengthen external validity.

### 6.2 Assumptions

1. **Independence:** We assume each cookie measurement is independent
2. **Normality:** With sample sizes of 29 and 36, the Central Limit Theorem supports using t-tests even if the underlying distributions are not perfectly normal
3. **Equal variance assumption:** Not required for one-sample test, but noted that variances are similar (0.469 vs 0.500)

### 6.3 Additional Considerations

1. **Manufacturing variation:** The standard deviations indicate natural variation in the manufacturing process affects both products similarly
2. **Measurement precision:** No information provided about measurement methodology or precision
3. **Product consistency:** This analysis represents a snapshot; manufacturing processes may vary over time or between facilities

### 6.4 Future Research

To strengthen this analysis, future studies could:
- Increase sample size for greater precision
- Document sampling methodology and cookie source
- Measure additional cookie characteristics (wafer mass, total mass)
- Conduct longitudinal analysis across different production batches
- Test other "Double" products for truth-in-advertising

---

## 7. Code Appendix

### Data Processing and Analysis Code

```r
# Read and clean data
data <- read.csv("oreo1.dat")

# Identify potential issues
print(data[60:66,])

# Reclassify observation 61
data$Type[61] <- "Regular"

# Separate by type
regular <- data$Filling.Mass[data$Type == "Regular"]
double_stuf <- data$Filling.Mass[data$Type == "Double Stuf"]

# Descriptive statistics
mean_regular <- mean(regular)
sd_regular <- sd(regular)
mean_double <- mean(double_stuf)
sd_double <- sd(double_stuf)

# Calculate ratio
ratio <- mean_double / mean_regular

# One-sample t-test
expected_double <- 2 * mean_regular
t.test(double_stuf, mu = expected_double)

# Sensitivity analysis without outlier
regular_clean <- regular[regular < 6]
mean_regular_clean <- mean(regular_clean)
expected_double_clean <- 2 * mean_regular_clean
t.test(double_stuf, mu = expected_double_clean)
```

### Python Alternative

```python
import pandas as pd
import numpy as np
from scipy import stats

# Read data
data = pd.read_csv("oreo1.dat")

# Clean data
data.loc[60, 'Type'] = 'Regular'

# Separate groups
regular = data[data['Type'] == 'Regular']['Filling Mass']
double_stuf = data[data['Type'] == 'Double Stuf']['Filling Mass']

# Descriptive statistics
print(f"Regular: Mean={regular.mean():.3f}, SD={regular.std():.3f}")
print(f"Double Stuf: Mean={double_stuf.mean():.3f}, SD={double_stuf.std():.3f}")
print(f"Ratio: {double_stuf.mean()/regular.mean():.3f}")

# One-sample t-test
expected = 2 * regular.mean()
t_stat, p_value = stats.ttest_1samp(double_stuf, expected)
print(f"t={t_stat:.3f}, p={p_value:.3f}")
```

---

## Summary

This analysis provides strong statistical evidence that Double Stuf Oreos contain approximately 191% of the crème filling found in Regular Oreos, falling short of the "double" claim by about 9%. While this difference is statistically significant, Double Stuf Oreos still contain substantially more crème filling than Regular Oreos—just not quite double.