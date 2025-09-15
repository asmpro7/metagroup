<p align="center">
  <h1 align="center"> metagroup </h1>
 <p align="center"> <img src="https://github.com/user-attachments/assets/218b27ff-9359-4b59-952d-ea91cf661fb1" data-canonical-src="https://github.com/user-attachments/assets/218b27ff-9359-4b59-952d-ea91cf661fb1" width="300" height="300" /><p>
<h2 align="center" id="MetaDesc">Meaningful Grouping of Studies in Meta-Analysis</h2>
</p>

## Overview

`metagroup` provides a suite of tools to uncover hidden structures in meta-analytic data. It uses a two-step process to perform meaningful subgroup analysis:

1.  **Group:** Use iterative grouping functions (e.g., `mgbin()`, `mgcont()`) to partition studies into statistically homogeneous clusters based on their effect size data.
2.  **Interpret:** Use the `meaning()` function and its associated `plot()` method to analyze these new subgroups and understand their composition based on study-level characteristics (e.g., country, setting).

This approach helps to provide a deeper, more data-driven interpretation of heterogeneity in a meta-analysis.

---

## Installation

Install the stable version of `metagroup` from CRAN with:

```r
install.packages("metagroup")
```

Install the development version of `metagroup` from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("asmpro7/metagroup")
```

---

## Example Usage

Here is a basic example of the core workflow: first grouping the studies, then finding the meaning behind the groups.

```r
# 1. Load the package
library(metagroup)

# 2. Step 1: Group the studies by homogeneity
# The result contains the original data with a new 'subgroup' column
grouped_results <- mgbin(
  data = study_data,
  event.e = event.e,
  n.e = n.e,
  event.c = event.c,
  n.c = n.c,
  studlab = studlab,
  sm = "OR"
)

# 3. Step 2: Analyze the composition of the new subgroups
meaning_results <- meaning(
  data = grouped_results,
  variables = c("country", "setting")
)

# Print the summary table to see the dominant characteristics of each group
print(meaning_results)

# Plot the results to visualize the composition of all groups
plot(meaning_results)
```

---

## License

This package is licensed under the GPL-3 License.
