---
title: ""
author: ""
date: ""
output: html_document
---


<br>
<br>

This function will return the statistical *power* for specified sample size for estimating $G \times E$ interaction  parameter, $\beta_3$, from a case-control model:

\[ log \bigg(\frac{p}{1-p}\bigg) = \beta_0 + \beta_1 G + \beta_2 E + \beta_3 (G \times E) \]


<br>
<br>

- $N_{case}$: Number of cases
- $N_{cont}$: Number of controls

- $P_g$: Prevalance of genotype risk factor
- $P_e$: Prevalance of environmental risk factor
- $OR_g$: Odds-Ratio for genotype risk factor
- $OR_e$: Odds-Ratio for environmental risk factor
- $OR_{ge}$: Odds-Ratio for $G \times E$ interaction
- $OR(G,E)$: Odds-ratio describing correlation between $G$ and $E$ in the source population. $OR(G,E)=1$ indicates no correlation.
- Alpha: Type I error rate
- \# Tests: Number of tests to be performed (i.e. number of variants in GWAS analysis). Will use Bonferonni correction to adjust power or sample size for specified number of tests.
