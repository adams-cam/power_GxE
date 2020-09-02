---
title: ""
author: ""
date: ""
output: html_document
---


<br>

This function will return the statistical **power** (1-Type II Error) for specified sample size for estimating $G \times E$ interaction parameter from a case-only model:

\[ log \bigg(\frac{p_E}{1-p_E}\bigg) = \eta_0 + \eta_1E\]

Where $\eta_1 \approx \beta_3$ from a traditional case-control interaction model under the following assumptions:

- Independnce between $G$ and $E$ risk factors in the source population
- Rare disease

For more information see:

- VanderWeele TJ. Sample Size and Power Calculations for Additive Interactions. Epidemiol Methods. 2012;1(1):159-188. [doi:10.1515/2161-962X.1010](https://dx.doi.org/10.1515%2F2161-962X.1010).

<br>

Input parameters:

- $N_{case}$: Number of cases
- $P_g$: Prevalance of genotype risk factor
- $P_e$: Prevalance of environmental risk factor
- $OR_g$: Odds-Ratio for genotype risk factor
- $OR_e$: Odds-Ratio for environmental risk factor
- $OR_{ge}$: Odds-Ratio for $G \times E$ interaction
- Alpha: Type I error rate
- \# Tests: Number of tests to be performed (i.e. number of variants in GWAS analysis). Will use Bonferonni correction to adjust power or sample size for specified number of tests.
