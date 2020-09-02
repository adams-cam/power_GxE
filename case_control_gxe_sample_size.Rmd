---
title: ""
author: ""
date: ""
output: html_document
---


<br>
<br>


This function will return the sample size required for specified power for estimating $G \times E$ interaction parameter from a case-control model:

\[ log\bigg(\frac{p}{1-p}\bigg)  = \beta_0 + \beta_1 G + \beta_2 E + \beta_3 (G \times E) \]

Where $\beta_3=log(OR_{Gxe})$, the multiplicative interaction parameter.

<br>
<br>

- Controls:Case: Number of controls for each case
- Inheritence: Mode of inheritence for genetic effect
- $P_g$: Prevalance of risk allele in population risk factor
- $P_e$: Prevalance of environmental risk factor in the population
- $OR_g$: Odds-Ratio for genotype risk factor in population
- $OR_e$: Odds-Ratio for environmental risk factor in population
- $OR_{gxe}$: Odds-Ratio for $G \times E$ interaction in population
- Alpha: Type I error rate
- Beta: Power or 1-Type II error
- \# Tests: Number of tests to be performed (i.e. number of variants in GWAS analysis). Will use Bonferonni correction to adjust power or sample size for specified number of tests.


Notes:

_Value ranges_

You can specify either a single value for $P_g$ and $OR_{GxE}$, or a range of values, using **From:** and **To:** input boxes.

_P(G=g)_

Depending on the mode of Inheritence, $P_g$ will be converted into a probability of exposure $P(G=1|P_g,Inheritence)$ assuming Hardy Weinberg Equilirium:

\[ p^2 + 2pq + q^2 \]

Where $p$ is the frequency of the allele in teh population or $P_g$, and $q = 1-p$, or the frequency of the other allele. If Inheritence == "Dominant": $P(G=1) = p^2 + 2pq$. If recessive: $P(G=1) = q^2$

