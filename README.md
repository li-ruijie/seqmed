***This is work in progress.***

# Background

`seqmed` is the output of my PhD research at King's College London [1] under Professors Sabine Landau, Richard Emsley and Kimberley Goldsmith. The problem that I set out to solve was that in sequential causal mediation, the current crop of software do not address the different scenarios that can come up in real life applications. Consider the following where `R` is the randomised treatment, `M` is the mediator and `Y` is the outcome:


```
┌─┐   ┌─┐   ┌─┐
│R│ → │M│ → │Y│
└─┘   └─┘   └─┘
└────────────↗
```

For a continuous mediator is nested in a binary outcome variable, and normal and logistic GLM are use to model the continuous mediator and binary outcome respectively, the current crop of software uses a form of estimation to derive the causal odds ratio. The estimation is valid only when the event rate of `Y` is low.

`seqmed` aims to do this estimation directly by simulation. Another problem arises when the number of mediators in the sequence is more than one.

```
┌─────────────↘
┌─┐   ┌──┐   ┌──┐   ┌─┐
│R│ → │M1│ → │M2│ → │Y│
└─┘   └──┘   └──┘   └─┘
│       └───────────↗↑
└────────────────────┘
```

The larger vision of `seqmed` is to be the basis of a flexible modelling framework that can address different models from the GLM families and beyond.

Its core uses are:

1. Conduct sequential causal mediation analysis in R regardless of the type of GLM used to estimate each.
2. Conduct sensitivity analysis to assess the robustness of the causal effect estimate to `M-Y` confounding.
3. Draw graphs to visualise the results of the causal mediation analysis and the sensitivity analysis.

# References

1. Li R. Causal inference in process evaluation: Development of statistical methods for causal inference in process evaluation of mental health related trials [Internet] [Doctor of Philosophy]. [London, United Kingdom]: King’s College London; 2022 [cited 2023 Feb 1]. Available from: https://kclpure.kcl.ac.uk/portal/en/theses/causal-inference-in-process-evaluation(4c4eaaa8-beee-4f24-9d03-8c29f2b83fce).html

# To-do

1. Documentation
2. Installation instructions
3. Tidy up the code and include only the necessary functions.
4. Package it and send it to CRAN.

