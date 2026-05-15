# Methods for tightening CIs in the quasi-Bayesian simulation

Investigation of statistical methods to narrow confidence intervals
in `sim.med()` without additional data. Methods are categorised into
those that genuinely reduce CI width, those that improve calibration
(tighter when current CIs are conservative), and those that reduce
Monte Carlo estimation error (same true CI, better estimated
endpoints).

## A. Genuine CI width reductions

### 1. Rao-Blackwellization (integrate out error terms)

Currently `gen.err.1` (sim-med.R:291) adds stochastic error terms —
Gaussian for continuous models, logistic for binary. The per-iteration
ATE at line 796 averages over N individuals so errors mostly cancel
by LLN, but "mostly" is not "fully."

The causal estimand is E[Y(r) | X] — the expectation over the
stochastic error, not a particular realisation. By setting
`b.exv = TRUE` for all models (not just "auto"), the error terms are
analytically integrated out. Each iteration's ATE then has variance
only from the drawn coefficients beta*, not from simulated noise.
Less dispersion across iterations means genuinely tighter CIs.

The code already supports this — `set.exv` (utils.R:363) controls it.
Under "auto", the outcome model (Y) always uses expected values, but
mediator models only do so for binomial family. For Gaussian mediators,
errors are simulated. Forcing expected values everywhere
Rao-Blackwellises the simulation.

**Caveat:** for binary mediators with `b.exv = FALSE`, the simulation
thresholds the latent utility (`(m > 0) + 0` in `do.x`, utils.R:615)
to produce binary mediator values. The expected-value version uses
P(M=1|X,R=r) as a continuous weight (`sim.1dep` already does this at
sim-med.R:748 via probability marginalisation). For models with binary
mediators, the code is already partially Rao-Blackwellised. The gain
is for Gaussian mediators where errors are currently simulated.

#### Error cancellation analysis

Within each iteration, the counterfactuals Y*_j(r=1) and Y*_j(r=0)
for the same individual j share the same error draw (same seed). This
means errors cancel exactly in some estimands but not others.

**Difference estimand (continuous Y, identity link):** Y uses
`b.exv = TRUE` under "auto", so no error in Y itself. A Gaussian
mediator error epsilon_j enters through the mediator:

    Y*_j(r=1) = b0 + bR + bM1*(a0 + aR + aX*Xj + ej) + bX*Xj
    Y*_j(r=0) = b0 + bM1*(a0 + aX*Xj + ej) + bX*Xj

    Y*_j(r=1) - Y*_j(r=0) = bR + bM1*aR

Exact cancellation — the difference is constant across individuals
and free of epsilon. Averaging at line 796 gives exactly bR + bM1*aR.
Rao-Blackwellization gives **zero benefit** here.

**OR estimand (binary Y, logistic link):** the individual-level
linear predictors also cancel:

    eta_j(r=1) - eta_j(r=0) = bR + bM1*aR

So the individual-level OR = exp(bR + bM1*aR) is free of epsilon and
constant across individuals. But the code does not compute individual
ORs. It averages probabilities first (line 796), then applies `ce.odr`
to the mean probabilities:

    OR = [mean_j(p_j(1)) / (1 - mean_j(p_j(1)))]
       / [mean_j(p_j(0)) / (1 - mean_j(p_j(0)))]

The logistic link is non-linear, so by Jensen's inequality:

    mean_j(logistic(eta_j + bM1*ej)) != logistic(mean_j(eta_j + bM1*ej))

The mediator errors perturb the mean probability. Individual ORs are
all exp(constant), but the OR of mean probabilities != mean of
individual ORs. The epsilon enters through the non-linearity of the
logistic link acting on the population average.

The magnitude depends on:
- How far probabilities are from 0.5 (where logistic is approximately
  linear and Jensen's bias is minimal)
- How large bM1 * sigma_epsilon is (how much mediator error shifts
  individuals along the logistic curve)

Rao-Blackwellization gives **positive benefit** for the OR estimand,
specifically through the interaction of Gaussian mediator errors with
the logistic non-linearity in the population-level averaging step.

#### Intuition

In each simulation iteration, `gen.coef` draws beta* and `gen.err`
draws error terms epsilon*. The counterfactual for individual j is:

    Y*_j(r) = f(X_j' beta* + epsilon*_j)

Then line 796 averages: ATE*_i = (1/N) sum_j [Y*_j(r=1) - Y*_j(r=0)].

The epsilon*_j add noise to each individual's counterfactual but
cancel out in the average (by LLN). They don't cancel perfectly —
with N=500, the residual noise contributes var(epsilon)/N to the
variance of ATE*_i. That extra variance inflates the spread of
ATE*_1, ..., ATE*_K across iterations, widening the CI.

The fix: don't simulate epsilon. Instead compute E[Y_j(r) | X_j,
beta*] directly — that's what `b.exv = TRUE` does. For a Gaussian
model, E[Y|X,beta*] = X'beta* (no noise needed). For logistic, it's
the predicted probability. You get the same expected ATE but without
the noise from individual error draws.

Think of it this way: you're averaging N coin-flip simulations to
estimate a probability. You could flip the coins, or you could just
use the probability directly. The latter has zero variance from the
coin flips.

However, for the difference estimand specifically, the error
cancellation analysis above shows the coin flips cancel exactly
between counterfactuals, making this a no-op. The benefit is confined
to the OR estimand where the logistic non-linearity prevents exact
cancellation.

- **Changes CI width:** no for difference estimand; yes for OR
- **Difficulty:** low (change default in `set.exv`)
- **Expected benefit:** zero for continuous Y with identity link;
  small to moderate for binary Y with logistic link, depending on
  how far mean probabilities are from 0.5 and the magnitude of
  bM1 * sigma_epsilon

### 2. Variance-stabilising transformations

The CI is computed as quantiles of simulated ATE draws (sim-med.R:958).
For the odds-ratio estimand (`ce.odr`), the distribution is
right-skewed. Quantile-based CIs on skewed distributions are wider
than necessary — the upper tail extends further.

Working on the log-OR scale:
- Compute log(OR*_i) for each iteration
- The log-OR distribution is more symmetric (approximately normal)
- CI endpoints = exp(quantile(log(OR*), probs))
- Resulting CI on the OR scale is asymmetric but tighter

For the difference estimand (`ce.dif`), the distribution is typically
close to symmetric and the gain is negligible.

#### Intuition

When the outcome is binary, causal effects are on the odds-ratio
scale. The distribution of simulated OR* values is right-skewed —
bounded below by 0, unbounded above. The 95% quantile CI on a skewed
distribution is wider than it needs to be because the upper tail
stretches further than the lower tail.

log(OR) is approximately normal (it's a ratio of probabilities, and
log linearises ratios). If you compute CIs on the log scale and
exponentiate back:

    CI = [exp(Q_{0.025}(log(OR*))), exp(Q_{0.975}(log(OR*)))]

The resulting interval on the OR scale is asymmetric (shorter on the
left, longer on the right) but has the same coverage with smaller
total width. The difference is most noticeable when the OR is far
from 1.

For the difference estimand (continuous Y), the distribution is
already close to symmetric, so this does nothing useful.

- **Changes CI width:** yes
- **Difficulty:** low (transform before quantile, back-transform after)
- **Expected benefit:** moderate for binary outcomes (odds ratios);
  negligible for continuous outcomes

### 3. Profile likelihood CIs

The current approach draws beta* from MVN(beta-hat, V-hat), which is
the Wald approximation to the posterior. For non-linear models
(logistic regression), V-hat is the inverse observed Fisher
information, and the Wald approximation can be conservative — the
actual likelihood contour may be smaller than the MVN ellipsoid in
some directions.

Profile likelihood CIs are based on the likelihood ratio:

    {theta : 2[l(theta-hat) - l_p(theta)] <= chi^2_{1,alpha}}

where l_p(theta) profiles out nuisance parameters. These are:
- Transformation-invariant (unlike Wald)
- Respect the curvature of the log-likelihood
- Can be asymmetric and tighter, especially in moderate samples
  with logistic regression

**Implementation:** instead of drawing from MVN, draw beta* from the
profile-likelihood-calibrated distribution. Requires either numerical
profiling or the Tierney-Kadane approximation to the marginal
posterior.

#### Intuition

The simulation draws beta* from MVN(beta-hat, V-hat). This is the
Wald approximation — it says the likelihood surface is a perfect
ellipsoid centred at the MLE. For linear models this is exact. For
logistic regression it's an approximation, and the real likelihood
surface can be asymmetric or narrower in certain directions.

Profile likelihood follows the actual likelihood contour. For a
single parameter theta_k, you find all values where the
log-likelihood hasn't dropped too much from the maximum:

    {theta_k : max_{theta_{-k}} l(theta) >= l(theta-hat) - chi^2/2}

This gives intervals that respect the true shape of the likelihood.
If the Wald ellipsoid is fatter than the real likelihood contour
(which happens in logistic regression with moderate samples), the
profile CI is genuinely narrower.

The difficulty: instead of drawing from a known MVN, you'd need to
draw from the profile-likelihood-calibrated distribution, which
doesn't have a closed form. You'd need MCMC or importance sampling.

- **Changes CI width:** yes
- **Difficulty:** high (numerical profiling, new sampling scheme)
- **Expected benefit:** moderate for logistic models with moderate N;
  negligible for linear models (Wald = profile asymptotically)

### 4. Higher-order asymptotic corrections

The Wald CI is first-order accurate: coverage = (1-alpha) + O(1/n).
Higher-order methods improve this:

- **Bartlett correction:** multiplies the likelihood ratio statistic
  by a correction factor so its distribution matches chi-squared to
  O(1/n^2). If the first-order CI is conservative (coverage >
  nominal), the Bartlett-corrected CI is tighter with the same
  nominal coverage.

- **r* statistic** (Barndorff-Nielsen 1991): a modified signed
  likelihood ratio statistic with O(1/n^(3/2)) accuracy. CIs based
  on r* are among the most accurate available for finite samples.

Both require the current Wald/MVN CIs to be conservative. This is
common in logistic regression with moderate samples but uncommon in
linear regression.

#### Intuition

The Wald approximation is "first-order" — it's right up to O(1/n)
error in coverage. With n=200 that's ~0.5% coverage error.
Higher-order methods reduce this.

Bartlett correction: the likelihood ratio statistic chi^2_LR is
distributed as chi^2 to first order. But it has a known expected
value E[chi^2_LR] = p(1 + b/n) where b is computable. Dividing by
(1 + b/n) makes it chi^2 to O(1/n^2). If your original CI was
conservative because of this first-order error, the corrected CI is
tighter.

r* (Barndorff-Nielsen): an even more refined correction to the signed
likelihood ratio. Achieves O(1/n^(3/2)) accuracy for one-sided tests.
Essentially the most accurate frequentist CI you can get without exact
computation.

Both only help if the current CIs are conservative. For linear models
they usually aren't (Wald is already exact). For logistic models with
moderate samples they often are.

- **Changes CI width:** yes (if current CIs are conservative)
- **Difficulty:** high (analytical derivation per estimand)
- **Expected benefit:** meaningful for small-to-moderate samples with
  non-linear models

## B. CI calibration improvements

### 5. Bootstrap calibration (double bootstrap)

The quasi-Bayesian quantile CI may not have exact nominal coverage.
If it over-covers (conservative), bootstrap calibration can tighten
it:

1. Compute the simulation CI with nominal alpha
2. For each of B bootstrap resamples of the original data, recompute
   the simulation CI
3. Estimate the actual coverage of the original CI across bootstrap
   resamples
4. Adjust alpha to achieve nominal coverage

If the original 95% CI actually has 97% coverage, calibration finds
a narrower interval with true 95% coverage.

#### Intuition

This asks: does the 95% CI from `sim.med` actually have 95%
coverage? It might have 97% — meaning it's wider than necessary.

To check and fix:
1. Resample the data with replacement (bootstrap)
2. On each bootstrap sample, refit models, rerun the full simulation,
   get CIs
3. Check what fraction of these CIs contain the full-sample point
   estimate
4. If that fraction is 97%, you know your CIs are conservative
5. Adjust: use 93.5% nominal level to get true 95% coverage

This is a "calibration" — you're not changing the method, you're
finding the right knob setting. The downside is cost: if the
simulation runs 10,000 iterations, and you do 200 bootstrap
resamples, that's 2 million model fits.

- **Changes CI width:** yes, if current CIs over-cover
- **Difficulty:** medium (nested simulation, computationally expensive)
- **Expected benefit:** unknown — requires empirical investigation of
  current coverage properties

### 6. BCa (bias-corrected and accelerated) intervals

For the `s.cof.mth = "boot"` path (bootstrap coefficients rather than
MVN draws), BCa intervals correct for both bias and skewness in the
bootstrap distribution. They can be substantially tighter than
percentile-based intervals when:
- The bootstrap distribution is skewed
- The estimand has a non-zero bias

The current code uses quantiles at sim-med.R:958. Replacing with BCa
requires computing the bias-correction constant z0 and acceleration
constant a from the simulation draws.

#### Intuition

Standard percentile CIs take quantiles of the simulation draws:
[Q_{0.025}, Q_{0.975}]. BCa adjusts the quantile positions for two
things:

**Bias correction (z0):** if the distribution of simulation draws is
centred away from the point estimate, the median of the draws != the
point estimate. z0 measures this shift and adjusts the quantile
positions accordingly.

**Acceleration (a):** if the variance of the estimator changes with
the parameter value (which happens for non-linear estimands like ORs),
the CI should be asymmetric. The acceleration constant a captures
this.

The adjusted quantile positions are:

    a1 = Phi(z0 + (z0 + z_{alpha/2}) / (1 - a*(z0 + z_{alpha/2})))
    a2 = Phi(z0 + (z0 + z_{1-a/2}) / (1 - a*(z0 + z_{1-a/2})))

Both z0 and a are computable from the existing simulation draws (z0
from the proportion of draws below the point estimate; a from the
jackknife). No additional simulation runs needed — you just change
which quantiles you read off.

- **Changes CI width:** yes, when distribution is skewed or biased
- **Difficulty:** low (compute z0 and a from existing draws)
- **Expected benefit:** moderate, especially for non-linear estimands
  (odds ratios, indirect effects in logistic models)

## C. Monte Carlo precision (same CI, better estimation)

These do not narrow the true CI but reduce noise in the estimated CI
endpoints. Relevant when computational budget limits `int.sims`.

### 7. Antithetic variates

For each MVN draw beta*_i, pair it with the "mirror" draw
2*beta-hat - beta*_i. These are negatively correlated — when one
overestimates, the other underestimates. The pair has the same
marginal distribution as two independent draws, but the average of
their ATEs has lower variance.

Reduces Monte Carlo error by roughly a factor of 2.

#### Intuition

Each iteration draws beta*_i from MVN(beta-hat, V-hat). The
antithetic draw is:

    beta*_i' = 2*beta-hat - beta*_i

This is the mirror image through the mean. If beta*_i is 1 SD above
the mean in some direction, beta*_i' is 1 SD below. Both are valid
MVN draws (the distribution is symmetric), and they're negatively
correlated.

Run the simulation for both, get ATE*_i and ATE*_i', and use
(ATE*_i + ATE*_i')/2 as one estimate. The negative correlation means
the variance of this average is lower than the variance of two
independent draws averaged.

You get the same true distribution but estimate it with less Monte
Carlo noise. With K/2 antithetic pairs you get roughly the precision
of K independent draws.

- **Changes CI width:** no (MC precision only)
- **Difficulty:** low (mirror each draw in `gen.coef`)
- **Expected benefit:** ~2x MC efficiency

### 8. Control variates (delta method as control)

The delta-method estimate of the ATE variance is analytically
available. Within each iteration, the delta-method ATE approximation:

    ATE*_delta,i ~ ATE(beta-hat) + grad(ATE(beta-hat))' (beta*_i - beta-hat)

is highly correlated with the actual ATE*_i (they agree to first
order). The control variate estimator:

    ATE*_adj,i = ATE*_i - gamma * (ATE*_delta,i - E[ATE*_delta])

where gamma is the optimal control coefficient, reduces MC variance.
Since E[ATE*_delta] = ATE(beta-hat) is known, this is straightforward.

#### Intuition

The delta method gives a first-order linear approximation to the ATE:

    ATE_approx(beta*) ~ ATE(beta-hat) + grad' (beta* - beta-hat)

where grad is the gradient of ATE with respect to beta, evaluated at
beta-hat. This is cheap to compute and highly correlated with the
true ATE(beta*).

The insight: the error in the simulation is ATE*(beta*) - true_ATE.
Part of that error is predictable from ATE_approx — since we know
E[ATE_approx] = ATE(beta-hat), we can subtract the predictable part:

    ATE*_adjusted = ATE* - gamma * (ATE_approx - ATE(beta-hat))

With optimal gamma (estimated from the draws), this removes the
linear component of the MC error. What remains is the second-order
deviation between ATE* and its linear approximation — much smaller.

This is the most powerful MC variance reduction technique on this
list, but requires computing the gradient of ATE w.r.t. beta. For the
sequential mediation setting with chained models, that gradient
involves the chain rule through the counterfactual computation.

- **Changes CI width:** no (MC precision only)
- **Difficulty:** medium (need gradient of ATE w.r.t. beta)
- **Expected benefit:** ~5-10x MC efficiency

### 9. Quasi-random sequences

Replace pseudorandom MVN draws with low-discrepancy sequences (Sobol,
Halton) transformed through the inverse CDF. These fill the parameter
space more uniformly, giving better tail estimation and smoother
quantile estimates with fewer iterations.

#### Intuition

Pseudorandom numbers cluster — some regions of the parameter space
get more samples, others fewer. This is fine asymptotically but
wastes samples in finite settings.

Low-discrepancy sequences (Sobol, Halton) fill the space more
uniformly. Instead of `rmvnorm(K, beta-hat, V-hat)`, you generate K
points from a Sobol sequence in [0,1]^p and transform them through
the inverse CDF of the MVN distribution.

The result: better coverage of the tails, smoother quantile estimates,
and more stable CI endpoints. The improvement is most noticeable for
tail quantiles (which is exactly what CIs are) — a Sobol sequence
with 2,000 points can match the tail accuracy of 10,000 pseudorandom
points.

Scrambled Sobol (Owen scrambling) adds randomisation while preserving
the low-discrepancy property. This allows MC error estimation from
independent scrambled replicates — you get the space-filling benefit
without losing the ability to assess convergence. Available in R via
`qrng::sobol(n, d = p, randomize = "Owen")`.

- **Changes CI width:** no (MC precision only)
- **Difficulty:** low (use `qrng::sobol` in place of `rmvnorm`)
- **Expected benefit:** ~3-5x MC efficiency

## Summary

```
┌─────────────────────────────┬────────────┬────────────┬──────────────┐
│ Method                      │ Narrows CI │ Difficulty │ Benefit      │
├─────────────────────────────┼────────────┼────────────┼──────────────┤
│ 1. Rao-Blackwellisation     │ OR only    │ Low        │ Small-mod    │
│ 2. Variance-stabilising     │ Yes        │ Low        │ Mod (binary) │
│ 3. Profile likelihood       │ Yes        │ High       │ Moderate     │
│ 4. Higher-order corrections │ Yes        │ High       │ Moderate     │
│ 5. Bootstrap calibration    │ If conserv │ Medium     │ Unknown      │
│ 6. BCa intervals            │ If skewed  │ Low        │ Moderate     │
│ 7. Antithetic variates      │ No (MC)    │ Low        │ ~2x MC       │
│ 8. Control variates         │ No (MC)    │ Medium     │ ~5-10x MC    │
│ 9. Quasi-random sequences   │ No (MC)    │ Low        │ ~3-5x MC     │
└─────────────────────────────┴────────────┴────────────┴──────────────┘
```

**Low-hanging fruit:** Rao-Blackwellisation (#1, OR estimand only —
errors cancel exactly for difference estimand), variance-stabilising
transforms for OR (#2), BCa intervals (#6). Minimal code changes,
genuine or calibration-based CI narrowing.

**Theoretically interesting:** profile likelihood (#3), higher-order
corrections (#4). Meaningful for logistic models with moderate samples
but require substantial analytical and implementation work.

**Quick wins for computational budget:** antithetic variates (#7) and
quasi-random sequences (#9) are easy to implement and reduce the
number of iterations needed for stable CI endpoints.
