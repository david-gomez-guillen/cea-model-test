# Test model

A minimal cost-effectiveness model of a hypothetical cancer, used as an example and
test bed for this shiny interface. It compares doing nothing against a screening
programme, against treating diagnosed cases, and against treating them with a more
effective but more toxic and more expensive experimental drug, over the lifetime of a
single cohort.

## Model structure

The model is a three-state Markov cohort model with annual cycles:

- **Healthy**: alive and cancer-free. The whole cohort starts here.
- **Cancer**: alive with cancer. Reachable from *Healthy*, and (except under
  `no_intervention`) can go back to *Healthy* when treatment is effective.
- **Dead**: absorbing state, reached from both *Healthy* and *Cancer*.

The cohort is followed from age 30 to age 74 (45 cycles), with no new entries.
Transitions are applied at the end of each cycle, after costs and utilities of the
cycle have been accrued.

## Parameters

| Parameter | Base value | Class | Meaning |
|---|---|---|---|
| `p.healthy.cancer` | 0.075 | General | Annual probability of developing cancer while healthy. |
| `p.healthy.death` | 0.00001 | General | Annual probability of death while healthy. |
| `p.cancer.death` | 0.0001 | General | Annual probability of death while having cancer, under every strategy except `experimental_treatment`. |
| `p.screening.effective` | 0.05 | Screening | Proportion of cancer cases prevented by screening. |
| `p.treatment.effective` | 0.05 | Treatment | Annual probability of moving back from *Cancer* to *Healthy* under `screening` and `treatment`. |
| `p.experimental.treatment.effective` | 0.1 | Treatment | Annual probability of moving back from *Cancer* to *Healthy* under `experimental_treatment`. |
| `p.experimental.cancer.death` | 0.005 | Treatment | Annual probability of death while having cancer under `experimental_treatment`, replacing `p.cancer.death`. |
| `cost.screening` | 10000 | Screening | Annual cost per healthy person under `screening`. |
| `cost.cancer.treatment` | 50000 | Treatment | Annual cost per person with cancer under `treatment`. |
| `cost.experimental.cancer.treatment` | 75000 | Treatment | Annual cost per person with cancer under `experimental_treatment`. |
| `utility.cancer` | 0.6 | General | Utility of a year spent in *Cancer*. |
| `discount` | 0.03 | General | Discount rate. |

The class is only used to group the parameters in the interface.

## Strata

Results are reported by five-year age group, from 30-34 to 70-74.

## Outputs

- `summary`: one row per strategy with the cost (`C`, the mean discounted cost per
  cycle) and the effect (`E`, the discounted quality-adjusted life years accumulated
  over the whole horizon).
- `incidence`: cancer incidence per stratum, i.e. the mean over the five years of the
  age group of the proportion of the healthy cohort that develops cancer.

## Calibration

The `standard` scheme calibrates `p.healthy.cancer` against cancer incidence targets,
one per age group, using the `no_intervention` strategy. Because the parameter is
estimated per stratum, the calibration searches over nine values, one for each age
group, starting from 0.075 everywhere.

The error is the sum of squared differences between the simulated and the target
incidence over the strata present in the target; parameter sets that make the
simulation fail get an infinite error.

The targets go from 0.01 in the 30-34 group up to 0.14 in the 70-74 group, so every
stratum contributes to the error.

For the latent space methods, the training set is drawn by scaling the initial guess
by a uniform random factor per parameter (capped at 1), with 500 samples, 50 training
epochs and 7 latent dimensions.
