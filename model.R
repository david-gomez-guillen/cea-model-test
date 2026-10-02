# A minimal cost-effectiveness model of a hypothetical cancer.
#
# It is a Markov cohort model. Rather than following people one by one, it keeps
# track of which share of a group of people, the cohort, is in each health state,
# and moves those shares from state to state once a year.
#
#   States:      Healthy, Cancer, Dead
#   Cycle:       one year
#   Horizon:     from age 30 to age 74
#   Strategies:  no_intervention, screening, treatment
#
# For each strategy the model adds up what the cohort costs and how much health
# it enjoys. Comparing those two totals between strategies is what a
# cost-effectiveness analysis does.

START.AGE <- 30
END.AGE <- 74

# Results by age are reported per five-year age group: 30-34, 35-39, ..., 70-74.
AGE.GROUP.SIZE <- 5

simulate <- function(strategies,
                     p.healthy.cancer,
                     p.healthy.death,
                     p.cancer.death,
                     p.cancer.recovery,
                     p.screening.effective,
                     p.treatment.effective,
                     cost.screening,
                     cost.cancer.treatment,
                     utility.cancer,
                     discount) {

  ages <- START.AGE:END.AGE

  summary <- data.frame()
  incidence <- list()
  cohort.info <- list()

  for (strategy in strategies) {

    # ---- What the strategy changes ----------------------------------------
    # A strategy is the same model with a few numbers changed. Screening
    # prevents some cancers and is paid for every healthy person. Treatment
    # raises the chance of recovery and is paid for every person with cancer.
    if (!strategy %in% c('no_intervention', 'screening', 'treatment'))
      stop('Unknown strategy: ', strategy)

    screening <- strategy == 'screening'
    treatment <- strategy == 'treatment'

    # Cancer can resolve on its own under every strategy. Treatment adds to that.
    p.recovery <- if (treatment) p.cancer.recovery + p.treatment.effective else p.cancer.recovery

    # What a person in each state costs in a year, and the quality of life of
    # that year: 1 is a year in full health and 0 is being dead.
    state.costs <- c(healthy = if (screening) cost.screening else 0,
                     cancer = if (treatment) cost.cancer.treatment else 0,
                     dead = 0)
    state.utilities <- c(healthy = 1, cancer = utility.cancer, dead = 0)

    # ---- The cohort ---------------------------------------------------------
    # The share of the cohort in each state. Everyone starts healthy, and the
    # three shares always add up to 1.
    cohort <- c(healthy = 1, cancer = 0, dead = 0)

    # One value per year, filled in as the cohort ages.
    costs <- c()
    utilities <- c()
    yearly.incidence <- c()
    cohort.trace <- list(cohort)

    for (age in ages) {
      years.elapsed <- age - START.AGE

      # ---- 1. Probability of developing cancer this year --------------------
      # It is either one value for every age or a list with one value per age
      # group, in which case the one of the current age is used.
      if (is.list(p.healthy.cancer)) {
        age.group <- years.elapsed %/% AGE.GROUP.SIZE + 1
        p.onset <- p.healthy.cancer[[age.group]]
      } else {
        p.onset <- p.healthy.cancer
      }
      if (screening) p.onset <- p.onset * (1 - p.screening.effective)

      # ---- 2. Transition matrix -----------------------------------------------
      # Row: the state a person is in now. Column: the state a year later.
      # Each row adds up to 1, since everyone has to end up somewhere, so the
      # probability of staying is whatever the other transitions leave.
      # Dead is an absorbing state: once there, nobody leaves.
      transitions <- matrix(
        c(1 - p.onset - p.healthy.death, p.onset,                            p.healthy.death,
          p.recovery,                    1 - p.recovery - p.cancer.death,    p.cancer.death,
          0,                             0,                                  1),
        nrow = 3, byrow = TRUE,
        dimnames = list(names(cohort), names(cohort)))

      # ---- 3. Costs and health of this year ---------------------------------
      # Each state contributes its cost and its utility in proportion to the
      # share of the cohort in it. Money and health count for less the further
      # in the future they are, which is what discounting expresses: at a rate
      # of 3%, what happens a year from now is worth 1 / 1.03 of the same today.
      discount.factor <- 1 / (1 + discount)^years.elapsed
      costs <- c(costs, sum(cohort * state.costs) * discount.factor)
      utilities <- c(utilities, sum(cohort * state.utilities) * discount.factor)

      # ---- 4. Cancer incidence of this year ---------------------------------
      # New cases among the people alive. Only the healthy can develop cancer,
      # but incidence is measured over everyone alive, as registries report it.
      new.cases <- cohort[['healthy']] * p.onset
      alive <- cohort[['healthy']] + cohort[['cancer']]
      yearly.incidence <- c(yearly.incidence, new.cases / alive)

      # ---- 5. Move the cohort one year forward ------------------------------
      # Multiplying the shares by the matrix sends each share to where its row
      # says, which gives the shares at the start of next year.
      cohort <- drop(cohort %*% transitions)
      cohort.trace <- c(cohort.trace, list(cohort))
    }

    # ---- Results of the strategy ----------------------------------------------
    # C is the average discounted cost per year and E the discounted
    # quality-adjusted life years (QALYs) accumulated over the whole horizon.
    summary <- rbind(summary, data.frame(strategy = strategy,
                                         C = mean(costs),
                                         E = sum(utilities)))

    # Incidence per age group: the average over the years of the group.
    incidence[[strategy]] <- colMeans(matrix(yearly.incidence, nrow = AGE.GROUP.SIZE))

    cohort.info[[strategy]] <- cohort.trace
  }

  list(summary = summary,
       incidence = incidence,
       cohort.info = cohort.info)
}
