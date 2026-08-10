simulate <- function(strategies,
                     p.healthy.cancer,
                     p.healthy.death,
                     p.cancer.death,
                     p.screening.effective,
                     p.treatment.effective,
                     p.experimental.treatment.effective,
                     p.experimental.cancer.death,
                     cost.screening,
                     cost.cancer.treatment,
                     cost.experimental.cancer.treatment,
                     utility.cancer,
                     discount) {

  df <- data.frame()
  cohort.states <- list()
  incidence <- list()

  strata <- c('30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74')
  p.healthy.cancer.original <- p.healthy.cancer
  for(strategy in strategies) {
    #Sys.sleep(3)

    costs <- c()
    utilities <- c()
    cancer.incidence <- c()

    cohort <- list()
    cohort[[1]] <- c(1,0,0)
    for(year in seq(30, 74)) {
      state.costs <- c(0, 0, 0)
      state.utilities <- c(1, utility.cancer, 0)

      if (is.list(p.healthy.cancer.original)) {
        # In this example, if p.healthy.cancer is a list, it means that we have different probabilities for different age groups. 
        # We need to select the appropriate one based on the current year.
        p.healthy.cancer <- p.healthy.cancer.original[[(year-30) %/% 5 + 1]]
      } else {
        # If p.healthy.cancer is not a list, we assume it's a single value that applies to all age groups.
        p.healthy.cancer <- p.healthy.cancer.original
      }
      # Most strategies share the background probability of dying while having cancer,
      # so it is only overridden by the ones that change it.
      p.cancer.death.strategy <- p.cancer.death
      if (strategy == 'no_intervention') {
        state.costs[1] <- 0
        state.costs[2] <- 0
        p.cancer <- p.healthy.cancer
        p.cancer.healthy <- 0
      } else if (strategy == 'screening') {
        state.costs[1] <- cost.screening
        state.costs[2] <- 0
        p.cancer <- p.healthy.cancer * (1-p.screening.effective)
        p.cancer.healthy <- p.treatment.effective
      } else if (strategy == 'treatment') {
        state.costs[1] <- 0
        state.costs[2] <- cost.cancer.treatment
        p.cancer <- p.healthy.cancer
        p.cancer.healthy <- p.treatment.effective
      } else if (strategy == 'experimental_treatment') {
        # Like 'treatment', but the experimental drug cures cancer more often and is also
        # more toxic, so both the cancer to healthy and the cancer to death probabilities
        # are higher, as is the cost of treating each case.
        state.costs[1] <- 0
        state.costs[2] <- cost.experimental.cancer.treatment
        p.cancer <- p.healthy.cancer
        p.cancer.healthy <- p.experimental.treatment.effective
        p.cancer.death.strategy <- p.experimental.cancer.death
      }

      tp.matrix <- matrix(c(1-p.cancer-p.healthy.death, p.cancer, p.healthy.death,
                            p.cancer.healthy, 1-p.cancer.healthy-p.cancer.death.strategy, p.cancer.death.strategy,
                            0, 0, 1),
                          nrow=3, byrow = TRUE)
                          
      costs <- c(costs, state.costs %*% cohort[[year-29]] * (1-discount)^(year-30))
      utilities <- c(utilities, state.utilities %*% cohort[[year-29]] * (1-discount)^(year-30))
      cancer.incidence <- c(cancer.incidence, (cohort[[year-29]][1] * p.cancer) / sum(cohort[[year-29]][1]))
      cohort[[year-29+1]]  <- as.numeric(cohort[[year-29]] %*% tp.matrix)
    }
    cohort.states[[strategy]] <- cohort


    df <- rbind(df, data.frame(strategy=strategy,
                      C=mean(costs),
                      E=sum(utilities)))

    cancer.incidence <- colMeans(matrix(cancer.incidence, nrow=5))
    incidence[[strategy]] <- cancer.incidence
  }
  return(list(
    summary=df,
    incidence=incidence,
    cohort.info=cohort.states
  ))
}
