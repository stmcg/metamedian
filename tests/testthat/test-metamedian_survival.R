# Checking all methods for one-group studies
estimands <- c('median_g1', 'median_g2', 'difference_median', 'ratio_median')
for (estimand in estimands){
  test_that(paste0(estimand, " estimand for metamedian_survival works"), {
    expect_no_error(metamedian_survival(dat.lung, estimand = estimand))
  })
}
