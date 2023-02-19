# Checking all approaches with the naive SE estimator
mean_methods <- c('wan', 'luo', 'shi_normal', 'shi_lognormal', 'qe', 'bc', 'mln', 'yang')
sd_methods <- c('wan', 'shi_normal', 'shi_lognormal', 'qe', 'bc', 'mln', 'yang')
for (mean_method in mean_methods){
  for (sd_method in sd_methods){
    test_that(paste0(mean_method, " mean method with the naive SE estimator (with ", sd_method, " standard deviation method) works"), {
      expect_no_error(metamean(dat.age, mean_method = mean_method, se_method = 'naive', sd_method = sd_method))
    })
  }
}

# Checking all approaches with the bootstrap SE estimator
set.seed(1234)
mean_methods_boot <- c('qe', 'bc', 'mln')
for (mean_method in mean_methods_boot){
  test_that(paste0(mean_method, " mean method with the bootstrap SE estimator works"), {
    expect_no_error(metamean(dat.age, mean_method = mean_method, se_method = 'bootstrap', nboot = 10))
  })
}

# Checking the approach with the plugin SE estimator
test_that("yang mean method with the bootstrap SE estimator works", {
  expect_no_error(metamean(dat.age, mean_method = 'yang', se_method = 'plugin'))
})
