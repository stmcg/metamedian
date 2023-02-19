# Augmenting dat.age with fake data to check scenarios S1 and S3
set.seed(1234)
n <- nrow(dat.age)
min.g1 <- runif(n, min = 25, max = min(dat.age$q1.g1, na.rm = TRUE))
min.g2 <- runif(n, min = 25, max = min(dat.age$q1.g2, na.rm = TRUE))
max.g1 <- runif(n, min = max(dat.age$q3.g1, na.rm = TRUE), max = 100)
max.g2 <- runif(n, min = max(dat.age$q3.g2, na.rm = TRUE), max = 100)
dat.age_S1 <- dat.age_S3 <- dat.age
dat.age_S1[, c('q1.g1', 'q3.g1', 'q1.g2', 'q3.g2')] <- NA
dat.age_S1[, c('min.g1', 'max.g1', 'min.g2', 'max.g2')] <-
  dat.age_S3[, c('min.g1', 'max.g1', 'min.g2', 'max.g2')] <-
  cbind(min.g1, max.g1, min.g2, max.g2)

# Checking all approaches with the naive SE estimator
mean_methods <- c('wan', 'luo', 'shi_normal', 'shi_lognormal', 'qe', 'bc', 'mln', 'yang')
sd_methods <- c('wan', 'shi_normal', 'shi_lognormal', 'qe', 'bc', 'mln', 'yang')
for (mean_method in mean_methods){
  for (sd_method in sd_methods){
    test_that(paste0(mean_method, " mean method with the naive SE estimator (with ", sd_method, " standard deviation method) in scenario S1 works"), {
      expect_no_error(metamean(dat.age_S1, mean_method = mean_method, se_method = 'naive', sd_method = sd_method))
    })
    test_that(paste0(mean_method, " mean method with the naive SE estimator (with ", sd_method, " standard deviation method) in scenario S2 works"), {
      expect_no_error(metamean(dat.age, mean_method = mean_method, se_method = 'naive', sd_method = sd_method))
    })
    test_that(paste0(mean_method, " mean method with the naive SE estimator (with ", sd_method, " standard deviation method) in scenario S3 works"), {
      expect_no_error(metamean(dat.age_S3, mean_method = mean_method, se_method = 'naive', sd_method = sd_method))
    })
  }
}

# Checking all approaches with the bootstrap SE estimator
set.seed(1234)
mean_methods_boot <- c('qe', 'bc', 'mln')
for (mean_method in mean_methods_boot){
  test_that(paste0(mean_method, " mean method with the bootstrap SE estimator in scenario S1 works"), {
    expect_no_error(metamean(dat.age_S1, mean_method = mean_method, se_method = 'bootstrap', nboot = 10))
  })
  test_that(paste0(mean_method, " mean method with the bootstrap SE estimator in scenario S2 works"), {
    expect_no_error(metamean(dat.age, mean_method = mean_method, se_method = 'bootstrap', nboot = 10))
  })
  test_that(paste0(mean_method, " mean method with the bootstrap SE estimator in scenario S3 works"), {
    expect_no_error(metamean(dat.age_S3, mean_method = mean_method, se_method = 'bootstrap', nboot = 10))
  })
}

# Checking the approach with the plugin SE estimator
test_that("yang mean method with the plug-in SE estimator in scenario S1 works", {
  expect_no_error(metamean(dat.age_S1, mean_method = 'yang', se_method = 'plugin'))
})
test_that("yang mean method with the plug-in SE estimator in scenario S2 works", {
  expect_no_error(metamean(dat.age, mean_method = 'yang', se_method = 'plugin'))
})
test_that("yang mean method with the plug-in SE estimator in scenario S3 works", {
  expect_no_error(metamean(dat.age_S3, mean_method = 'yang', se_method = 'plugin'))
})
