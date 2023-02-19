# Checking all methods for two-group studies
median_methods <- c('mm', 'wm', 'qe')
for (median_method in median_methods){
  test_that(paste0(median_method, " median method for two-group studies works"), {
    expect_no_error(metamedian(dat.age, median_method = median_method))
  })
}

# Checking all methods for one-group studies
median_methods_onegroup <- c('mm', 'wm', 'qe', 'cd')
for (median_method in median_methods_onegroup){
  test_that(paste0(median_method, " median method for one-group studies works"), {
    expect_no_error(metamedian(dat.age[, 1:7], median_method = median_method))
  })
}
