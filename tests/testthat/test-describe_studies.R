# Checking methods other than the CD method
test_that("describe_studies with a method other than 'cd' works", {
  expect_no_error(describe_studies(dat.age))
})

# Checking the CD method
test_that("describe_studies with method 'cd' method works", {
  expect_no_error(describe_studies(dat.age[, 1:7], method = 'cd'))
})
