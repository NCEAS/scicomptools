# Run all tests in this script:
## testthat::test_file(file.path("tests", "testthat", "test-stat_extract.R"))

# Error testing
test_that("Only accepts correct inputs", {
  expect_error(stat_extract())
  expect_error(stat_extract(mod_fit = 1:10))
  expect_error(stat_extract(mod_fit = c("a", "b", "c")))
})

# Output testing
test_that("Outputs are correct", {
  
  # t-Test
  t_test <- t.test(1:10, y = c(7:20))
  expect_equal(stat_extract(mod_fit = t_test)$Estimate[1], 5.5)
  
  # Linear Model
  x <- c(5.6, 4.3, 2.3, 7.8, 10.3)
  y <- c(1.1, 4.2, 8.0, 3.8, 9.9)
  linear_model = lm(y~x)
  expect_equal(round(stat_extract(mod_fit = linear_model)$Estimate[1]), 4)
  
  # Nonlinear Least Squares
  set.seed(1)
  x <- 1:10
  y <- 2*x + 3                            
  yeps <- y + rnorm(length(y), sd = 0.01) 
  nonlinear_least_squares <- nls(yeps ~ a + b*x, start = list(a = 0.12345, b = 0.54321))
  expect_equal(round(stat_extract(mod_fit = nonlinear_least_squares)$Estimate[1]), 3)
  
  # # Linear Mixed-Effects Model
  # data("sleepstudy", package = "lme4")
  # linear_mixed <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
  # expect_equal(round(stat_extract(mod_fit = linear_mixed)$Estimate[1]), 251)
  
  data("Orthodont", package = "nlme")
  linear_mixed_2 <- nlme::lme(distance ~ age, data = Orthodont)
  expect_equal(round(stat_extract(mod_fit = linear_mixed_2)$Value[1]), 17)
  
  # Multiple Regression on distance Matrices (MRM)
  data("graze", package = "ecodist")
  mrm_model <- ecodist::MRM(dist(LOAR10) ~ dist(sitelocation) + dist(forestpct), data=graze, nperm=10)
  expect_equal(round(stat_extract(mod_fit = mrm_model)$Response[1]), 7)
  
  # Trajectory Analysis
  data("Pupfish", package = "RRPP")
  fit <- RRPP::lm.rrpp(coords ~ Pop * Sex, data = Pupfish, iter = 199)
  traj_analysis <- RRPP::trajectory.analysis(fit, groups = Pupfish$Pop, 
                            traj.pts = Pupfish$Sex, print.progress = FALSE)
  expect_equal(round(stat_extract(mod_fit = traj_analysis)$Z_Score[1]), 2)
})