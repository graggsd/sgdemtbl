context("Overall Output")

test_that("Output is same as first build.", {

    test_results_new <-
        strat_dem_tbl(data = data,
                      outcome = "Participant_Type",
                      continuous.covariates = c("BMI", "Age"),
                      categorical.covariates = c("Sex", "Education",
                                                 "Alcohol", "Smoking",
                                                 "Household_Income"),
                      continuous.tests = "kruskal.wallis.test",
                      categorical.tests = "likelihood.ratio.chi.square",
                      strata.col = "Race")

    expect_equivalent(test_results_new, test_results)

})
