context("Counting NAs for Categorical or Continuous data")

test_that("NAs counted for character cols", {

    miss_tbl_char <-
        make_miss_tbl(form = as.formula("~Household_Income+Participant_Type"),
                      data = data)

    expect_equivalent(c(19,26,25),
                 miss_tbl_char[is.na(rownames(miss_tbl_char)),])

})

test_that("NAs counted for factor cols", {

    test.dat <- data
    test.dat$Household_Income <- as.factor(test.dat$Household_Income)
    miss_tbl_factor <-
        make_miss_tbl(form = as.formula("~Household_Income+Participant_Type"),
                      data = test.dat)

    expect_equivalent(c(19,26,25),
                      miss_tbl_factor[is.na(rownames(miss_tbl_factor)),])

})

test_that("NAs counted for factor cols", {

    miss_tbl_cont <-
        make_miss_tbl(form = as.formula("~BMI+Participant_Type"),
                      data = data)

    expect_equivalent(c(22,27,22),
                      miss_tbl_cont[is.na(rownames(miss_tbl_cont)),])

})
