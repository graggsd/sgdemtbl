test_that("output df has appropriate classes", {
    test <- suppressWarnings(
        test_categorical(data = demtbl_dat_2,
                             predictor = colnames(demtbl_dat_2)[2:5],
                             response = "outcome")
    )
    # char_cols <- colnames(test) %in% c("name", "value", "pref_test")
    num_cols <- colnames(test) %in% c("OR", "CI_lower", "CI_upper", "pearson_p",
                  "fisher_CI_lower", "fisher_CI_upper", "fisher_p")
    col_classes <- unlist(lapply(test, class))
    expect_true(all(col_classes[num_cols] == "numeric"))
})

test_that("OR computation appropriate for covariates of class factor when
          ref 1st level", {
    test <- suppressWarnings(
        test_categorical(data = demtbl_dat_2,
                         predictor = colnames(demtbl_dat_2)[c(2,4,10)],
                         response = "outcome",
                         format = FALSE)
    )
    test <- round(test[, "OR"], 2)

    OR1 <-
        round(fisher.test(table(
            demtbl_dat_2$gender, demtbl_dat_2$outcome
        ))$estimate,
        2)

    OR2 <-
        round(fisher.test(table(demtbl_dat_2$rf_1,
                                demtbl_dat_2$outcome))$estimate,
              2)

    idx <- demtbl_dat_2$insurance %in% c("Uninsured", "Public")

    tmp_dat <- demtbl_dat_2[idx,]
    tmp_dat$insurance <- droplevels(tmp_dat$insurance)

    OR3 <- round(fisher.test(table(tmp_dat$insurance,
                                   tmp_dat$outcome))$estimate,
                 2)

    idx <- demtbl_dat_2$insurance %in% c("Uninsured", "Private")


    tmp_dat <- demtbl_dat_2[idx,]
    tmp_dat$insurance <- droplevels(tmp_dat$insurance)

    OR4 <- round(fisher.test(table(tmp_dat$insurance,
                                   tmp_dat$outcome))$estimate,
                 2)
    expect_equivalent(test[1], OR1)
    expect_equivalent(test[2], OR2)
    expect_equivalent(test[3], OR3)
    expect_equivalent(test[4], OR4)
})





