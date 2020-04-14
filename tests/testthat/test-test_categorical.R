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

test_that("OR and p-value computation appropriate for covariates of class
           factor when referring to 1st level in computations", {
    test <- suppressWarnings(
        test_categorical(data = demtbl_dat_2,
                         predictor = colnames(demtbl_dat_2)[c(2,4,10)],
                         response = "outcome",
                         format = FALSE)
        )

    # 1st Row
    tab1 <- table(demtbl_dat_2$gender,
                  demtbl_dat_2$outcome)
    OR1 <- round(fisher.test(tab1)$estimate, 2)
    fp1 <- fisher.test(tab1)$p.value
    pp1 <- chisq.test(tab1)$p.value

    # 2nd Row
    tab2 <- table(demtbl_dat_2$rf_1, demtbl_dat_2$outcome)
    OR2 <- suppressWarnings(round(fisher.test(tab2)$estimate, 2))
    fp2 <- suppressWarnings(fisher.test(tab2)$p.value)
    pp2 <- suppressWarnings(chisq.test(tab2)$p.value)

    # 3rd Row
    idx <- demtbl_dat_2$insurance %in% c("Uninsured", "Public")
    tmp_dat <- demtbl_dat_2[idx,]
    tmp_dat$insurance <- droplevels(tmp_dat$insurance)
    tab3 <- table(tmp_dat$insurance, tmp_dat$outcome)
    OR3 <- suppressWarnings(round(fisher.test(tab3)$estimate, 2))
    fp3 <- suppressWarnings(fisher.test(tab3)$p.value)
    pp3 <- suppressWarnings(chisq.test(tab3)$p.value)

    # 4th Row
    idx <- demtbl_dat_2$insurance %in% c("Uninsured", "Private")
    tmp_dat <- demtbl_dat_2[idx,]
    tmp_dat$insurance <- droplevels(tmp_dat$insurance)
    tab4 <- table(tmp_dat$insurance, tmp_dat$outcome)
    OR4 <- suppressWarnings(round(fisher.test(tab4)$estimate,  2))
    fp4 <- suppressWarnings(fisher.test(tab4)$p.value)
    pp4 <- suppressWarnings(chisq.test(tab4)$p.value)

    # Test odds ratios
    expect_equivalent(round(test[1, "OR"], 2), OR1)
    expect_equivalent(round(test[2, "OR"], 2), OR2)
    expect_equivalent(round(test[3, "OR"], 2), OR3)
    expect_equivalent(round(test[4, "OR"], 2), OR4)

    # Test fisher p-values
    expect_equivalent(test[1, "fisher_p"], fp1)
    expect_equivalent(test[2, "fisher_p"], fp2)
    expect_equivalent(test[3, "fisher_p"], fp3)
    expect_equivalent(test[4, "fisher_p"], fp4)

    # Test pearson p-values
    expect_equivalent(test[1, "pearson_p"], pp1)
    expect_equivalent(test[2, "pearson_p"], pp2)
    expect_equivalent(test[3, "pearson_p"], pp3)
    expect_equivalent(test[4, "pearson_p"], pp4)
})





