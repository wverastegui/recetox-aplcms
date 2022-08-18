patrick::with_parameters_test_that(
  "basic unsupervised test",
  {
    ######## Handle clusters ###########
    # CRAN limits the number of cores available to packages to 2
    # source https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions#50571533
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      cluster <- 2L
    } else {
      # use all cores in devtools::test()
      cluster <- parallel::detectCores()
    }

    if (!is(cluster, "cluster")) {
      cluster <- parallel::makeCluster(cluster)
      on.exit(parallel::stopCluster(cluster))
    }
    #######
    
    # set actual and expected entries
    testdata <- file.path("..", "testdata")

    filenames <- lapply(files, function(x) {
      file.path(testdata, "input", paste0(x, ".mzML"))
    })
    filenames <- unlist(filenames)

    actual <- unsupervised(
      filenames,
      min_exp = 2,
      min_pres = min_pres,
      min_run = min_run,
      mz_tol = mz_tol,
      baseline_correct = 0,
      baseline_correct_noise_percentile = 0.05,
      shape_model = "bi-Gaussian",
      BIC_factor = 2,
      peak_estim_method = "moment",
      min_bandwidth = min_bandwidth,
      max_bandwidth = max_bandwidth,
      sd_cut = sd_cut,
      sigma_ratio_lim = sigma_ratio_lim,
      component_eliminate = 0.01,
      moment_power = 1,
      max_align_mz_diff = max_align_mz_diff,
      recover_mz_range = recover_mz_range,
      recover_chr_range = recover_chr_range,
      use_observed_range = use_observed_range,
      recover_min_count = recover_min_count,
      intensity_weighted = intensity_weighted,
      cluster = cluster
    )
    
    aligned_feature_sample_table_expected <- arrow::read_parquet(file.path(testdata, "recovered", "aligned_feature_sample_table.parquet"))
    recovered_feature_sample_table_expected <- arrow::read_parquet(file.path(testdata, "recovered", "recovered_feature_sample_table.parquet"))

    browser()

    expect_equal(actual$aligned_feature_sample_table, aligned_feature_sample_table_expected)
    expect_equal(actual$recovered_feature_sample_table, recovered_feature_sample_table_expected)
  },
  patrick::cases(
    RCX_shortened = list(
      files = c("RCX_06_shortened", "RCX_07_shortened", "RCX_08_shortened"),
      mz_tol = 1e-05,
      min_pres = 0.5,
      min_run = 12,
      intensity_weighted = FALSE,
      sd_cut = c(0.01, 500),
      sigma_ratio_lim = c(0.01, 100),
      max_align_mz_diff = 0.01,
      recover_mz_range = NA,
      recover_chr_range = NA,
      use_observed_range = TRUE,
      min_bandwidth = NA,
      max_bandwidth = NA,
      recover_min_count = 3
    )
  )
) 




# test_that("basic unsupervised test", {
#   test_files <- c('../testdata/mbr_test0.mzml',
#                   '../testdata/mbr_test1.mzml',
#                   '../testdata/mbr_test2.mzml')
  
#   expected <- arrow::read_parquet('../testdata/unsupervised_recovered_feature_sample_table.parquet')
  
#   # CRAN limits the number of cores available to packages to 2
#   # source https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions#50571533
#   chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  
#   if (nzchar(chk) && chk == "TRUE") {
#     # use 2 cores in CRAN/Travis/AppVeyor
#     num_workers <- 2L
#   } else {
#     # use all cores in devtools::test()
#     num_workers <- parallel::detectCores()
#   }
  
#   result <- unsupervised(test_files, cluster = num_workers)

#   expect_equal(result$recovered_feature_sample_table, expected)
# })

