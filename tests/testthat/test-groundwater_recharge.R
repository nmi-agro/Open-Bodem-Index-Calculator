test_that("whether ind_gw_recharge works", {
  expect_equal(
    ind_gw_recharge(
      B_LU_BRP = c(265,265,265,233,233,233),
      D_PSP = c(100,200,300,400,500,600),
      D_WRI_K = c(50,90,50,100,50,90),
      B_DRAIN = c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE),
      B_GWL_GHG = c(20, 20, 90, 60, 20, 90),
      B_SC_WENR = rep(c('Zeer groot', 'Zeer beperkt'), 3),
      D_SE = c(1, 1, 1, 6.049445, 6.049445, 6.049445)
    ),
    expected = c(0.190, 0.585, 0.4242104, 0.9628, 0.505, 0.9557414),
    tolerance = 0.01
  )

})

# helper to compare drained and undrained scores for a set of groundwater inputs
gwr_drain_test <- function(B_DRAIN, B_GWL_GHG = NULL, B_GWL_CLASS = NULL){
  n <- max(length(B_GWL_GHG), length(B_GWL_CLASS))
  ind_gw_recharge(
    B_LU_BRP = rep(265, n),
    D_PSP = rep(300, n),
    D_WRI_K = rep(50, n),
    B_DRAIN = rep(B_DRAIN, n),
    B_GWL_GHG = B_GWL_GHG,
    B_GWL_CLASS = B_GWL_CLASS,
    B_SC_WENR = rep('Zeer beperkt', n),
    D_SE = rep(1, n)
  )
}

test_that("B_DRAIN affects scores regardless of B_GWL_GHG", {
  ghg <- c(0, 50, 80, 81, 120, 250)
  # drainage reduces the score at any GHG
  expect_all_true(
    gwr_drain_test(TRUE, B_GWL_GHG = ghg) < gwr_drain_test(FALSE, B_GWL_GHG = ghg)
  )
  # GHG itself does not affect the score
  expect_equal(length(unique(gwr_drain_test(TRUE, B_GWL_GHG = ghg))), 1)
  expect_equal(length(unique(gwr_drain_test(FALSE, B_GWL_GHG = ghg))), 1)
})

test_that("deprecated B_GWL_CLASS still works and gives a warning", {
  expect_warning(
    gwr_drain_test(TRUE, B_GWL_CLASS = 'IIIb'),
    regexp = 'deprecated'
  )

  # all classes are affected by drainage
  cl.all <- c("I", "Ia", "Ic", "II", "IIa", "IIb", "IIc", "III", "IIIa", "IIIb",
              "IV", "IVu", "IVc", "V", "sV", "sVb", "Va", "Vao", "Vad",
              "Vb", "Vbo", "Vbd", "VI", "VIo", "VId",
              "VII", "VIIo", "VIId", "VIII", "VIIIo", "VIIId")
  expect_all_true(
    suppressWarnings(gwr_drain_test(TRUE, B_GWL_CLASS = cl.all)) <
      suppressWarnings(gwr_drain_test(FALSE, B_GWL_CLASS = cl.all))
  )
})

test_that("B_GWL_GHG is used when both B_GWL_GHG and B_GWL_CLASS are provided", {
  expect_warning(
    gwr_drain_test(TRUE, B_GWL_GHG = 40, B_GWL_CLASS = 'VIII'),
    regexp = 'only B_GWL_GHG will be used'
  )
})

test_that("an error is given when both B_GWL_GHG and B_GWL_CLASS are missing", {
  expect_error(
    ind_gw_recharge(B_LU_BRP = 265, D_PSP = 300, D_WRI_K = 50, B_DRAIN = TRUE,
                    B_SC_WENR = 'Zeer beperkt', D_SE = 1),
    regexp = 'B_GWL_GHG'
  )
  expect_error(gwr_drain_test(TRUE, B_GWL_GHG = -10))
  expect_error(suppressWarnings(gwr_drain_test(TRUE, B_GWL_CLASS = 'X')))
})

test_that("est_gwl_ghg_from_class gives GHG <= 80 for classes I to VId except IVc", {
  cl.low <- c("I", "Ia", "Ic", "II", "IIa", "IIb", "IIc", "III", "IIIa", "IIIb",
              "IV", "IVu", "V", "sV", "sVb", "Va", "Vao", "Vad",
              "Vb", "Vbo", "Vbd", "VI", "VIo", "VId")
  cl.high <- c("IVc", "VII", "VIIo", "VIId", "VIII", "VIIIo", "VIIId")
  expect_all_true(est_gwl_ghg_from_class(cl.low) <= 80)
  expect_all_true(est_gwl_ghg_from_class(cl.high) > 80)
  expect_length(est_gwl_ghg_from_class(c(cl.low, cl.high)), 31)
  expect_error(est_gwl_ghg_from_class('X'))
})

test_that("Groundwater recharge is correctly modified with a target by ind_gw_target", {
  dt <- data.table(
    D_RISK_GWR = c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7),
    B_SOILTYPE_AGR = c('veen',
                       'zeeklei', 'zeeklei', 'zeeklei', 'zeeklei',
                       'loess', 'loess'),
    B_GWL_CLASS = c('IIa',
                    'IIa', 'Va', 'Vb', 'IVu',
                    'IIa', 'VIIIo'),
    B_AREA_DROUGHT = c(FALSE,
                       FALSE, FALSE, FALSE, FALSE,
                       TRUE, TRUE)
  )
  
  dt[,S_H_GWR := ind_gw_target(D_RISK_GWR, B_SOILTYPE_AGR, B_GWL_CLASS, B_AREA_DROUGHT = B_AREA_DROUGHT)]
  
  expect_gt(dt[B_SOILTYPE_AGR == 'veen' & B_GWL_CLASS == 'IIa', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'IIa', S_H_GWR])
  expect_gt(dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'IIa', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'Va', S_H_GWR])
  expect_gt(dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'Va', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'Vb', S_H_GWR])
  expect_gt(dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'Vb', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'IVu', S_H_GWR])
  expect_gt(dt[B_SOILTYPE_AGR == 'zeeklei' & B_GWL_CLASS == 'IIa' & B_AREA_DROUGHT == FALSE, S_H_GWR],
            dt[B_SOILTYPE_AGR == 'loess' & B_GWL_CLASS == 'IIa' & B_AREA_DROUGHT == TRUE, S_H_GWR])
  expect_gt(dt[B_SOILTYPE_AGR == 'loess' & B_GWL_CLASS == 'IIa', S_H_GWR], 
            dt[B_SOILTYPE_AGR == 'loess' & B_GWL_CLASS == 'VIIIo', S_H_GWR])

})

test_that('A warning is thrown when deprecated argument I_P_SE is used in ind_gw_recharge',{
  expect_warning(
    ind_gw_recharge(
      B_LU_BRP = 265,
      D_PSP = 400,
      D_WRI_K = 50,
      I_P_SE = 0.8,
      B_DRAIN = TRUE, 
      B_GWL_GHG = 30,
      B_SC_WENR = 'Matig'
    )
  )
})

test_that('A warning is thrown when deprecated argument I_P_CO is used in ind_gw_recharge',{
  expect_warning(
    ind_gw_recharge(
      B_LU_BRP = 265,
      D_PSP = 400,
      D_WRI_K = 50,
      I_P_CO = 0.1,
      B_DRAIN = TRUE, 
      B_GWL_GHG = 30,
      D_SE = 25
    )
  )
})

test_that('A warning is thrown when both D_SE and I_P_SE are supplied to ind_gw_recharge',{
  expect_warning(
    ind_gw_recharge(
      B_LU_BRP = 265,
      D_PSP = 400,
      D_WRI_K = 50,
      B_DRAIN = TRUE,
      I_P_SE = 0.8,
      B_GWL_GHG = 30,
      B_SC_WENR = 'Matig',
      D_SE = 25
    )
  )
})

test_that('A warning is thrown when both I_P_CO and B_SC_WENR or D_P_CO are suppolied to ind_gw_recharge',{
  expect_warning(
    ind_gw_recharge(
      B_LU_BRP = 265,
      D_PSP = 400,
      D_WRI_K = 50,
      B_DRAIN = TRUE,
      I_P_CO = 0.1,
      B_GWL_GHG = 30,
      B_SC_WENR = 'Matig',
      D_SE = 25
    )
  )
  
  expect_warning(
    ind_gw_recharge(
      B_LU_BRP = 265,
      D_PSP = 400,
      D_WRI_K = 50,
      B_DRAIN = TRUE,
      I_P_CO = 0.1,
      B_GWL_GHG = 30,
      D_SE = 25,
      D_P_CO = 0.5
    )
  )
})


test_that('D_P_CO is favoured over B_SC_WENR',{
  expect_gt(
    object = 
      ind_gw_recharge(
        B_LU_BRP = 2014,
        D_PSP = 400,
        D_WRI_K = 50,
        B_DRAIN = FALSE, # compaction is only corrected for on undrained fields
        B_GWL_GHG = 100,
        B_SC_WENR = 'Groot',
        D_SE = 25,
        D_P_CO = 1 # this should be used instead of B_SC_WENR
      ),
    expected = 
      ind_gw_recharge(
        B_LU_BRP = 2014,
        D_PSP = 400,
        D_WRI_K = 50,
        B_DRAIN = FALSE,
        B_GWL_GHG = 100,
        B_SC_WENR = 'Groot',
        D_SE = 25
      )
  )
})

test_that('D_SE is used instead of I_P_SE when both are provided and a warning is given',{
  expect_warning(
    ind_gw_recharge(
      B_LU_BRP = 2014,
      D_PSP = 400,
      D_WRI_K = 50,
      B_DRAIN = TRUE,
      B_GWL_GHG = 60,
      B_SC_WENR = 'Groot',
      D_SE = 5,
      I_P_SE = 0.95
    )
  )
  
  suppressWarnings(
  expect_gt(
    object = 
      ind_gw_recharge(
        B_LU_BRP = 2014,
        D_PSP = 400,
        D_WRI_K = 50,
        B_DRAIN = TRUE,
        B_GWL_GHG = 60,
        B_SC_WENR = 'Groot',
        D_SE = 25,
        I_P_SE = 0.15
      ),
    expected = 
      ind_gw_recharge(
        B_LU_BRP = 2014,
        D_PSP = 400,
        D_WRI_K = 50,
        B_DRAIN = TRUE,
        B_GWL_GHG = 60,
        B_SC_WENR = 'Groot',
        D_SE = 2,
        I_P_SE = 0.95
      )
  )
  )
})
