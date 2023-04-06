test_that("land allocation works", {

  r.names  <- c('s_cereals', 's_farm_woodland')
  economic <- setNames(data.frame(matrix(nrow = 2, ncol = 2)), r.names)
  rownames(economic) <- colnames(economic)

  ### arbitrary economic transition
  economic[1, 2] <- 5
  economic[2, 1] <- -5
  economic[2, 2] <- 0
  economic[1, 1] <- 0

  behavioural <- list(willingness = economic,
                      CDR_max     = economic,
                      tig         = economic)


  behavioural$willingness[1, 2] <- 0.9
  behavioural$CDR_max[1, 2]     <- 4
  behavioural$tig[1, 2]         <- 0.75

  logistics     <- economic

  CDR      <- c(FALSE, TRUE)
  LULC     <- economic

  ### Isolate cases of non-CDR to CDR LUC
  for(i in 1:ncol(LULC)) {

    if(CDR[i] == TRUE) {

      for(j in 1:nrow(economic)) {

        if(CDR[j] == FALSE) {

          ### CDR-specific willingness
          LULC[j, i] <- LULC[j, i] * behavioural$willingness[j, i]
          LULC[i, j] <- 0 - LULC[j, i]

          expect_equal(LULC[j, i], 5 * 0.9)

          ### Overall willingness
          LULC[j, i] <- ifelse(LULC[j, i] > behavioural$CDR_max[j, i],behavioural$CDR_max[j, i], LULC[j, i])
          LULC[i, j] <- 0 - LULC[j, i]

          expect_equal(LULC[j, i], behavioural$CDR_max[j, i])

          ### Trust in govt
          LULC[j, i] <- LULC[j, i] * behavioural$tig[j, i]
          LULC[i, j] <- 0 - LULC[j, i]

          expect_equal(LULC[j, i], behavioural$CDR_max[j, i] * behavioural$tig[j, i])

          ### logistics (nursery stocks) - here adequate for demand
          LULC[j, i] <- ifelse(LULC[j, i] > logistics[j, i],logistics[j, i], LULC[j, i])
          LULC[i, j] <- 0 - LULC[j, i]

          expect_equal(LULC[j, i], behavioural$CDR_max[j, i] * behavioural$tig[j, i])

        }


      }

    }

  }

  expect_equal(sum(unlist(LULC)), 0)

})
