test_that("income pressure works", {


 income        <- c(7, 3, 2.5, 15, 0) * c(100, 500, 100, 75, 0) * c(0.3, 0.15, 0.5, 0.3, 0) #yield * price * margin
 subsidy       <- c(100, 100, 200, 200, 200)
 carbon_income <- c(0, 0, 562.5, 0, 550.0)
 biodiversity_income <- c(1, 3, 525, 3, 1050)
 size                <- c(150, 25, mean(c(150, 25)), mean(c(150, 25)), mean(c(150, 25)))

 inc.per.ha      <- (income + subsidy + carbon_income+ biodiversity_income)
 income.per.farm <- (income + subsidy + carbon_income+ biodiversity_income) * size

 .income          <- list()
 .income.per.farm <- list()

 for(i in 1:length(inc.per.ha)) {

   .income[[i]]          <- inc.per.ha[i] / inc.per.ha
   .income.per.farm[[i]] <- income.per.farm[i] / income.per.farm

 }

 ### veg more profitable than cereals per ha, less profitable per farm
 ### peat restoration most profitable per ha
 expect(.income[[1]][2] < .income[[2]][1] & .income.per.farm[[2]][1] < .income.per.farm[[1]][2] &
          which.min(.income[[3]]) == 5, 'income pressure calculation incorrect')


})
