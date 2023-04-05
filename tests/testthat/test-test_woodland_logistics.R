test_that("woodland logistics works", {

 wood.thresh <- 100 ### available nursery stocks for planting
 wood.income <- c(70, -40, 35, 20)


 ### does woodland planting exceed availability or nursery stocks?
 if(sum(wood.income[which(wood.income > 0)]) > wood.thresh) {

  ### scale woodland planting by availability of nursery stocks
  wood.income <- ifelse(wood.income > 0, wood.income / (sum(wood.income[which(wood.income > 0)] / wood.thresh)), wood.income)

 }

expect_equal(wood.income, c(70*(100/125), -40.0, 35*(100/125), 20*(100/125)))


})
