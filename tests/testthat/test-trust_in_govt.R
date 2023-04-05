test_that("trust in govt update works", {

TIG        <- 0
TIG.result <- list()


for(i in 1:30) {
### update based on constant increment, and the uptake of CDR options
tig.increment <- 0.2 + sum(c(0.998, 0.119, 0.018))
TIG           <- TIG + tig.increment
TIG.frac      <- TIG / (TIG + 40) ### taking the mean of the implied beta distribution

TIG.frac <- c(rep(TIG.frac, times = 3))

### downweight subsidies
TIG.frac <- 1-(c(200, 200, 200)/c(1412.5, 800, (200+1050+550))) * (1-TIG.frac)

TIG.result[[i]] <- TIG.frac

}

TIG.result <- unlist(lapply(TIG.result, mean))

expect_true(all(diff(TIG.result) > 0) & all(TIG.result < 1 & TIG.result > 0.5))

})
