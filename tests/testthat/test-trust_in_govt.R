test_that("trust in govt update works", {

TIG <- 5

### update based on constant increment, and the uptake of CDR options
tig.increment <- 0.2 + sum(c(0.998, 0.119, 0.018))
TIG           <- TIG + tig.increment
TIG           <- TIG / (TIG + 40) ### taking the mean of the implied beta distribution

TIG <- c(rep(TIG, times = 3))

### downweight subsidies
 <- 1-(c(200, 200, 200)/c(1412.5, 203, (200+1050+550))) * (1-TIG)
df$tig[, i] <- ifelse(df$tig[, i] < 0 & !is.na(df$tig[, i]), 0, df$tig[, i])

})
