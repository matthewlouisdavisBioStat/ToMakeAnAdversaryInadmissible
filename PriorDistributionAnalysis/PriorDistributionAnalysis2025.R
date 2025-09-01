## Load data from previous years
r1 <- read.delim("query_2017.txt")
r2 <- read.delim("query_2018.txt")
r3 <- read.delim("query_2019.txt")
r4 <- read.delim("query_2020.txt")
r <- as.data.frame(rbind(r1,r2,r3,r4),
                   stringsAsFactor = F)
std <- function(x)(x-mean(x))/sd(x)


## Construct predictions and response from home team perspective
r$total_points <- rowSums(cbind(as.numeric(r$points),
                                as.numeric(r$o.points)))
r <- na.omit(r)
r <- r[r$site == "home ",]
r$predict_total <- (as.numeric(r$total))
r$actual_total <- (as.numeric(r$total_points))
r$predict_spread <- -as.numeric(r$line)
r$actual_spread <- rowSums(cbind(as.numeric(r$points),
                                 -as.numeric(r$o.points)))
predict_hometeam <- as.numeric(r$predict_spread + r$predict_total)/2
actual_hometeam <- as.numeric(r$actual_spread + r$actual_total)/2
predict_prop <- predict_hometeam/r$predict_total
actual_prop <- actual_hometeam/r$actual_total
r$predict_asinsqrtprop <- asin(sqrt((predict_hometeam + 3/8)/(r$predict_total + 3/4)))
r$actual_asinsqrtprop <- asin(sqrt((actual_hometeam + 3/8)/(r$actual_total + 3/4)))
r$predict_log_total <- log(r$predict_total)
r$actual_log_total <- log(r$actual_total)


## Final results
with(r, qqnorm(std(predict_total - actual_total)))
abline(0, 1)
with(r, sd(predict_total - actual_total))/48.33594*48
# [1] 17.72642
with(r, qqnorm(std(predict_asinsqrtprop - actual_asinsqrtprop)))
abline(0, 1)
with(r, sd(predict_asinsqrtprop - actual_asinsqrtprop))
# [1] 0.05738556
with(r, qqnorm(std(predict_log_total - actual_log_total)))
abline(0, 1)
with(r, sd(predict_log_total - actual_log_total))
# [1] 0.08112754
with(r, qqnorm(std(predict_spread- actual_spread)))
abline(0, 1)
with(r, sd(predict_spread - actual_spread))
# [1] 12.39497