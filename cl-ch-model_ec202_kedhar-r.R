library(ggplot2)
years <- seq(0:30)
population.early <- c(1)
num.events.early <- c()
growth.rate.early <- c()
for (i in 1:30) {
    event.count <- rpois(1, 0.3)
    if (event.count == 0) {
        growth.rate <- 1.3
    } else if (event.count == 1) {
        growth.rate <- 0.9
    } else {
        growth.rate <- 0.5
    }
    population.early <- append(population.early, population.early[length(population.early)]*growth.rate)
    num.events.early <- append(num.events.early, event.count)
    growth.rate.early <- append(growth.rate.early, growth.rate)
}
data.early <- data.frame(years = years[1:30], population = population.early[-1], num.events = num.events.early, growth.rate = growth.rate.early)
head(data.early)
mean(growth.rate.early[1:10])
exp(mean(log(growth.rate.early[1:10])))
population.late <- c(1)
num.events.late <- c()
growth.rate.late <- c()
for (i in 1:30) {
    event.count <- rpois(1, 1)
    if (event.count == 0) {
        growth.rate <- 1.3
    } else if (event.count == 1) {
        growth.rate <- 0.9
    } else {
        growth.rate <- 0.5
    }
    population.late <- append(population.late, population.late[length(population.late)]*growth.rate)
    num.events.late <- append(num.events.late, event.count)
    growth.rate.late <- append(growth.rate.late, growth.rate)
}
data.late <- data.frame(years = years[1:30], population = population.late[-1], num.events = num.events.late, growth.rate = growth.rate.late)
head(data.late)
mean(growth.rate.late[1:10])
exp(mean(log(growth.rate.late[1:10])))

par(mfrow = c(1, 3))
plt1 <- ggplot(data = data.late, aes(x = years, y = log(population))) + geom_line() + xlab("Time") + ylab("Log of population") + geom_abline(intercept = 0, slope = log(exp(mean(log(growth.rate.late[1:10])))), color = "red") + geom_abline(intercept = 0, slope = log(mean(growth.rate.late[1:10])), color = "blue")
plt2 <- ggplot(data = data.late, aes(x = years, y = num.events)) + geom_line() + xlab("Time") + ylab("Number of heatwaves")
plt3 <- ggplot(data = data.late, aes(x = years, y = growth.rate)) + geom_line() + xlab("Time") + ylab("Growth rate")
ggarrange(plt1, plt2, plt3, labels = c("A", "B", "C"), ncol = 3, nrow = 1)
