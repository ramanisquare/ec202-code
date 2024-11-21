library(ggplot2)
library(ggpubr)

while (x > 0) {
    hi <- rnorm(25, mean = 1.1, sd = 0.3)
    if (exp(mean(log(vec))) < 1) {
         break
    }
}
mean(vec)
exp(mean(log(vec)))
growth.rates <- vec
pop <- c()
for (i in 1:25){
    pop <- append(pop, prod(growth.rates[1:i]))
}
populn <- data.frame(time = seq(0, 25), gr = c(NA, growth.rates), populn = c(1, pop))

par(mfrow = c(1,2))
plt1 <- ggplot(data = populn, aes(x = time, y = populn)) + geom_line() + xlab("Time") + ylab("Population")
plt2 <- ggplot(data = populn, aes(x = time, y = gr)) + geom_line() + xlab("Time") + ylab("Growth rate")
ggarrange(plt1, plt2, labels = c("A", "B"), ncol = 2, nrow = 1)
