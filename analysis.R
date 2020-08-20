library(here)
source(file.path(here(), "functions.R"))

# Used for setting seeds in parallel computing contexts
set.seed(1, kind = "L'Ecuyer-CMRG")

# testing out target collection
test <- PacksToCompletion(T, F, F, T, F, T, "ashes",
                          target = c(common = 5, rare = 2,
                                     epic = 1, legendary = 1))





# New, take advantage of parallel
startTime <- proc.time()
a <- pmap(list(nrRuns = c(250, 250, 250),
               useDust = c(T, T, T),
               keepGold = c(F, F, F),
               packDupeProtect = c(F, T, F),
               guaranteeLegend = c(F, T, T),
               legendDupeProtect = c(F, T, F),
               allDupeProtect = c(F, F, T)),
          RunSimulation)
endTime <- proc.time() - startTime

packTotal <- a %>% flatten() %>% map(1) %>% bind_rows()
dustTotal <- a %>% flatten() %>% map(2) %>% bind_rows()

a2summ <-
    packTotal %>%
    group_by_at(vars(-matches("run|nrPacks"))) %>%
    summarise(n = n(),
              mean = mean(nrPacks),
              sd = sd(nrPacks),
              se = sd / sqrt(n),
              moe = qt(0.975, df = n - 1) * se,
              confintu = mean + moe,
              confintl = mean - moe)

# qt(0.995, df = n - 1) is the t-statistic (because we don't know the true population std. dev.)
# Because we don't know the true pop. std. dev., cannot use z-statistic
# See khan academy
# Degrees of freedom is sample size - 1
# 99% confidence interval, leaving 0.05% unfilled at both ends (so 0.005, and .995)
# 95% CI, leaving 0.025% unfilled at both ends (so 0.025, and 0.975)
# To find confidence interval: mean +- t-statistic * (sd / sqrt(n))
# (sd / sqrt(n)) is the standard error of the mean
# t-statistic is your confidence interval (95%, 99%, whatever) and the degree of freedom (sample - 1)
# t-statistic * standard error of the mean is your margin of error
# you can use a z-statistic for estimating a sample size given a margin of error and desired confidence interval
# you would just assume your sample std. dev. is the true pop. std. dev., this should really reinforce it's an estimate
# n = (z-statistic * sample std. dev. (assumed pop.) / margin of error) ^ 2
    
aNorm <- a2$nrPacks %>% filter(useDust == T, keepGold == T)
ggplot(aNorm, aes(x = nrPacks)) + geom_histogram(bins = 40) + theme_bw()

file.remove("log.txt")
