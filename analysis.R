library(here)
invisible(sapply(list.files(file.path(here(), "functions"), full.names = T), source))

# Used for setting seeds in parallel computing contexts
set.seed(1, kind = "L'Ecuyer-CMRG")

# testing out target collection
test <- PacksToCompletion(useDust = T,
                          keepGold = F,
                          packDupeProtect = T,
                          guaranteeLegend = T,
                          legendDupeProtect = F,
                          allDupeProtect = F,
                          onlyTarget = T,
                          setName = "ashes",
                          target = c(common = 0, rare = 6,
                                     epic = 6, legend = 4))

sim1 <- RunSimulation(nrRuns = 2,
                   useDust = T,
                   keepGold = F,
                   packDupeProtect = T,
                   guaranteeLegend = T,
                   legendDupeProtect = F,
                   allDupeProtect = F,
                   onlyTarget = F)
packTotal <- sim1 %>% map(1) %>% bind_rows()
dustTotal <- sim1 %>% map(2) %>% bind_rows()


test <- RunSimulation(nrRuns = 2,
                      useDust = T,
                      keepGold = F,
                      packDupeProtect = T,
                      guaranteeLegend = T,
                      legendDupeProtect = F,
                      allDupeProtect = F,
                      onlyTarget = F,
                      setLabels = c("classic", "gvg", "grandt"),
                      target = list(classic = c(common = 30,
                                                rare = 10,
                                                epic = 5,
                                                legend = 2),
                                    gvg = c(common = 20,
                                            rare = 7,
                                            epic = 3,
                                            legend = 1),
                                    grandt = c(common = 0,
                                               rare = 5,
                                               epic = 2,
                                               legend = 1)))

packTotal <- test %>% map(1) %>% bind_rows()
dustTotal <- test %>% map(2) %>% bind_rows()

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
