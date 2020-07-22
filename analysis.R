library(here)
source(file.path(here(), "functions.R"))

# The standard error of the sample mean is an estimate of how far the sample mean is likely to be from the population mean.
# Think of the standard error as describing the standard deviation of the test statistic in question.
# 
# Whereas the standard deviation of the sample is the degree to which individuals within the sample differ from the sample
# mean.
# 
# If the population standard deviation is finite, the standard error of the mean of the sample will tend to zero with
# increasing sample size because the estimate of the population mean will improve.
# 
# While the standard deviation of the sample will tend to approximate the population standard deviation as the sample size
# increases. 

a <- pmap(list(c(1, 1), c(T, T), c(T, F), c(T, F), c(T, F)), RunSimulation)
a2 <- pmap(a, function(...) {bind_rows(...)})
    
a3 <- bind_rows(a[[1]]["nrPacks"], a[[2]]["nrPacks"])
a4 <- bind_rows(a[[1]]["dustTotals"], a[[2]]["dustTotals"])

############################################################# See the pack logs
b1 <- PacksToCompletion(F, F, F, F, F, "ashes")
b1d <- PacksToCompletion(T, F, F, F, F, "ashes")

b2 <- PacksToCompletion(F, T, T, T, F, "ashes")
a2d <- b2 %>% mutate(id = row_number()) %>% filter_all(any_vars(. == "legend" | . == "goldl")) %>%
    mutate(diff = id - lag(id), mean = mean(diff, na.rm = T))
b2d <- PacksToCompletion(T, T, T, T, F, "ashes")

b3 <- PacksToCompletion(F, F, T, F, T, "ashes")
a3 <- b3 %>% mutate(id = row_number()) %>% filter_all(any_vars(. == "legend" | . == "goldl")) %>%
    mutate(diff = id - lag(id), mean = mean(diff, na.rm = T))
b3d <- PacksToCompletion(T, F, T, F, T, "ashes")

# Playing around with pack dupe protection and legend pity timer and legend dupe
ashesFunc <- CreateCollection("ashes", T, T)
b <- map_dfr(1:400, function(x) {OpenPack(ashesFunc)})

# add cards one at a time
for(i in 1:100) pick <- ashesFunc("goldc", 1) # just adds dust
for(i in 1:100) pick <- ashesFunc("common", 1) # adds cards
collection <- ashesFunc("")

################### Brain storming ways I can keep track of collected vs. collected and dusted
ashes <- CreateCollection("ashes", T)
aCo <- ashes("common", 1)
aDf <- bind_rows(aCo[[1]]) %>% pivot_longer(cols = everything(), names_to = "id", values_to = "amount")

# possibly filter out legends
aCo2 <- ashes("legend", 1)
aDf2 <- bind_rows(aCo2[[1]]) %>% pivot_longer(cols = everything(), names_to = "id", values_to = "amount") %>%
    filter(!(str_detect(id, "legend") & amount == 1))

# This is old code for insuring the number of commons/rares/etc. matched the probs I supplied, roughly.
df_config <-
    packs %>%
    mutate(id = row_number()) %>%
    pivot_longer(cols = matches("d[0-9]"),
                 names_to = "pickOrder",
                 values_to = "rarity") %>%
    group_by(id) %>%
    mutate(nrC = sum(rarity == "common"),
           nrR = sum(rarity == "rare"),
           nrE = sum(rarity == "epic"),
           nrL = sum(rarity == "legend"),
           nrGc = sum(rarity == "goldc"),
           nrGr = sum(rarity == "goldr"),
           nrGe = sum(rarity == "golde"),
           nrGl = sum(rarity == "goldl")) %>%
    select(-pickOrder, -rarity)
# %>%
#     distinct() %>%
#     group_by(nrC, nrR, nrE, nrL, nrGc, nrGr, nrGe, nrGl) %>%
#     summarise(count = n()) %>%
#     ungroup()
map(df_config, function(x) {sum(x) / (nrow(df_config) * 5)})
