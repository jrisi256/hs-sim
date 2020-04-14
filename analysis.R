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


################### Brain storming ways I can keep track of collected vs. collected and dusted
ashes <- CreateCollection("ashes")
aCo <- ashes("common")
aDf <- bind_rows(aCo[[1]]) %>% pivot_longer(cols = everything(), names_to = "id", values_to = "amount")

# Brain storming ways I can open packs until a full collection for each set an arbitrary number of times
# And then finding the stats
results <- vector("list", 2)
resultsDust <- vector("list", 2)

for(i in seq(2)) {
    a <- map(as.list(setNames), function(x) {PacksToCompletion(useDust = T, setName = x)})
    results[[i]] <- map_dfc(a, function(x) {return(nrow(x))})
    
    resultsDust[[i]] <-
        pmap(list(a, names(a)),
             function(dustDf, setName) {
                 dustDf %>%
                     select(dust) %>%
                     mutate(set = setName,
                            packNr = row_number(),
                            iteration = i)
                 })
}

resultsDf <-
    bind_rows(results) %>%
    pivot_longer(cols = everything(),
                 names_to = "set",
                 values_to = "nrPacks") %>%
    group_by(set) %>%
    summarise(mean = mean(nrPacks),
              sd = sd(nrPacks),
              se = sd / sqrt(n()),
              median = median(nrPacks),
              n = n())

# Trying to track the accumulation of dust across packs
b <- map_dfr(resultsDust, function(x) {bind_rows(x)})

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
