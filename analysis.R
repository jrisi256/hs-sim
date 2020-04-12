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

ashes <- CreateCollection("ashes")
a <- ashes("common")
aDf <- bind_rows(a[[1]]) %>% pivot_longer(cols = everything(), names_to = "id", values_to = "amount")

a <- map(1:15, function(x) {PacksToCompletion(useDust = T, "ashes", "none")})
b <- map_dbl(a, function(x) {
    return(x[[2]])
})

bmean <- mean(b)
bmedian <- median(b)
bsd <- sd(b)
bse <- bsd / sqrt(15)

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
