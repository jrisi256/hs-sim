library(here)
source(file.path(here(), "packages.R"))
source(file.path(here(), "functions.R"))

packs <- map_dfr(1:10000, function(x) {OpenPack()})

df_config <-
    packs %>%
    mutate(id = row_number()) %>%
    pivot_longer(cols = matches("d[0-9]"),
                 names_to = "pickOrder",
                 values_to = "rarity") %>%
    group_by(id) %>%
    mutate(nrC = sum(rarity == "c"),
           nrR = sum(rarity == "r"),
           nrE = sum(rarity == "e"),
           nrL = sum(rarity == "l"),
           nrGc = sum(rarity == "gc"),
           nrGr = sum(rarity == "gr"),
           nrGe = sum(rarity == "ge"),
           nrGl = sum(rarity == "gl")) %>%
    select(-pickOrder, -rarity) 
# %>%
#     distinct() %>%
#     group_by(nrC, nrR, nrE, nrL, nrGc, nrGr, nrGe, nrGl) %>%
#     summarise(count = n()) %>%
#     ungroup()
map(df_config, function(x) {sum(x) / (nrow(df_config) * 5)})