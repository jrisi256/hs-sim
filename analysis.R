library(here)
source(file.path(here(), "functions.R"))

packs <- map_dfr(1:300, function(x) {
    OpenPack(AddCardToAshesCollection)
})

a <- AddCardToAshesCollection("")
b <-
    tibble(nr = unlist(a[[1]], use.names = F), name = names(a[[1]])) %>%
    mutate(mssng = if_else(str_detect(name, "l"), nr - 1, nr - 2),
           neededDust = case_when(
               str_detect(name, "c") ~ 40 * mssng,
               str_detect(name, "r") ~ 100 * mssng,
               str_detect(name, "e") ~ 400 * mssng,
               str_detect(name, "l") ~ 1600 * mssng
           ))
if(sum(b$neededDust, a[[2]]) >= 0) {
    print("Hurray, complete your collection with dust")
} else
    print("Sad, cannot complete collection with dust")

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
