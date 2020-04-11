library(here)
source(file.path(here(), "functions.R"))

completeCollection <- F
completeCollectionNoDust <- F
packs <- vector("list", 3000)
counter <- 1
completeCollectionCounter = 0

while(!completeCollectionNoDust) {
    
    # Add pack to log of packs
    pack <- OpenPack(addCardFuncs$AddCardFromAshes)
    packs[[counter]] <- pack
    
    # Check if completion is complete with/without using dust
    collection <- addCardFuncs$AddCardFromAshes("")
    completeCollection <- CompleteCollection(collection)
    completeCollectionNoDust <- CompleteCollectionNoDust(collection)
    
    if(completeCollection & completeCollectionCounter == 0)
        completeCollectionCounter <- counter
    
    counter <- counter + 1
}

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
