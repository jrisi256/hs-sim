library(here)
source(file.path(here(), "functions.R"))

packs <- vector("list", 3000)
counter <- 1
completeCollectionCounter = 0

while(T) {
    
    # Add pack to log of packs
    pack <- OpenPack(addCardFuncs$ashes)
    packs[[counter]] <- pack
    
    # Get collection
    collection <- addCardFuncs$ashes("")
    
    # Collection is complete using our dust
    if(CompleteCollection(collection) & completeCollectionCounter == 0)
        completeCollectionCounter <- counter
        
    # Collection is complete not using dust
    if(CompleteCollectionNoDust(collection))
        break()
    
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
