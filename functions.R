library(here)
source(file.path(here(), "packages.R"))

# number of cards in a pack or the number of "draws"
nrDraw <- 5

# every set and the distribution of rarities
sets <- tibble(name = c("classic", "gvg", "grandt", "oldgods", "gadgetzan",
                        "ungoro", "frozent", "kobolds", "witchwood", "boomsday",
                        "rastakhan", "shadows", "uldum", "descent", "ashes"),
               c = c(92, 40, 49, 50, 49, 49, 49, 49, 48, 49, 49, 49, 49, 49,
                       52),
               r = c(80, 37, 36, 36, 36, 36, 36, 36, 35, 36, 36, 37, 36, 36,
                       35),
               e = c(36, 26, 27, 27, 27, 27, 27, 27, 25, 27, 27, 26, 27, 27,
                       23),
               l = c(32, 20, 20, 21, 20, 23, 23, 23, 21, 24, 23, 24, 23, 28,
                       25))

# numbers/probabilities of pulling certain rarities as well as other info
pInfo <-
    tibble(rarity = c("c", "r", "e", "l", "gc", "gr", "ge", "gl"),
           nr = c(10767865 - 198978,
                  3443419 - 173882,
                  664097 - 32768,
                  172864 - 11178,
                  198978,
                  173882,
                  32768,
                  11178),
           pr = nr / sum(nr),
           dust = c(5, 20, 100, 400, 50, 100, 400, 1600))

#
OpenPack <-
    function(set,
             space = pInfo[["rarity"]],
             draws = nrDraw,
             probs = pInfo[["pr"]])
    {
        pack <- as.list(sample(space, draws, replace = T, probs))
        names(pack) <- c("d1", "d2", "d3", "d4", "d5")
        walk(pack, AddCardToCollection, set = set)
        collection <- AddCardToCollection("", "")
        return(list(pack, collection))
    }

CreateCollection <- function() {
    collection <- list(c = vector(length = 10),
                       r = vector(length = 10),
                       e = vector(length = 10),
                       l = vector(length = 10),
                       cIndex = 1,
                       rIndex = 1,
                       eIndex = 1,
                       lIndex = 1)
    
    function(draw, set) {
        if(draw != "" & set != "") {
            print(collection)
            
            draw <- str_replace(draw, "g", "")
            card <- sets %>% filter(name == set) %>% pull(draw) %>% seq() %>% sample(size = 1)
            counter <- collection[[paste0(draw, "Index")]]
            collection[[draw]][[counter]] <<- card
            collection[[paste0(draw, "Index")]] <<- collection[[paste0(draw, "Index")]] + 1
            
        }
        return(collection)
    }
}

AddCardToCollection <- CreateCollection()
