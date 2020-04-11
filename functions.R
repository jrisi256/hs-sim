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
CreateCollection <- function(setName, allSets = sets, dustInfo = pInfo) {
    
    # find set information
    targetSet <- allSets %>% filter(name == setName)
    
    # make character vectors for each card which will act as names in our list
    commons <- paste0("c", seq(targetSet$c))
    rares <- paste0("r", seq(targetSet$r))
    epics <- paste0("e", seq(targetSet$e))
    legends <- paste0("l", seq(targetSet$l))
    allCards <- c(commons, rares, epics, legends)
    
    # the collection, represented as a dictionary, implemented w/ a named list
    collection <- as.list(rep(0, length(allCards)))
    names(collection) <- allCards
    startDust <- 0
    
    function(draw, set = targetSet, dustDf = dustInfo) {
        if(draw != "") {
            
            # if we drew a golden card, dust it
            if(str_detect(draw, "g")) {
                cardDust <- dustInfo %>% filter(rarity == draw) %>% pull(dust)
                startDust <<- startDust + cardDust
                
            # else if the card is not golden
            } else {
                card <- set %>% pull(draw) %>% seq() %>% sample(size = 1)
                index <- paste0(draw, card)
                
                # If we already have full copies of the card, dust it
                if(collection[[index]] == 2 | (draw == "l" & collection[[index]] == 1)) {
                    cardDust <- dustInfo %>% filter(rarity == draw) %>% pull(dust)
                    startDust <<- startDust + cardDust
                
                # If we don't already have full copies, add it to collection
                } else {
                    collection[[index]] <<- collection[[index]] + 1
                }
            }
        }
        return(list(collection, startDust))
    }
}

addCardNames <- list(
    AddCardFromAshes = "ashes",
    AddCardFromDescent = "descent",
    AddCardFromUldum = "uldum",
    AddCardFromShadows = "shadows",
    AddCardFromRasta = "rastakhan",
    AddCardFromBoom = "boomsday",
    AddCardFromWitch = "witchwood",
    AddCardFromKob = "kobolds",
    AddCardFromFrozen = "frozent",
    AddCardFromUngoro = "ungoro",
    AddCardFromGadget = "gadgetzan",
    AddCardFromOldG = "oldgods",
    AddCardFromGrandT = "grandt",
    AddCardFromGvg = "gvg",
    AddCardFromClassic = "classic"
)

addCardFuncs <- map(addCardNames, CreateCollection)

#
OpenPack <-
    function(AddCardFunc,
             space = pInfo[["rarity"]],
             draws = nrDraw,
             probs = pInfo[["pr"]])
    {
        pack <- as.list(sample(space, draws, replace = T, probs))
        names(pack) <- c("d1", "d2", "d3", "d4", "d5")
        walk(pack, AddCardFunc)
        return(pack)
    }

#
CompleteCollection <- function(collection) {
    
    df <-
        tibble(nr = unlist(collection[[1]], use.names = F),
               name = names(collection[[1]])) %>%
        mutate(mssng = if_else(str_detect(name, "l"), nr - 1, nr - 2),
               neededDust = case_when(
                   str_detect(name, "c") ~ 40 * mssng,
                   str_detect(name, "r") ~ 100 * mssng,
                   str_detect(name, "e") ~ 400 * mssng,
                   str_detect(name, "l") ~ 1600 * mssng
               ))
    
    if(sum(df$neededDust, collection[[2]]) >= 0) {
        return(T)
    } else
        return(F)
}

CompleteCollectionNoDust <- function(collection) {
    df <-
        tibble(nr = unlist(collection[[1]], use.names = F),
               name = names(collection[[1]])) %>%
        mutate(complete = case_when(
            str_detect(name, "l") & nr == 1 ~ T,
            str_detect(name, "l") & nr == 0 ~ F,
            nr == 2 ~ T,
            nr == 0 | nr == 1 ~ F
        ))
    
    if(all(df$complete)) {
        return(T)
    } else {
        return(F)
    }
}