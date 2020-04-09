# rarities
rarities <- c("c", "r", "e", "l", "gc", "gr", "ge", "gl")

# number of cards in a pack or the number of "draws"
nrDraw <- 5

# every set and the distribution of rarities
sets <- tibble(name = c("classic", "gvg", "grandt", "oldgods", "gadgetzan",
                        "ungoro", "frozent", "kobolds", "witchwood", "boomsday",
                        "rastakhan", "shadows", "uldum", "descent", "ashes"),
               nrC = c(92, 40, 49, 50, 49, 49, 49, 49, 48, 49, 49, 49, 49, 49,
                       52),
               nrR = c(80, 37, 36, 36, 36, 36, 36, 36, 35, 36, 36, 37, 36, 36,
                       35),
               nrE = c(36, 26, 27, 27, 27, 27, 27, 27, 25, 27, 27, 26, 27, 27,
                       23),
               nrL = c(32, 20, 20, 21, 20, 23, 23, 23, 21, 24, 23, 24, 23, 28,
                       25))

# probabilities of opening specific card rarities as of 2020-04-08 23:12:50 EST
packNr <- tibble(nrGc = 198978,
                 nrGr = 173882,
                 nrGe = 32768,
                 nrGl = 11178,
                 nrC = 10767865 -nrGc,
                 nrR = 3443419 - nrGr,
                 nrE = 664097 - nrGe,
                 nrL = 172864 - nrGl,
                 ttl = sum(nrGc, nrGr, nrGe, nrGl, nrC, nrR, nrE, nrL))

packStats <- tibble(prC = packNr[["nrC"]] / packNr[["ttl"]],
                    prR = packNr[["nrR"]] / packNr[["ttl"]],
                    prE = packNr[["nrE"]] / packNr[["ttl"]],
                    prL = packNr[["nrL"]] / packNr[["ttl"]],
                    prGc = packNr[["nrGc"]] / packNr[["ttl"]],
                    prGr = packNr[["nrGr"]] / packNr[["ttl"]],
                    prGe = packNr[["nrGe"]] / packNr[["ttl"]],
                    prGl = packNr[["nrGl"]] / packNr[["ttl"]])

#
OpenPack <-
    function(space = rarities, draws = nrDraw, probs = unlist(packStats[1,])) {
        pack <- as.list(sample(space, draws, replace = T, probs))
        names(pack) <- c("d1", "d2", "d3", "d4", "d5")
        return(pack)
    }
