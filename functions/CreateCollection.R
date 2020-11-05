library(here)
source(file.path(here(), "globals.R"))

#
CreateCollection <-
    function(set, useDust, keepGold, packDupeProtect, legendDupeProtect,
             allDupeProtect, target, onlyTarget, rrty = rarities, allSets = sets,
             dInfo = pInfo[["dust"]]) {
        
        # make character vectors for each card which will act as names in our list
        commons <- paste0(rrty["common"], seq(allSets$common[set]))
        rares <- paste0(rrty["rare"], seq(allSets$rare[set]))
        epics <- paste0(rrty["epic"], seq(allSets$epic[set]))
        legends <- paste0(rrty["legend"], seq(allSets$legend[set]))
        allCards <- c(commons, rares, epics, legends)
        
        # the collection, represented as a dictionary, implemented w/ a named list
        cllctn <- as.list(rep(0, length(allCards)))
        names(cllctn) <- allCards
        
        # "phantom" collection, tracks all cards opened even if we dust them
        pCllctn <- cllctn
        
        # Keep track of cards opened in current pack to do pack dupe protection
        openingPack <- c(rep("", nrDraw))
        startDust <- 0
        
        function(draw, drawNr) {
            
            # If there are no blanks, it means the pack is full and can be reset
            if(all(openingPack != "")) openingPack <<- rep("", nrDraw)
            
            # pick a card of "draw" rarity from target set, else return collection
            if(draw != "") {
                
                gold <- c(rrty["goldc"], rrty["goldr"], rrty["golde"], rrty["goldl"])
                
                # Normalize the rarity by removing gold from the rarity name
                normalizeDraw <- case_when(draw == "goldc" ~ "common",
                                           draw == "goldr" ~ "rare",
                                           draw == "golde" ~ "epic",
                                           draw == "goldl" ~ "legend",
                                           TRUE ~ draw)
                
                # By default, select from whole set
                sampleSpace <- allSets[[normalizeDraw]][set] %>% seq()
                
                # If we have duplicate protection on across all rarities
                if(allDupeProtect) {
                    
                    # Find all cards in current rarity that we have max copies of
                    if(normalizeDraw != "legend") {
                        full <-
                            str_detect(names(pCllctn), normalizeDraw) & pCllctn >= 2
                        
                    } else {
                        full <-
                            str_detect(names(pCllctn), normalizeDraw) & pCllctn >= 1
                    }
                    
                    # Remove all fully collected cards from the sample space
                    collected <- str_extract(names(pCllctn[full]), "[0-9]{1,2}")
                    sampleSpace <- setdiff(sampleSpace, collected)
                    
                    # If length is 1, make into a list so the sample function works
                    if(length(sampleSpace) == 1) {
                        sampleSpace <- list(sampleSpace)
                        
                    # If empty, all cards in current rarity have been collected
                    } else if(length(sampleSpace) == 0) {
                        sampleSpace <- allSets[[normalizeDraw]][set] %>% seq()
                    }
                    
                    
                # If we're drawing a legendary, and have dupe protect on legendary  
                } else if(legendDupeProtect && normalizeDraw == "legend") {
                    
                    # Find all legends in our collection
                    collectedLegends <-
                        str_extract(names(cllctn[str_detect(names(cllctn), "legend") &
                                                     cllctn == 1]),
                                    "[0-9]{1,2}")
                    
                    # Remove all legends already collected from the sample space
                    sampleSpace <- setdiff(sampleSpace, collectedLegends)
                    
                    # If length is 1, make into a list so the sample function works
                    if(length(sampleSpace) == 1) {
                        sampleSpace <- list(sampleSpace)
                        
                    # if sample space is empty, means all legends collected
                    } else if(length(sampleSpace) == 0)
                        sampleSpace <- allSets[[normalizeDraw]][set] %>% seq()
                    
                    # Pack dupe protect, don't pick cards already picked twice
                } else if(packDupeProtect && normalizeDraw != "legend") {
                    
                    # Find nr. of cards in current pack with "normalizeDraw" rarity
                    openedRarities <- openingPack[str_detect(openingPack, normalizeDraw)]
                    countRarities <- table(openedRarities)
                    
                    # Keep only those cards drawn max amount
                    countRarities <- countRarities[countRarities == 2]
                    
                    # Remove all cards drawn twice from the sample space
                    sampleSpace <- setdiff(sampleSpace,
                                           str_extract(names(countRarities),
                                                       "[0-9]{1,2}"))
                }
                
                # Pick a card from the sample space
                card <- sampleSpace %>% sample(size = 1)
                idx <- paste0(normalizeDraw, card)
                
                # Add the card to our phantom collection
                pCllctn[[idx]] <<- pCllctn[[idx]] + 1
                
                # if we drew a golden card and we're not keeping them, dust it
                if(!keepGold && draw %in% gold ) {
                    startDust <<- startDust + unname(dInfo[draw])
                    
                    # If we drew a non-target card and we're not keeping them, dust it
                } else if(onlyTarget && !(idx %in% paste0(normalizeDraw, 1:target[normalizeDraw]))) {
                    startDust <<- startDust + unname(dInfo[draw])
                    
                    # If we drew a non-golden card (or a golden card but we're keeping them)
                    # AND a target card (or a non-target card but we're keeping them),
                    # add the card to the collection
                } else {
                    
                    # If we already have full copies of the card, dust it
                    if(cllctn[[idx]] == 2 || (normalizeDraw == rrty["legend"] && cllctn[[idx]] == 1)) {
                        startDust <<- startDust + unname(dInfo[draw])
                        
                        # If we don't already have full copies, add it to collection
                    } else if(cllctn[[idx]] == 0 || cllctn[[idx]] == 1) {
                        cllctn[[idx]] <<- cllctn[[idx]] + 1
                    }
                }
                
                # Add card to the pack
                openingPack[drawNr] <<- idx
                
                return(list(cllctn, startDust, card))
            }
            
            return(list(cllctn, startDust))
        }
    }
