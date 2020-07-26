library(here)
source(file.path(here(), "globals.R"))

#
CreateCollection <-
    function(set, useDust, keepGold, packDupeProtect, legendDupeProtect,
             allDupeProtect, rrty = rarities, allSets = sets,
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
            } else if(legendDupeProtect & normalizeDraw == "legend") {
                
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
            } else if(packDupeProtect & normalizeDraw != "legend") {
                
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
            if(draw %in% gold & !keepGold) {
              startDust <<- startDust + unname(dInfo[draw])
            
            } else {
                
                # If we already have full copies of the card, dust it
                if(cllctn[[idx]] == 2 | (normalizeDraw == rrty["legend"] & cllctn[[idx]] == 1)) {
                    startDust <<- startDust + unname(dInfo[draw])
                
                # If we don't already have full copies, add it to collection
                } else if(cllctn[[idx]] == 0 | cllctn[[idx]] == 1) {
                    cllctn[[idx]] <<- cllctn[[idx]] + 1
                }
            }
            
            # Add card to the pack
            openingPack[drawNr] <<- idx
        }
        return(list(cllctn, startDust))
    }
}

#
OpenPack <-
    function(AddCardFunc, guaranteePityTimer = 0,
             space = rarities, draws = nrDraw, probs = pInfo[["pr"]]) {
        
        # Scale up ratio of legends to golden legends to account for pity timer
        guaranteePityTimerGold <-
            pInfo[["pr"]][["goldl"]] * guaranteePityTimer / pInfo[["pr"]][["legend"]]
        
        # Pity timer for opening a legend in the 1st 10 packs, defaults to 0
        guaranteedLegend <-
            sample(c("nolegend", "legend", "goldl"),
                   size = 1,
                   prob = c(1 - guaranteePityTimer,
                            guaranteePityTimer - guaranteePityTimerGold,
                            guaranteePityTimerGold))
        
        # Didn't draw a legend or pity timer is off, create pack like normal
        if(guaranteedLegend == "nolegend") {
            pack <- as.list(sample(space, draws, replace = T, probs))
        
        # Drew a legend, only need to draw 4 cards from modified distribution   
        } else {
            
            # Distribute probability of opening legend to other rarities evenly
            legendProb <-
                (pInfo[["pr"]][["legend"]] + pInfo[["pr"]][["goldl"]]) / 6
            
            # Remove legend probabilities and add them to other rarities
            probsMod <- pInfo[["pr"]]
            probsMod <- probsMod[!(names(probsMod) %in% c("legend", "goldl"))]
            probsMod <- probsMod + legendProb
            
            # Remove legends from rarities
            spaceMod <- space[!(names(space) %in% c("legend", "goldl"))]
            
            pack <- as.list(c(guaranteedLegend,
                              sample(spaceMod, draws - 1, replace = T, probsMod)))
        }
        
        names(pack) <- c("d1", "d2", "d3", "d4", "d5")
        
        # For the given rarity, choose card from the set specified by function
        # Then we capture our cumulative dust total thus far
        openedPack <- pmap(list(pack, 1:length(pack)), AddCardFunc)
        cDust <- openedPack[[draws]][[2]]
        
        # Return the pack rarity distribution
        return(as.list(c(pack, dust = cDust)))
}

#
CompleteCollection <-
    function(collection, craftInfo = pInfo[["craft"]], rrts = rarities) {

    # Turn collection into data frame, calculate dust cost of missing cards
    df <-
        tibble(nr = unlist(collection[[1]], use.names = F),
               name = names(collection[[1]])) %>%
        mutate(missing = if_else(str_detect(name, rrts["legend"]), nr - 1, nr - 2),
               neededDust = case_when(
                   str_detect(name, rrts["common"]) ~ craftInfo["common"] * missing,
                   str_detect(name, rrts["rare"]) ~ craftInfo["rare"] * missing,
                   str_detect(name, rrts["epic"]) ~ craftInfo["epic"] * missing,
                   str_detect(name, rrts["legend"]) ~ craftInfo["legend"] * missing))
    
    # Do we have enough dust to craft all the missing cards?
    if(sum(df$neededDust, collection[[2]]) >= 0) {
        return(T)
    } else
        return(F)
}

#
CompleteCollectionNoDust <- function(collection, rrts = rarities) {
    
    # Turn list into data frame and find all missing cards
    df <-
        tibble(nr = unlist(collection[[1]], use.names = F),
               name = names(collection[[1]])) %>%
        mutate(complete = case_when(
            str_detect(name, rrts["legend"]) & nr == 1 ~ T,
            str_detect(name, rrts["legend"]) & nr == 0 ~ F,
            nr == 2 ~ T,
            nr == 0 | nr == 1 ~ F
        ))
    
    # Are there any missing cards?
    if(all(df$complete)) {
        return(T)
    } else {
        return(F)
    }
}

#
PacksToCompletion <- function(useDust, keepGold, packDupeProtect,
                              guaranteeLegend, legendDupeProtect,
                              allDupeProtect, setName) {
    
    # Create function for adding cards to collection given a specific set
    AddCardFunc <- CreateCollection(setName,
                                    useDust = useDust,
                                    keepGold = keepGold,
                                    packDupeProtect = packDupeProtect,
                                    legendDupeProtect = legendDupeProtect,
                                    allDupeProtect = allDupeProtect)
    
    # Pre-populate a list so we can keep a log of the packs we opened
    packs <- vector("list", 10000)
    
    # Keep track of number packs opened
    counter <- 1
    
    while(T) {
        
        # If we're guaranteeing a legend in 1st 10 packs, use a pity timer
        if(guaranteeLegend & counter <= 10) {
            pack <- OpenPack(AddCardFunc, guaranteePityTimer = counter / 10)
            
            # If we opened a legendary or golden legendary, turn off pity timer
            if(any(pack == "legend" | pack == "goldl"))
                guaranteeLegend = F
            
        # No legend guarantee or opened at least 10 packs, don't use pity timer
        } else if(!guaranteeLegend | counter > 10) {
            pack <- OpenPack(AddCardFunc)
        }
        
        # Add pack to log of packs
        packs[[counter]] <- pack
        
        # Get collection
        collection <- AddCardFunc("")
        
        # Using dust and collection is complete, return pack log and set
        if(useDust & CompleteCollection(collection)) {
            packs <- bind_rows(packs)
            return(packs)
            
        # Not using dust and collection is complete, return pack log and set
        } else if(!useDust & CompleteCollectionNoDust(collection)) {
            packs <- bind_rows(packs)
            return(packs)
        }
        
        counter <- counter + 1
    }
}

#
RunSimulation <- function(nrRuns, useDust, keepGold, packDupeProtect,
                          guaranteeLegend, legendDupeProtect, allDupeProtect, 
                          setLabels = setNames) {
    
    # Create file to write progress to
    writeLines(c(""), "log.txt")
    
    # Each run, simulate how many packs to a complete collection for each set
    foreach(i = 1:nrRuns) %dopar% {
        
        # Open file to write progress to
        sink("log.txt", append = T)
        
        # For each completed set, return total number of packs opened as a df
        packLog <-
            map(as.list(setLabels),
                function(x) {
                    
                    cat(paste0("\nCurrent Iteration: ", i, "\n",
                               "Current Set: ", x, "\n",
                               "Current Configuration: ", useDust, keepGold,
                               packDupeProtect, guaranteeLegend,
                               legendDupeProtect, allDupeProtect, "\n"))
                    
                    PacksToCompletion(useDust = useDust,
                                      keepGold = keepGold,
                                      packDupeProtect = packDupeProtect,
                                      guaranteeLegend = guaranteeLegend,
                                      legendDupeProtect = legendDupeProtect,
                                      allDupeProtect = allDupeProtect,
                                      setName = x)
        })
        
        # Take the number of packs opened for each set and turn into a df
        nrPacksOpened <-
            map_dfc(packLog, function(df) {return(nrow(df))}) %>%
            pivot_longer(cols = everything(),
                         names_to = "set",
                         values_to = "nrPacks") %>%
            mutate(run = i,
                   useDust = useDust,
                   keepGold = keepGold,
                   packDupeProtect = packDupeProtect,
                   guaranteeLegend = guaranteeLegend,
                   legendDupeProtect = legendDupeProtect,
                   allDupeProtect = allDupeProtect)
        
        # Turn the dust accumulated at each pack opening for each set into a df
        dustAccumulated <-
            pmap_dfr(list(packLog, names(packLog)), function(dustDf, setName) {
                dustDf %>%
                    select(dust) %>%
                    mutate(set = setName,
                           packNr = row_number(),
                           run = i,
                           useDust = useDust,
                           keepGold = keepGold,
                           packDupeProtect = packDupeProtect,
                           guaranteeLegend = guaranteeLegend,
                           legendDupeProtect = legendDupeProtect,
                           allDupeProtect = allDupeProtect)
            })
        
        list(nrPacksOpened, dustAccumulated)
    }
}
