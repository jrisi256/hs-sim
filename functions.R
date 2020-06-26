library(here)
source(file.path(here(), "globals.R"))

#
CreateCollection <-
    function(set, rrty = rarities, allSets = sets, dInfo = pInfo[["dust"]]) {
    
    # make character vectors for each card which will act as names in our list
    commons <- paste0(rrty["common"], seq(allSets$common[set]))
    rares <- paste0(rrty["rare"], seq(allSets$rare[set]))
    epics <- paste0(rrty["epic"], seq(allSets$epic[set]))
    legends <- paste0(rrty["legend"], seq(allSets$legend[set]))
    allCards <- c(commons, rares, epics, legends)
    
    # the collection, represented as a dictionary, implemented w/ a named list
    cllctn <- as.list(rep(0, length(allCards)))
    names(cllctn) <- allCards
    startDust <- 0
    
    function(draw) {
        
        # Just return the collection
        if(draw != "") {
            
            # if we drew a golden card, dust it
            gold <- c(rrty["goldc"], rrty["goldr"], rrty["golde"], rrty["goldl"])
            if(draw %in% gold) {
                startDust <<- startDust + unname(dInfo[draw])
                
            # else if the card is not golden
            } else {
                
                # Pick a card with the given rarity as specified by "draw"
                card <- allSets[[draw]][set] %>% seq() %>% sample(size = 1)
                idx <- paste0(draw, card)
                
                # If we already have full copies of the card, dust it
                if(cllctn[[idx]] == 2 | (draw == rrty["legend"] & cllctn[[idx]] == 1)) {
                    startDust <<- startDust + unname(dInfo[draw])
                
                # If we don't already have full copies, add it to collection
                } else if(cllctn[[idx]] == 0 | cllctn[[idx]] == 1) {
                    cllctn[[idx]] <<- cllctn[[idx]] + 1
                }
            }
        }
        return(list(cllctn, startDust))
    }
}

#
OpenPack <-
    function(AddCardFunc, space = rarities, draws = nrDraw, probs = pInfo[["pr"]]) {
        
        # draw a number of random rarities
        pack <- as.list(sample(space, draws, replace = T, probs))
        names(pack) <- c("d1", "d2", "d3", "d4", "d5")
        
        # For the given rarity, choose card from the set specified by function
        # Then we capture our cumulative dust total thus far
        openedPack <- map(pack, AddCardFunc)
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
PacksToCompletion <- function(useDust, setName) {
    
    # Create function for adding cards to collection given a specific set
    AddCardFunc <- CreateCollection(setName)
    
    # Pre-populate a list so we can keep a log of the packs we opened
    packs <- vector("list", 10000)
    
    # Keep track of number packs opened
    counter <- 1
    
    while(T) {
        
        # Add pack to log of packs
        pack <- OpenPack(AddCardFunc)
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
RunSimulation <- function(nrRuns, useDust, setLabels = setNames) {

    # Pre-populate lists
    nrPacksOpenedList <- vector("list", nrRuns)
    dustAccumulatedList <- vector("list", nrRuns)
    
    # Each run, simulate how many packs to a complete collection for each set
    for(i in seq(nrRuns)) {
        
        # For each completed set, return total number of packs opened as a df
        packLog <-
            map(as.list(setLabels),
                function(x) {PacksToCompletion(useDust = useDust, setName = x)})
        
        # Combine column-wise # of packs needed to be opened to complete a set
        nrPacksOpenedList[[i]] <- map_dfc(packLog, function(df) {return(nrow(df))})
        names(nrPacksOpenedList)[i] <- i
        
        # List of dfs where each df tracks dust total after each pack was opened
        dustAccumulatedList[[i]] <-
            pmap(list(packLog, names(packLog)), function(dustDf, setName) {
                dustDf %>%
                    select(dust) %>%
                    mutate(set = setName,
                           packNr = row_number(),
                           run = i)})
    }
    
    # Turn # of packs opened for each set for each simulation run into nice df
    nrPacksOpenedDf <-
        bind_rows(nrPacksOpenedList) %>%
        mutate(run = names(nrPacksOpenedList)) %>%
        pivot_longer(cols = -matches('run'),
                     names_to = "set",
                     values_to = "nrPacks") %>%
        mutate(useDust = useDust)

    # Turn dust total after each pack for each set for each sim run into nice df
    dustAccumulatedDf <-
        dustAccumulatedList %>%
        flatten_dfr() %>%
        mutate(useDust = useDust)
    
    # Return these two data frames
    return(list(nrPacks = nrPacksOpenedDf, dustTotals = dustAccumulatedDf))
}
