library(here)
source(file.path(here(), "globals.R"))

#
PacksToCompletion <- function(useDust, keepGold, packDupeProtect,
                              guaranteeLegend, legendDupeProtect,
                              allDupeProtect, onlyTarget, setName, 
                              target = c(common = sets[["common"]][[setName]],
                                         rare = sets[["rare"]][[setName]],
                                         epic = sets[["epic"]][[setName]],
                                         legendary = sets[["legendary"]][[setName]])) {
    
    # Create function for adding cards to collection given a specific set
    AddCardFunc <- CreateCollection(setName,
                                    useDust = useDust,
                                    keepGold = keepGold,
                                    packDupeProtect = packDupeProtect,
                                    legendDupeProtect = legendDupeProtect,
                                    allDupeProtect = allDupeProtect,
                                    target = target,
                                    onlyTarget = onlyTarget)
    
    # Pre-populate a list so we can keep a log of the packs we opened
    packs <- vector("list", 10000)
    
    # Keep track of number packs opened
    counter <- 1
    
    while(T) {
        
        # If we're guaranteeing a legend in 1st 10 packs, use a pity timer
        if(guaranteeLegend && counter <= 10) {
            pack <- OpenPack(AddCardFunc, guaranteePityTimer = counter / 10)
            
            # If we opened a legendary or golden legendary, turn off pity timer
            if(any(pack == "legend" | pack == "goldl"))
                guaranteeLegend = F
            
            # No legend guarantee or opened at least 10 packs, don't use pity timer
        } else if(!guaranteeLegend || counter > 10) {
            pack <- OpenPack(AddCardFunc)
        }
        
        # Add pack to log of packs
        packs[[counter]] <- pack
        
        # Get collection
        collection <- AddCardFunc("")
        
        # Using dust and collection is complete, return pack log and set
        if(useDust && CompleteCollection(collection)) {
            packs <- bind_rows(packs)
            return(packs)
            
            # Not using dust and collection is complete, return pack log and set
        } else if(!useDust && CompleteCollectionNoDust(collection)) {
            packs <- bind_rows(packs)
            return(packs)
        }
        
        counter <- counter + 1
    }
}
