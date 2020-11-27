library(here)
source(file.path(here(), "globals.R"))

#' Called from RunSimulation. Given a set and a target collection, this function
#' will return how many packs it takes to complete the collection as well as an
#' associated pack log which catalogs every pack opened and dust acquired.
#' 
#' @param useDust Logical. Should dust be used to complete the collection?
#' @param keepGold Logical. Should we keep golden cards or dust them?
#' @param packDupeProtect Logical. Do we have pack-level duplicate protection?
#' @param guaranteeLegend Logical. Do we ensure a legendary in the 1st 10 packs?
#' @param legendDupeProtect Logical. Do we have legendary duplication protection?
#' @param allDupeProtect Logical. Do we have duplicate protection for all rarities?
#' @param onlyTarget Logical. Should we keep only cards in target?
#' @param setName Character. What set are we running the simulation on?
#' @param target Named numeric vector. Specifies number of common, rare,
#' epic, and legendary cards one wishes to obtain from the set.
#' 
#' @return A data frame which catalogs every packed opened and the dust acquired
#' with the opening of each pack (due to dusting cards).
#' 
#' @examples 
#' test <- PacksToCompletion(useDust = T,
#'                           keepGold = F,
#'                           packDupeProtect = T,
#'                           guaranteeLegend = T,
#'                           legendDupeProtect = F,
#'                           allDupeProtect = F,
#'                           onlyTarget = T,
#'                           setName = "ashes",
#'                           target = c(common = 0, rare = 6, epic = 6, legend = 4))
PacksToCompletion <- function(useDust, keepGold, packDupeProtect,
                              guaranteeLegend, legendDupeProtect,
                              allDupeProtect, onlyTarget, setName, target) {
    
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
            if(any(str_detect(pack, "legend|goldl")))
                guaranteeLegend = F
            
        # No legend guarantee or opened at least 10 packs, don't use pity timer
        } else if(!guaranteeLegend || counter > 10) {
            pack <- OpenPack(AddCardFunc)
        }
        
        # Add pack to log of packs
        packs[[counter]] <- pack
        
        # Get collection
        collection <- AddCardFunc("", "")
        
        # Using dust and collection is complete, return pack log and set
        if(useDust && CompleteCollection(collection, target)) {
            packs <- bind_rows(packs)
            return(packs)
            
        # Not using dust and collection is complete, return pack log and set
        } else if(!useDust && CompleteCollectionNoDust(collection, target)) {
            packs <- bind_rows(packs)
            return(packs)
        }
        
        counter <- counter + 1
    }
}
