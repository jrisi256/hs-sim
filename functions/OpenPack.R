library(here)
source(file.path(here(), "globals.R"))

#' Open a pack. Called from within PacksToCompletion.
#' 
#' @param AddCardFunc Function. Function created from CreateCollection function
#' factory.
#' @param guranteePityTimer Numeric. Defaults to 0. Can give it a number between
#' 0 and 1. 0 indicates pity timer is off. Any other number will be used as the
#' new probability for pulling a legendary.
#' @param space Named character vector. The names of the rarities in Hearthstone.
#' Defaults to the global variable rarities.
#' @param draws Numeric. The number of cards in a pack. Defaults to the global
#' variable nrDraw.
#' @param probs Named numeric vector. Probabilities of pulling a card of each
#' rarity type. Defaults to the global variable pInfo[["pr"]].
#' 
#' @return A 6 element list. The first 5 elements are the opened cards. The 6th
#' elements is the accumulated dust.
#' 
#' @example
#' AshesCollection <- CreateCollection(set = "ashes",
#'                                     useDust = T,
#'                                     keepGold = F,
#'                                     packDupeProtect = F,
#'                                     legendDupeProtect = F,
#'                                     allDupeProtect = T,
#'                                     onlyTarget = T,
#'                                     target = c(common = 30, rare = 12, epic = 10,
#'                                                legend = 5))
#' pack <- OpenPack(AshesCollection)
#' collection <- AshesCollection("")
OpenPack <-
    function(AddCardFunc, guaranteePityTimer = 0, space = rarities,
             draws = nrDraw, probs = pInfo[["pr"]]) {
        
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
        
        # Capture the cards opened in our pack
        openedCards <- openedPack %>% map(3)
        openedCards <- paste0(pack, unlist(openedCards))
        names(openedCards) <- c("d1", "d2", "d3", "d4", "d5")
        
        # Return the pack rarity distribution
        return(as.list(c(openedCards, dust = cDust)))
    }
