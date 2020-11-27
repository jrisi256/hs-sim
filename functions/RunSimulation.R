library(here)
source(file.path(here(), "globals.R"))

#' Run a simulation a number of times for each set for the targeted collection.
#' The simulation is designed to capture the number of packs needed to complete
#' the targeted collection for the target set.
#' 
#' @param nrRuns Integer. How many completed collections should be simulated for
#' each set?
#' @param useDust Logical. Should dust be used to complete the collection?
#' @param keepGold Logical. Should we keep golden cards or dust them?
#' @param packDupeProtect Logical. Do we have pack-level duplicate protection?
#' @param guaranteeLegend Logical. Do we ensure a legendary in the 1st 10 packs?
#' @param legendDupeProtect Logical. Do we have legendary duplication protection?
#' @param allDupeProtect Logical. Do we have duplicate protection for all rarities?
#' @param onlyTarget Logical. Should we keep only cards in target?
#' @param setLabels Character vector. What sets are we running the simulation on?
#' Defaults to all sets.  
#' @param target List of named numeric vectors. Specifies number of common, rare,
#' epic, and legendary cards one wishes to obtain from the specific set. By
#' default it is set to NULL and  is subsequently set to be equal to the
#' complete set for each set. It's important the order of target matches the
#' order of setLabels.
#'
#' @return A list of two data frames. The first data frame is the number of packs
#' needed to complete the target collection for each set for each run along with
#' the simulation configuration options (keepGold, packDupeProtect, etc.). The
#' second data frame is the amount of dust accumulated per pack for every run
#' for every set.
#' 
#' @examples
#' sim1 <- RunSimulation(nrRuns = 2,
#'                       useDust = T,
#'                       keepGold = F,
#'                       packDupeProtect = T,
#'                       guaranteeLegend = T,
#'                       legendDupeProtect = F,
#'                       allDupeProtect = F,
#'                       onlyTarget = F)
#' packTotal1 <- sim1 %>% map(1) %>% bind_rows()
#' dustTotal1 <- sim1 %>% map(2) %>% bind_rows()
#' 
#' sim2 <- RunSimulation(nrRuns = 2,
#'                       useDust = T,
#'                       keepGold = F,
#'                       packDupeProtect = T,
#'                       guaranteeLegend = T,
#'                       legendDupeProtect = F,
#'                       allDupeProtect = F,
#'                       onlyTarget = F,
#'                       setLabels = c("classic", "gvg", "grandt"),
#'                       target = list(classic = c(common = 30,
#'                                                 rare = 10,
#'                                                 epic = 5,
#'                                                 legend = 2),
#'                                     gvg = c(common = 20,
#'                                             rare = 7,
#'                                             epic = 3,
#'                                             legend = 1),
#'                                     grandt = c(common = 0,
#'                                                rare = 5,
#'                                                epic = 2,
#'                                                legend = 1)))
#' packTotal2 <- sim2 %>% map(1) %>% bind_rows()
#' dustTotal2 <- sim2 %>% map(2) %>% bind_rows()
RunSimulation <- function(nrRuns, useDust, keepGold, packDupeProtect,
                          guaranteeLegend, legendDupeProtect, allDupeProtect,
                          onlyTarget, setLabels = setNames, target = NULL) {

    if(is.null(target)) {
        
        target <- map(as.list(setLabels), function(setLabel) {
            c(common = sets[["common"]][[setLabel]],
              rare = sets[["rare"]][[setLabel]],
              epic = sets[["epic"]][[setLabel]],
              legend = sets[["legend"]][[setLabel]])  
        })
    }
    
    # Create file to write progress to
    writeLines(c(""), "log.txt")
    
    # Each run, simulate how many packs to complete target collection for each set
    foreach(i = 1:nrRuns) %dopar% {
        
        # Open file to write progress to
        sink("log.txt", append = T)
        
        # For each completed set, return total number of packs opened as a df
        packLog <-
            pmap(list(as.list(setLabels), target),
                function(x, y) {
                    
                    cat(paste0("\nCurrent Iteration: ", i, "\n",
                               "Current Set: ", x, "\n",
                               "Current Configuration: ", useDust, keepGold,
                               packDupeProtect, guaranteeLegend,
                               legendDupeProtect, allDupeProtect, onlyTarget,
                               "\n"))
                    
                    PacksToCompletion(useDust = useDust,
                                      keepGold = keepGold,
                                      packDupeProtect = packDupeProtect,
                                      guaranteeLegend = guaranteeLegend,
                                      legendDupeProtect = legendDupeProtect,
                                      allDupeProtect = allDupeProtect,
                                      onlyTarget = onlyTarget,
                                      setName = x,
                                      target = y)
        })
        
        names(packLog) <- setLabels
        
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
                   allDupeProtect = allDupeProtect,
                   onlyTarget = onlyTarget)
        
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
                           allDupeProtect = allDupeProtect,
                           onlyTarget = onlyTarget)
            })
        
        list(nrPacksOpened, dustAccumulated)
    }
}
