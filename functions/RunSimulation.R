library(here)
source(file.path(here(), "globals.R"))

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
