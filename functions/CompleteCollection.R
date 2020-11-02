library(here)
source(file.path(here(), "globals.R"))

#
CompleteCollection <-
    function(collection, target, craftInfo = pInfo[["craft"]]) {
        
        if(target["common"] != 0) {
            common <- paste0("common", 1:target["common"])
            commonDust <- sum((unlist(collection[[1]][common]) - 2) * craftInfo["common"])
        } else
            commonDust <- 0
        
        if(target["rare"] != 0) {
            rare <- paste0("rare", 1:target["rare"])
            rareDust <- sum((unlist(collection[[1]][rare]) - 2) * craftInfo["rare"])
        } else
            rareDust <- 0
            
        if(target["epic"] != 0) {
            epic <- paste0("epic", 1:target["epic"])
            epicDust <- sum((unlist(collection[[1]][epic]) - 2) * craftInfo["epic"])
        } else
            epicDust <- 0
            
        if(target["legend"] != 0) {
            legendary <- paste0("legend", 1:target["legend"])
            lDust <- sum((unlist(collection[[1]][legendary]) - 1) * craftInfo["legend"])
        } else
            lDust <- 0

        tDust <- sum(commonDust, rareDust, epicDust, lDust)
        
        # Do we have enough dust to craft all the missing cards?
        if(sum(tDust, collection[[2]]) >= 0) {
            return(T)
        } else
            return(F)
    }
