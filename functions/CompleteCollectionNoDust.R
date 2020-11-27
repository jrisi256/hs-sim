#' Is our collection complete without using dust? Called in PacksToCompletion.
#' 
#' @param collection A two element list. The first element is a numeric vector
#' which represents every card in the set and the number of copies obtained of
#' each card. The second element is the amount of dust required.
#' @param target Named numeric vector. Specifies number of common, rare,
#' epic, and legendary cards one wishes to obtain from the set.
#' 
#' @return Returns a logical indicating if the collection is complete or not.
#' 
#' @example
#' target <- c(common = 30, rare = 12, epic = 10, legend = 5)
#' AshesCollection <- CreateCollection(set = "ashes",
#'                                     useDust = T,
#'                                     keepGold = F,
#'                                     packDupeProtect = F,
#'                                     legendDupeProtect = F,
#'                                     allDupeProtect = T,
#'                                     onlyTarget = T,
#'                                     target = target)
#' CompleteCollectionNoDust(AshesCollection(""), target)
CompleteCollectionNoDust <- function(collection, target) {
    
    common <- ""
    rare <- ""
    epic <- ""
    legendary <- ""
    
    if(target["common"] != 0)
        common <- paste0("common", 1:target["common"])
    if(target["rare"] != 0)
        rare <- paste0("rare", 1:target["rare"])
    if(target["epic"] != 0)
        epic <- paste0("epic", 1:target["epic"])
    if(target["legend"] != 0)
        legendary <- paste0("legend", 1:target["legend"])
    
    targetCollection <- unlist(collection[[1]][c(common, rare, epic)])
    targetCollection <- targetCollection[!is.na(targetCollection)]
    
    targetL <- unlist(collection[[1]][legendary])
    targetL <- targetL[!is.na(targetL)]
    
    if(all(targetCollection == 2) && all(targetL == 1))
        return(T)
    else
        return(F)
}
