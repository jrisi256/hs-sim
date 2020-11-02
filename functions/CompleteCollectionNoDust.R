#
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
