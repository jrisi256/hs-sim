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
