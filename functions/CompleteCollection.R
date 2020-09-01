library(here)
source(file.path(here(), "globals.R"))

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
