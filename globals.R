library(here)
source(file.path(here(), "packages.R"))
registerDoParallel(cores = parallel::detectCores() - 1)

# number of cards in a pack or the number of "draws"
nrDraw <- 5

# set names and rarity names
setNames = c(classic = "classic", gvg = "gvg", grandt = "grandt",
             oldgods = "oldgods", gadgetzan = "gadgetzan", ungoro = "ungoro",
             frozent = "frozent", kobolds = "kobolds", witchwood = "witchwood",
             boomsday = "boomsday", rastakhan = "rastakhan", shadows = "shadows",
             uldum = "uldum", descent = "descent", ashes = "ashes")
rarities <- c(common = "common", rare = "rare", epic = "epic", legend = "legend",
              goldc = "goldc", goldr = "goldr", golde = "golde", goldl = "goldl")

# pulled from https://pitytracker.com/insights on 2020-04-11 15:47:33 EST
nrRarities <- c(10816247 - 199883,
                3458886 - 174657,
                667068 - 32916,
                173619  - 11216,
                199883,
                174657,
                32916,
                11216)

# every set and the distribution of rarities
sets <- map(list(c(92, 40, 49, 50, 49, 49, 49, 49, 48, 49, 49, 49, 49, 49, 52),
                 c(80, 37, 36, 36, 36, 36, 36, 36, 35, 36, 36, 37, 36, 36, 35),
                 c(36, 26, 27, 27, 27, 27, 27, 27, 25, 27, 27, 26, 27, 27, 23),
                 c(32, 20, 20, 21, 20, 23, 23, 23, 21, 24, 23, 24, 23, 28, 25)),
            function(x, names) {
                names(x) = names
                return(x)},
            names = setNames)

names(sets) <- rarities[c("common", "rare", "epic", "legend")]

# probabilities of pulling certain rarities as well as dust/craft values
pInfo <- map(list(pr = nrRarities / sum(nrRarities),
                  dust = c(5, 20, 100, 400, 50, 100, 400, 1600),
                  craft = c(40, 100, 400, 1600, 400, 800, 1600, 3200)),
             function(x, names) {
                 names(x) = names
                 return(x)},
             names = rarities)
