# --------------------------------------------------------------
# define paths, load packages, load functions
# --------------------------------------------------------------
packs <- c("mstate", "flexsurv", "muhaz")    # smoothHazard

for (i in 1:length(packs)){library(packs[i], character.only = TRUE)}

path <- paste(getwd(), "/simulate_PFS_OS/", sep = "")

# --------------------------------------------------------
# input functions
# --------------------------------------------------------
source(paste(path, "functions/derivePFS_OS_events.r", sep = ""), echo = FALSE)
source(paste(path, "functions/simulateIDM.r", sep = ""), echo = FALSE)

