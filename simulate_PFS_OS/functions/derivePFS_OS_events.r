derivePFS_OS_events <- function(sim){
  
  require("plyr")

  pd <- with(subset(sim, to == 2), data.frame(
      "id" = id,
      "pdtime" = duration, 
      "pd" = status
  ))

  death <- with(ddply(subset(sim, to == 3), .(id), tail, 1), 
              data.frame(
                "id" = id,
                "deathtime" = Tstop, 
                "death" = status
              ))

  tte <- merge(pd, death, by = "id")
  tte <- with(tte, data.frame(tte,
                            "pfs" = pmin(pdtime, deathtime),
                            "pfsfail" = ifelse((pd == 1) | (pdtime == deathtime & death == 1), 1, 0),
                            "os" = deathtime,
                            "osfail" = death
  ))

return(tte)
}