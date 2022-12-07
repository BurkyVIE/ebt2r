stash_read <- function() {
  
  # Library ----
  library(tidyverse)
  
  # Einlesen des Files ----
  # tmp <- read_lines(paste0(EBT_global$whereis, "/_REC/notes2enter.txt"),
                    # locale = locale(encoding = "WINDOWS-1252"),
  tmp <- read_lines("_REC/notes2enter.txt",
                    locale = locale(encoding = "UTF-8"),
                    lazy = FALSE)
  
  # Initialisiere Schleife ----
  stash <- NULL
  i <- 1
  
  # Zeilenweise abarbeiten ----
  while(i <= length(tmp)) {
    if(str_starts(tmp[i], "<202")) {
      loc <- tmp[i+1]
      i <- i+3
    } else {
      if(str_starts(tmp[i], "1|2|5"))
        stash <- rbind(stash, cbind(Loc = loc, Note = tmp[i]))
      i <- i+1
    }
  }
  
  # Rückschreiben Objekt ----
  stash <- as_tibble(stash) %>%
    separate(Loc, sep = ", ", into = c("City", "Country", "ZIP"), remove = FALSE) |> 
    separate(Note, sep = " ", into = c("Value", "Printer", "Serial", "Comment"), extra = "merge", remove = FALSE) |> 
    mutate(Value = as.integer(Value),
           Stashed = lubridate::dmy(str_sub(Comment, -10, -1))) %>% 
    relocate(c(Loc, Note), .after = Stashed)
  stash <<- stash
  
  # Kurze Auswertungen ----
  cat("stash:\n")
  print(stash |>  count(Stashed, ZIP) |>  as.data.frame())
  cat("\n")
  cat(paste0(count(stash), " bills stashed"))
  cat("\n")
  cat(paste0("duplicates = ", stash |>  pull(Serial) |>  duplicated() |>  any()))
  cat("\n")
}


# - - - - - - - - - -

stash_split <- function (chunks = 14, size = NULL) {
  
  # Library ----
  library(tidyverse)
  
  # Einlesen Daten ----
  stash_read()
  
  # Berechne chunksize, wenn nicht gegeben ---> wenn chunksize gegeben wird chunknumber ignoriert! ----
  if(is.null(size)) size = count(stash) %/% chunks
  
  # Dateinamen
  # main <- paste0(EBT_global$whereis, "/_REC/notes2enter.txt")
  # sicher <- paste0(EBT_global$whereis, "/_REC/notes2enter_sicherung.txt")
  main <- "_REC/notes2enter.txt"
  sicher <- "_REC/notes2enter_sicherung.txt"
  
  # Umbennen Originalfile (Sicherung) ----
  file.copy(from = main, to = sicher, overwrite = TRUE)
  
  # Initialisierung ----
  file.create(main)
  nc = 1 # Numbercount (auch Schleife)
  bc = 1 # Blockcount 
  loc = FALSE # Location breaker
  export = NULL # Aufbau File vor Schreiben

  # Verlauf ----
  cat("                   <---'----'----'----'---->\nsplitting stash... ")
  
  # Zeilenweises Schreiben und Aufteilung ZIP, Blockgröße max 25 ----
  while(nc <= count(stash)) {
    if(nc %% size == 1 | bc %% 25 == 1 | !loc) {
      export <- c(export,
                  paste0("\n<202_-__-__ __:__:__>     ", ifelse(nc %% size == 1, "Start ...\n", "... continue\n"),
                         pull(stash[nc, "Loc"]),
                         "\n- - - - - - - - - - -"))
      bc <- 1
    }
    export <- c(export, pull(stash[nc, "Note"]))
    loclast <- pull(stash[nc, "Loc"])
    if(nc %% floor(count(stash) / 25) == 0) cat ("°")
    nc <- nc + 1
    bc <- bc + 1
    loc <- loclast == pull(stash[nc, "Loc"])
  }
  
  cat(" DONE!\nwriting file ...") # schließt Verlauf ab
  
  # Schreiben ----
  write(x = export, file = main)
  
  nc <- nc - 1 # Korrektur für folgende Berechnungen
  cat(paste0(" DONE!\n\n", nc, " bills were split into ", nc %/% size, " chunks á ", size, ", residue ", nc %% size))
  beepr::beep(2)
}
