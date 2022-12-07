stash_read <- function() {
  
  # Library ----
  library(tidyverse)
  
  # Einlesen des Files ----
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
  main <- "_REC/notes2enter.txt"
  sicher <- "_REC/notes2enter_sicherung.txt"
  
  # Umbennen Originalfile (Sicherung) ----
  file.copy(from = main, to = sicher, overwrite = TRUE)
  
  # Initialisiere ----
  stash_exp <- transmute(stash,
                         Loc, Note, Stashed,
                         nr = row_number(),
                         nc = nr %/% size,
                         nb = nr %/% 25) |> 
    group_by(Loc, Stashed) |> 
    mutate(loc = cur_group_id()) |> 
    ungroup() |> 
    group_split(nc, nb, loc)

  # Export Funktion ----
  export_stash <- function(x){
    ret <- NULL
    ret <- c(ret,
             paste0("<202_-__-__ __:__:__>     ", ifelse(x[1, "nr"] == 1, "S T A R T ...\n", "... continue\n"),
                    pull(x[1, "Loc"]),
                    "\n- - - - - - - - - - -"))
    ret <- c(ret, pull(x[, "Note"]), "\n")
    return(ret)
    }

  # Erzeuge Rückgabe ----
  export <- unlist(lapply(stash_exp, export_stash))

  # Schreiben ----
  cat("\nwriting file...")
  write(x = export, file = main)
  
  # Rückmeldung
  cat(paste0("DONE!\n\n", count(stash), " bills were split into ", count(stash) %/% size, " chunks á ", size, ", residue ", count(stash) %% size))
  beepr::beep(2)
}
