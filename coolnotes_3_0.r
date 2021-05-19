coolnotes <- function(data = notes, Serial = SerialPlain, encore = TRUE) {#, count = 7) {
  
  library(tidyverse)
  Serial <- enquo(Serial)
  
  # Function to do the calculation
  calculate <- function(data, Serial, encore = TRUE) {
    
    Serial <- enquo(Serial)
    
    tmp <- data %>%
      mutate(DigitVector = map(!!Serial, ~as.integer(str_extract_all(., "\\d", simplify = TRUE))),
             Solid = map_dbl(!!Serial, ~max(0, nchar(str_extract_all(., "(\\d)\\1+", simplify = TRUE)), na.rm = TRUE)), # digit followed by at least one identical
             LadderUp = map(DigitVector, ~c(0, diff(.))),
             LadderDown = map(DigitVector, ~c(0, diff(rev(.)))), # LadderDown is LadderUp backwards 'rev'
             Unique = map_int(DigitVector, ~length(unique(.))), # Binary/Trinary: length of unique; or dim(table(.)) == 2
             Radar = map_lgl(DigitVector, ~identical(.[1:5], rev(.)[1:5])), # Radar is identical first 5 and reversed last five digits
             Binary = Unique == 2) # Binary/Trinary: length of unique; or dim(table(.)) == 2
    
    # Encore (i.e. ladder does not end at 9 resp 0; -> 8, 9, 0, 1, 2 <-)
    if(encore)
      tmp <- tmp %>%
      # mutate(across(c(LadderUp, LadderDown), ~map(., ~replace(., . == -9, 1)))) # replace -9 for encore
      mutate(LadderUp = map(LadderUp, ~replace(., . == -9, 1)),
             LadderDown = map(LadderDown, ~replace(., . == -9, 1)))
    
    # finally get the Ladder done
    tmp <- tmp %>%
      # mutate(across(c(LadderUp, LadderDown), ~map_dbl(., ~max(table(cumsum(. != 1)))))) %>%  # cumsum steps up by 1 if diff >1, use table to get most common non-difference
      mutate(LadderUp = map_dbl(LadderUp, ~max(table(cumsum(. != 1)))),
             LadderDown = map_dbl(LadderDown, ~max(table(cumsum(. != 1))))) %>% 
      rowwise() %>%
      mutate(Ladder = max(LadderUp, LadderDown)) %>%
      select(-contains(c("Up", "Down", "Vector")))  # remove helping variables
    
    # one is not a value for Solid and Ladder and make integer
    tmp <- tmp %>% 
      mutate(Solid = as.integer(replace(Solid, Solid == 1, 0)),
             Ladder = as.integer(replace(Ladder, Ladder == 1, 0))) %>% 
      select(Serial = !!Serial, Solid, Ladder, Unique:Binary)
    
    return(unique(tmp))
  }
  
  # iitialise if non existent
  if(!exists("coolnotes_stored")) coolnotes_stored <<- calculate(data = data, Serial = !!Serial, encore = encore)
  
  # find not yet calculated data
  by = set_names("Serial", quo_name(Serial)) # create named pairs for join (by)
  
  work <- anti_join(data, coolnotes_stored, by = by)  # not yet calculated part of data ...
  if(count(work) > 0) {                               # if positive
    new <- calculate(work, !!Serial, encore = encore) # ... now calculated
    coolnotes_stored <<- bind_rows(coolnotes_stored, new)
  }
  
  # join data an calculations, add rating
  tmp <- data %>%
    select(!!Serial, any_of(c("Value", "PrinterPlain", "Printer", "NoteID", "DateStamp", "Stashed"))) %>%
    left_join(., coolnotes_stored, by = by) %>%
    mutate(Rating = (Solid/10)**2 + .9 * (Ladder/10)**3 + .75 * (1-Unique/10)**5 + .25 * Radar + .25 * Binary)
  
  # return only cool bills
  tmp <- tmp %>%
#    filter(Solid >= count | Ladder >= count | Radar | Binary) %>%
    filter(Rating >= .5 | Radar | Binary) %>%
    arrange(desc(Rating)) %>%
    relocate(c(Solid, Ladder, Unique:Rating), .after = !!Serial)
  
  return(tmp)
  
}
