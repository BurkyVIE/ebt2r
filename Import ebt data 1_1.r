#
#
### THEME_EBT
library(ggplot2)
theme_ebt <- function()
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(size = 16, face = "bold", color = rgb(45, 56, 81, maxColorValue = 255)),
        plot.background = element_rect(fill = rgb(221, 226, 233, maxColorValue = 255), color = NA),
        strip.background = element_rect(fill = rgb(186, 194, 207, maxColorValue = 255), color = NA),
        panel.grid = element_line(color = "white"))



library(tidyverse)

#
#
### GLOBAL

EBT_global <- list(
  whereis = switch(Sys.info()[4],
                   "BURKY-TOP" = "C:/Users/Thomas/Eurobanknotes/",
                   "BURKY-CORE" = "C:/Users/Thomas/Eurobanknotes/",
                   "BURKY-ULTRA" = "C:/Users/Thomas/Eurobanknotes/",
                   "C:/Eurobanknotes/"),
  
  series = tribble(~Value, ~Copyright, ~Series,
                   5L, 2002L, "ES-1",
                   10L, 2002L, "ES-1",
                   20L, 2002L, "ES-1",
                   50L, 2002L, "ES-1",
                   100L, 2002L, "ES-1",
                   200L, 2002L, "ES-1",
                   500L, 2002L, "ES-1",
                   5L, 2013L, "ES-2",
                   10L, 2014L, "ES-2",
                   20L, 2015L, "ES-2",
                   50L, 2017L, "ES-2"),
  
  printer = tribble(~PrinterCode, ~Series, ~PrinterShort,
                    "D", "ES-1", "FI",
                    "E", "ES-1", "FR",
                    "F", "ES-1", "AT",
                    "G", "ES-1", "NL",
                    "H", "ES-1", "UK",
                    "J", "ES-1", "IT",
                    "K", "ES-1", "IE",
                    "L", "ES-1", "FR",
                    "M", "ES-1", "ES",
                    "N", "ES-1", "GR",
                    "P", "ES-1", "DE",
                    "R", "ES-1", "DE",
                    "T", "ES-1", "BE",
                    "U", "ES-1", "PT",
                    "E", "ES-2", "FR",
                    "M", "ES-2", "PT",
                    "N", "ES-2", "AT",
                    "P", "ES-2", "NL",
                    "R", "ES-2", "DE",
                    "S", "ES-2", "IT",
                    "T", "ES-2", "IE",
                    "U", "ES-2", "FR",
                    "V", "ES-2", "ES",
                    "W", "ES-2", "DE",
                    "X", "ES-2", "DE",
                    "Y", "ES-2", "GR",
                    "Z", "ES-2", "BE"),
  
  country = tribble(~IssuerCode, ~Series, ~IssuerShort,
                    "D", "ES-1", "EE",
                    "E", "ES-1", "SK",
                    "F", "ES-1", "MT",
                    "G", "ES-1", "CY",
                    "H", "ES-1", "SI",
                    "L", "ES-1", "FI",
                    "M", "ES-1", "PT",
                    "N", "ES-1", "AT",
                    "P", "ES-1", "NL",
                    "S", "ES-1", "IT",
                    "T", "ES-1", "IE",
                    "U", "ES-1", "FR",
                    "V", "ES-1", "ES",
                    "X", "ES-1", "DE",
                    "Y", "ES-1", "GR",
                    "Z", "ES-1", "BE",
                    "E", "ES-2", "FR",
                    "M", "ES-2", "PT",
                    "N", "ES-2", "AT",
                    "P", "ES-2", "NL",
                    "R", "ES-2", "DE",
                    "S", "ES-2", "IT",
                    "T", "ES-2", "IE",
                    "U", "ES-2", "FR",
                    "V", "ES-2", "ES",
                    "W", "ES-2", "DE",
                    "X", "ES-2", "DE",
                    "Y", "ES-2", "GR",
                    "Z", "ES-2", "BE"),
  
  countrynames = tribble(~Short, ~Country,
                        "AT", "Austria",
                        "BE", "Belgium", 
                        "CY", "Cyprus",
                        "DE", "Germany",
                        "EE", "Estonia",
                        "ES", "Spain",
                        "FI", "Finland",
                        "FR", "France",
                        "GR", "Greece",
                        "IE", "Ireland",
                        "IT", "Italy",
                        "MT", "Malta",
                        "NL", "Netherlands",
                        "PT", "Portugal",
                        "SI", "Slovenia",
                        "SK", "Slovakia",
                        "UK", "United Kingdom",
                        "CZ", "Czech Republic",
                        "HU", "Hungary",
                        "VA", "Vatican City",
                        "AE", "United Arab Emirates",
                        "CH", "Switzerland",
                        "SM", "San Marino",
                        "EG", "Egypt",
                        "ME", "Montenegro",
                        "SZ", "Swaziland",
                        "LU", "Luxembourg",
                        "AU", "Australia",
                        "NZ", "New Zealand",
                        "MC", "Monaco",
                        "PL", "Poland",
                        "NO", "Norway",
                        "US", "United States")
) # End of List


#
#
### NOTES

# Einlesen Rohdaten
raw <- read_delim(file = "C:/Users/Thomas/Eurobanknotes/script/EBT-Bills.csv",
                  delim = ";",
                  skip = 1,
                  col_names = c("Value", "Copyright", "SerialPlain", "Comment", "DateStamp",
                                "EntryCity", "EntryCountry", "EntryZIP", "PrinterPlain", "NoteID",
                                "TimesEntered", "Mod", "Lat", "Long"),
                  col_types = list(col_integer(), col_integer(), col_character(), col_character(), col_datetime(),
                                   col_character(), col_character(), col_character(), col_character(), col_integer(),
                                   col_integer(), col_logical(), col_double(), col_double()),
                  progress = TRUE)

cat(paste0("...EBT> READ ",raw %>% count()," notes/lines\n"))

# Ergänze Serie
notes <- raw %>%
  left_join(EBT_global$series, by = c("Value", "Copyright"))


# Ergänze Printer
notes <- notes %>%
  mutate(PrinterCode = substr(PrinterPlain, 1, 1)) %>%
  left_join(EBT_global$printer, by = c("Series", "PrinterCode"))

# Ergänze Land
notes <- notes %>%
  mutate(IssuerCode = substr(SerialPlain, 1, 1)) %>%
  left_join(EBT_global$country, by = c("Series", "IssuerCode"))

notes <- notes %>% left_join(EBT_global$countrynames, by = c("PrinterShort"  = "Short")) %>% rename(PrinterCountry = Country)
notes <- notes %>% left_join(EBT_global$countrynames, by = c("IssuerShort"  = "Short")) %>% rename(IssuerCountry = Country)
notes <- notes %>% left_join(EBT_global$countrynames, by = c("EntryCountry"  = "Country")) %>% rename(EntryShort = Short)

notes <- notes %>% select(Value, Copyright, Series,
                          PrinterPlain, PrinterCode, PrinterCountry, PrinterShort,
                          SerialPlain, IssuerCode, IssuerCountry, IssuerShort,
                          DateStamp, EntryCountry, EntryShort, EntryZIP, EntryCity,
                          TimesEntered, Long, Lat, NoteID, Mod)

cat("...EBT> SUCCESSfully created 'notes'\n")

rm(raw)


#
#
### HITS

# Einlesen Rohdaten
raw <- read_lines(file = "script/EBT-Hits.csv", skip = 1, progress = TRUE)

# Finde Trennstelle
splitter <- which(raw == "")

# Trefferdaten
hits1 <- raw[1:(splitter - 1)] %>% enframe(name = NULL) %>%
  separate(value, into = c("Value", "SerialPlain", "PrinterPlain", "Copyright", "NoteID", "DateStamp", "TimesEntered", "Mod", "Km", "Days", "LongVector", "LatVector"),
           sep = ";", remove = TRUE)

# Übernehme Informationenen aus den Trefferdetails (insb. für fixes Trefferdatum = mein Schein wurde zum Treffer bzw. ich mache einen Treffer; weiteres Finden wird ignoriert)
hits2 <- raw[-(1:splitter)] %>%
  enframe(name = NULL) %>%
  mutate(value = gsub(pattern = "\\\".*\\\"", replacement = "bad comment", value)) %>% 
  separate(value, into = c("Value", "SerialPlain", "PrinterPlain", "Copyright", "NoteID", "DateStamp", "Comment", "EntryCountry", "EntryCity", "EntryZIP", "UserID", "UserName", "Long", "Lat", "Km", "Days"),
           sep = ";", remove = TRUE) %>% 
  select(Value, SerialPlain, Copyright, NoteID, DateStamp, UserID, UserName, Long, Lat) %>% 
  transmute(Value = parse_integer(Value),
            SerialPlain = SerialPlain,
            Copyright = parse_integer(Copyright),
            NoteID = parse_integer(NoteID),
            DateStamp = parse_datetime(DateStamp),
            UserID = parse_integer(UserID),
            UserName = UserName,
            Long = parse_double(Long),
            Lat = parse_double(Lat)) %>% 
  nest(DateStamp, NoteID, UserID, UserName, Lat, Long, .key = HitData) %>% 
  mutate(NoteID = map_int(.x = HitData, .f = ~ .$NoteID[.$UserID == 32954]),
         MeIsNo = map_int(.x = HitData, .f = ~ which(.$UserID == 32954)),
         DateFixed = purrr::map(.x = HitData, .f = ~ .[max(2L, which(.$UserID == 32954)), 1]) %>% unlist(),
         DateFixed = lubridate::as_datetime(DateFixed, origin = "1970-01-01 00:00:00") %>% lubridate::date())

n <- c(hits1 %>% count() %>% pull(), hits1 %>% filter(Mod == 1) %>% count() %>% pull())
cat(paste("...EBT> READ", n[1], "hits/lines\n"))

# Schließe moderierte aus
hits <- hits1 %>%
  filter(Mod == 0)

cat(paste0("...EBT> REMOVED ", n[2], " moderated hits (", format(round(n[2] / n[1] * 100, 2), digits = 2, nsmall = 2)," %)\n"))
rm(n)

hits <- hits %>% 
  transmute(DateStamp = parse_datetime(DateStamp),
            Value = parse_integer(Value),
            Km = parse_integer(Km),
            Days = parse_integer(Days),
            TimesEntered = parse_integer(TimesEntered),
            NoteID = parse_integer(NoteID)) %>%
  arrange(DateStamp) %>%
  left_join(y = hits2, by = "NoteID") %>% 
  select(DateStamp, DateFixed, NoteID, Value = Value.x, TimesEntered, MeIsNo, Km, Days, HitData)

cat("...EBT> SUCCESSfully created 'hits'\n")

rm(splitter, raw, hits1, hits2)

source(paste0(EBT_global$whereis, "make mds ex notes.r"))
cat("...EBT> SUCCESSfully saved 'ebt_mds.txt'\n")

source(paste0(EBT_global$whereis, "georgescore ex notes.r"))

source(paste0(EBT_global$whereis, "overview ex notes.r"))
cat("...EBT> SUCCESSfully created 'graph.png'    ... 1 of 4\n")

source(paste0(EBT_global$whereis, "overview_gg ex notes.r"))
cat("...EBT> SUCCESSfully created 'graph_gg.png' ... 2 of 4\n")

source(paste0(EBT_global$whereis, "trend ex mds.r"))
cat("...EBT> SUCCESSfully created 'trend.png'    ... 3 of 4\n")

source(paste0(EBT_global$whereis, "dotmap ex_notes.r"))
cat("...EBT> SUCCESSfully created 'dotmap.png'   ... 4 of 4\n")

source(paste0(EBT_global$whereis, "stdtable ex notes.r"))
cat("...EBT> SUCCESSfully initialised 'stdtable()'\n")

sapply(dir(paste0(EBT_global$whereis,"modulares erfassen"), pattern = "_", full.names = TRUE), source)
cat("...EBT> SUCCESSfully initialised 'modulares erfassen'\n")