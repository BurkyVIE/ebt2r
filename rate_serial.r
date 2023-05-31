rate_serial <- function(serial = "XY0123456789") {
  DigitVector = as.integer(str_extract_all(serial, "\\d")[[1]])
  Length = length(DigitVector)
  D1_raw = as.integer(abs(diff(DigitVector)))
  D1_raw = ifelse(D1_raw>5, 10-D1_raw, D1_raw)
  Solid = as.integer(max(0, nchar(str_extract_all(serial, "(\\d)\\1+")[[1]]), na.rm = TRUE))
  Ladder = max(table(cumsum(D1_raw != 1)))
  if(Ladder == 1) Ladder = 0
  Unique = length(unique(DigitVector))
  Radar = identical(DigitVector[1:5], rev(DigitVector)[1:5])
  Binary = Unique == 2
  D1 = 1 - sum(D1_raw) / 5 / (Length - 1)
  D2_raw = as.integer(abs(diff(D1_raw)))
  D2 = 1 - sum(D2_raw) / 5 / (Length - 2)
  UF = c(2, 1, .5, .25, .125, 0, 0, 0, 0, .5)[Unique]
  
  return(
    tibble(Serial = serial,
           # DigitVector = list(DigitVector),
           Length = Length,
           # D1_raw = list(D1_raw),
           D1 = D1,
           # D2_raw = list(D2_raw),
           D2 = D2,
           UF = UF,
           Solid = Solid,
           Ladder = Ladder,
           Unique = Unique,
           Radar = Radar,
           Binary = Binary,
           Rating = Solid/10 + Ladder/10 + (1-Unique/10) + .3 * Radar + .5 * Binary,
           Rating2 = (4 * D1 ** 2 + D2 + UF) * (1 + .5 * (Radar | Binary)) / 5
    )
  )
}
