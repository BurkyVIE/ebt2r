rate_serial <- function(serial = "XY0123456789") {
  DigitVector = as.integer(str_extract_all(serial, "\\d")[[1]])
  Length = length(DigitVector)
  D1_raw = abs(diff(DigitVector))
  D1_raw[D1_raw > 5] = 10 - D1_raw[D1_raw > 5]
  D1_raw = as.integer(D1_raw)
  D1 = 1 - sum(D1_raw) / 5 / (Length - 1)
  D2_raw = as.integer(abs(diff(D1_raw)))
  D2 = 1 - sum(D2_raw) / 5 / (Length - 2)
  Unique = length(unique(DigitVector))
  UF = c(2, 1, .5, .25, .125, 0, 0, 0, 0, .5)[Unique]
  Radar = identical(DigitVector[1:5], rev(DigitVector)[1:5])
  Binary = Unique == 2
  
  return(
    tibble(Serial = serial,
           DigitVector = list(DigitVector),
           Length = Length,
           D1_raw = list(D1_raw),
           D1 = D1,
           D2_raw = list(D2_raw),
           D2 = D2,
           Unique = Unique,
           UF = UF,
           Radar = Radar,
           Binary = Binary,
           Rating2 = (2 * D1 + D2 + UF) * (1 + .5 * (Radar | Binary)) / 4
    )
  )
}
