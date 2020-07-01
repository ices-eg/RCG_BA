extractCodes <- function(codes, codeList, removeNames = T){
  res <- sapply(codes, function(x){
    codeList[[x]]
  })
  if(removeNames) res <- unname(res)
  
  return(res)
}

spCharCodes2intCodes <- function(charCodes, removeNames=T){
  codeList <- list(
    HER = 126417,
    SPR = 126425,
    COD = 126436,
    MAC = 127023,
    WHB = 126439,
    FLE = 127141,
    POK = 126441,
    SAN = 125909,
    GUG = 150637,
    NOP = 126444,
    TUR = 127149,
    POL = 126440
    
    
  )
  #TODO add codes or read from external file API etc.
  res <- extractCodes(charCodes, codeList, removeNames)
  
  return(res)
}



spIntCodes2SciNames <- function(intCodes, removeNames=T){
  intCodes <- as.character(intCodes)
  codeList <- list(
    `126436` = "Gadus morhua",
    `126417` = "Clupea harengus",
    `126425` = "Sprattus sprattus",
    `126438` = "Merlangius merlangus",
    `126505` = "Gasterosteus aculeatus",
    `127214` = "Cyclopterus lumpus",
    `254529` = "Myoxocephalus quadricornis",
    `125476` = "Gasterosteidae",
    `127023` = "Scomber scombrus",
    `105923` = "Squalus acanthias",
    `126439` = "Micromesistius poutassou",
    `126441` = "Pollachius virens",
    `127141` = "Platichthys flesus",
    `125909` = "Ammodytes",
    `150637` = "Eutrigla gurnardus",
    `127149` = "Scophthalmus maximus",
    `154210` = "Esox lucius",
    `126444` = "Trisopterus esmarkii",
    `127143` = "Pleuronectes platessa",
    `126375` = "Belone belone",
    `126437` = "Melanogrammus aeglefinus",
    `127150` = "Scophthalmus rhombus",
    `126822` = "Trachurus trachurus",
    `127160` = "Solea solea",
    `126555` = "Lophius piscatorius",
    `126484` = "Merluccius merluccius",
    `127140` = "Microstomus kitt",
    `126461` = "Molva molva",
    `125517` = "Anarhichadidae",
    `11734` = "Loliginidae",
    `127138` = "Hippoglossus hippoglossus",
    `126440` = "Pollachius pollachius",
    `10194` = "Actinopterygii",
    `126139` = "Coregonus"
  )
  #TODO add codes or read from external file API etc.
  
  res <- extractCodes(intCodes, codeList, removeNames)
  
  return(res)
}
