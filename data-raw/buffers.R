buffers<-rbind(
  c(
    Buffer ="Staining Buffer",
    'Buffer Alias' = "SB",
    Constituents = "1x PBS + 2% FBS (490 mL PBS + 10 mL FBS); 0.2 um filter sterilized"
  ),
  c(
    Buffer ="Staining Buffer + Universal Nuclease",
    'Buffer Alias' = "SBn",
    Constituents = "SB + 1:10,000 Universal Nuclease"
  ),
  c(
    Buffer ="Viability Buffer",
    'Buffer Alias' = "VB",
    Constituents = "1 part SBn + 3 parts 1x PBS; 0.5% FBS final"
  ),
  c(
    Buffer ="BD Cytofix/CytoPerm",
    'Buffer Alias' = "Fix/Perm",
    Constituents = "BD Cytofix/CytoPerm; use as provided (1x solution)"
  ),
  c(
    Buffer ="BD Permeabilization Buffer (1x)",
    'Buffer Alias' = "PB",
    Constituents = "10x BD Permeabilization Buffer diluted to 1x in UltraPure Distilled Water"
  )
)
##
# c(
#   Buffer ="",
#   'Buffer Alias' = "",
#   Constituents = ""
##
buffers<-data.table::data.table(buffers)
##
usethis::use_data(buffers, overwrite = TRUE, internal = TRUE)
