reagents<-rbind(
  c(
    Reagent ="FBS",
    Manufacturer = "Avantor Seradigm",
    Catalog = "97068-085",
    href = "https://export.vwr.com/store/product?keyword=97068-085",
    Note = "Fetal Bovine Serum; heat-inactivated; previously aliquoted"
  ),
  c(
    Reagent ="PBS",
    Manufacturer = "Gibco",
    Catalog = "10010023",
    href = "https://www.thermofisher.com/order/catalog/product/10010023",
    Note = "Phosphate Buffered Saline"
  ),
  c(
    Reagent ="Universal Nuclease",
    Manufacturer = "Pierce",
    Catalog = "88702",
    href = "https://www.thermofisher.com/order/catalog/product/88702",
    Note = ""
  ),
  c(
    Reagent ="UltraPure Distilled Water",
    Manufacturer = "Invitrogen",
    Catalog = "10977015",
    href = "https://www.thermofisher.com/order/catalog/product/10977015",
    Note = ""
  ),
  c(
    Reagent ="BD Cytofix/Cytoperm",
    Manufacturer = "BD Biosciences",
    Catalog = "554722",
    href = "https://www.bdbiosciences.com/en-us/products/reagents/flow-cytometry-reagents/research-reagents/single-color-antibodies-ruo/fixation-and-permeabilization-solution.554722?tab=product_details",
    Note = ""
  ),
  c(
    Reagent ="BD Perm/Wash Buffer",
    Manufacturer = "BD Biosciences",
    Catalog = "554723",
    href = "https://www.bdbiosciences.com/en-us/products/reagents/cell-preparation-separation-reagents/blood-lysis/staining-and-cell-preparation/perm-wash-buffer.554723?tab=product_details",
    Note = "10X solution; diluted 1:10 using UltraPure Distilled Water"
  )
)
##
# c(
#   Reagent ="",
#   Manufacturer = "",
#   Catalog = "",
#   href = "",
#   Note = ""
# )
##
reagents<-data.table::data.table(reagents)
##
usethis::use_data(reagents, overwrite = TRUE, internal = TRUE)
