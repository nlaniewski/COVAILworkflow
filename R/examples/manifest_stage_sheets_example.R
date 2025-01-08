#a batch-assigned manifest as prepared by:
#COVAILworkflow::BLIS.manifest.prepare()
#COVAILworkflow::manifest.batch.assign()

COVAIL_manifest_batched.path<-list.files(
  system.file("extdata",package="COVAILworkflow"),
  pattern="COVAIL_manifest_batched", full.names = TRUE
)

COVAIL_manifest_batched<-data.table::setDT(
  openxlsx::read.xlsx(
    COVAIL_manifest_batched.path,sheet="manifest_collapsed"
  )
)

invisible(
  file.remove(
    list.files(tempdir(),full.names = TRUE,pattern = "COVAIL_[0-9]{3}.xlsx")
  )
)

manifest.stage.sheets(
  COVAIL_manifest_batched[stage.name=="COVAIL_001"],
  workbook.dir = tempdir()
)

list.files(tempdir(),pattern = "COVAIL_[0-9]{3}.xlsx")
# shell.exec(list.files(tempdir(),pattern = "COVAIL_[0-9]{3}.xlsx",full.names = TRUE))
