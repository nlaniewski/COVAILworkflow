#a batch-assigned manifest as prepared and saved by:
#COVAILworkflow::BLIS.manifest.prepare()
#COVAILworkflow::manifest.batch.assign()

COVAIL_manifest_batched.path<-list.files(
  system.file("extdata",package="COVAILworkflow"),
  pattern="COVAIL_manifest_batched", full.names = TRUE
)

invisible(
  file.remove(
    list.files(
      tempdir(),
      full.names = TRUE,
      pattern = "COVAIL_[0-9]{3}_workflow.xlsx")
  )
)

COVAILworkbook.initialize(
  covail.manifest.batched.xlsx = COVAIL_manifest_batched.path,
  batch.name = "COVAIL_001",
  workbook.dir = tempdir()
)

list.files(tempdir(),pattern = "COVAIL_[0-9]{3}_workflow.xlsx")
# shell.exec(list.files(tempdir(),pattern = "COVAIL_[0-9]{3}_workflow.xlsx",full.names = TRUE))
