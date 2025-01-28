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

#COVAILworkbooks; initialized
#representative batches: "COVAIL_001" and "COVAIL_040"
invisible(
  lapply(paste0("COVAIL_",c("001","040")),function(covail.batch){
    COVAILworkbook.initialize(
      covail.manifest.batched.xlsx = COVAIL_manifest_batched.path,
      batch.name = covail.batch,
      workbook.dir = tempdir()
    )
  })
)

#three sheets per workbook
lapply(
  list.files(tempdir(),full.names=TRUE,pattern = "workflow"),
  openxlsx::getSheetNames
)

#update the workbooks
invisible(
  lapply(
    list.files(tempdir(),full.names=TRUE,pattern = "workflow"),
    COVAILworkbook.cellcounts,
    cellometer.counts.path = list.files(
      system.file("extdata",package="COVAILworkflow"),
      full.names = TRUE,
      pattern="cellometer_counts",
    ),
    rest.volume.ml = 2.2
  )
)

#updated workbooks
#three sheets from COVAILworkbook.initialize(...)
#two additional sheets from COVAILworkbook.cellcounts(...)
lapply(
  list.files(tempdir(),full.names=TRUE,pattern = "workflow"),
  openxlsx::getSheetNames
)
# shell.exec(list.files(tempdir(),pattern = "COVAIL_040_workflow.xlsx",full.names = TRUE))
