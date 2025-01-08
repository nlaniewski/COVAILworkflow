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

#cell counts file (.csv) from a Cellometer K2
COVAIL_cellometer_counts_path<-list.files(
  system.file("extdata",package="COVAILworkflow"),
  pattern="cellometer_counts", full.names = TRUE
)

manifest.cell.counts(
  manifest.collapsed = COVAIL_manifest_batched,
  cellometer.counts.path = COVAIL_cellometer_counts_path,
  rest.volume.ml = 2.2
)
