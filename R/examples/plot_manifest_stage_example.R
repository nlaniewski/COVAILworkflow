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

stage.boxes<-plot_manifest.stage(COVAIL_manifest_batched)
stage.boxes[[1]]
stage.boxes[[41]]
