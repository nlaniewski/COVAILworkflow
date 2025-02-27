#' @title Generate printable batch-specific worksheets
#'
#' @description
#' Using the result of \link{manifest.batch.assign}, generate printable batch-specific worksheets: stage vials, a stage vials box map (9x9), and a 'Day 1: Thaw' sheet. This pre-organization step allows for confirmation of sample vial availability and correct metadata (unique identifier) and expedites assay workflow. Sheets are to be printed, signed, and hole-punched for storage in study-specific binder.
#'
#'
#' @param covail.manifest.batched.xlsx file.path (character string); .xlsx (Excel) workbook as saved from \link{manifest.batch.assign}. An expected MD5 hash is encoded in this function and will return an error/stop message if invalidated.
#' @param batch.name Character string of the form "COVAIL_###"; ### currently can have the string value of "001" through "041".
#' @param workbook.dir file.path (character string); batch-specific .xlsx (Excel) workbook will be written to the directory.
#' @param .overwrite logical; default `FALSE`. If `TRUE`, will overwrite an existing batch-specific workbook.
#'
#' @return a batch-specific .xlsx (Excel) workbook with three sheets:
#' \enumerate{
#'    \item stage vials (sample vials); banded-row table
#'    \item stage vials box map; embedded .png plot of a 9x9 box
#'    \item Day 1: Thaw; banded-row table
#' }
#' @export
#'
#' @example /R/examples/COVAILworkbook_initialize_example.R
COVAILworkbook.initialize<-function(
    covail.manifest.batched.xlsx=NULL,
    batch.name=NULL,
    workbook.dir=NULL,
    .overwrite=FALSE
){
  ##
  .SD<-NULL
  ##MD5 hash to ensure reproducibility
  # if(tools::md5sum(covail.manifest.batched.xlsx)!="71a2f44bf9ef106720b4781ef2b9a9cd"){
  #   stop("COVAIL manifest (batch assigned) MD5 hash does not match historic value; has something changed?")
  # }
  if(is.null(batch.name)){
    stop("Define the 'batch.name' argument; 'batch.name=COVAIL_###' (### currently equals '001' through '041')")
  }
  ##issue with pluralized names from old code; hacky fix here until it can be rooted out
  cols<-c(
    'g.id'
    ,'subject.id'
    ,'visit'
    ,'subject.id.alias'
    ,'visit.alias'
    ,'sample.id'
    ,'freezer'
    ,'rack.location'
    ,'box.label'
    ,'box.row.col'
    ,'batch.seq'
    ,'batch.order'
    ,'stage.name'
    ,'stage.position'
  )
  cols.plural<-paste0(cols,"s")

  dt<-data.table::setDT(
    openxlsx::read.xlsx(
      covail.manifest.batched.xlsx,
      sheet = "manifest_collapsed"
    )
  )[stage.name==batch.name]

  cols.keep<-names(dt)[names(dt) %in% c(cols,cols.plural)]

  dt<-dt[,.SD,.SDcols=cols.keep]
  ##
  # dt<-data.table::setDT(
  #   openxlsx::read.xlsx(
  #     covail.manifest.batched.xlsx,
  #     sheet = "manifest_collapsed"
  #   )
  # )[stage.name==batch.name,
  #   .(
  #     g.ids
  #     ,subject.id
  #     ,visit
  #     ,subject.id.alias
  #     ,visit.alias
  #     ,sample.id
  #     ,freezer
  #     ,rack.location
  #     ,box.label
  #     ,box.row.cols
  #     ,batch.seq
  #     ,batch.order
  #     ,stage.name
  #     ,stage.position
  #   )
  # ]
  plot.boxmap<-plot_manifest.stage(dt)[[1]]
  ##create an excel workbook
  ##three sheets: banded-row table (pull/stage list); boxmap plot; Day 1: Thaw
  wb <- openxlsx::createWorkbook()
  sheet.names<-c("stage_vials","stage_box","Day1_Thaw")
  sheet.names<-sapply(seq(sheet.names),function(i){sprintf("%s_%s_%s",i,sheet.names[i],batch.name)})
  ##add the sheets; set a header
  invisible(
    lapply(seq(sheet.names),function(i){
      openxlsx::addWorksheet(
        wb,
        sheetName = sheet.names[i],
        orientation = "landscape",
        paperSize = 1,
        header=c(
          "COVAILworkflow",
          sprintf("%s: %s",batch.name,toupper(sub("_"," ",sub("_COVAIL.*$","",sub("^[0-9]_","",sheet.names[i]))))),
          paste("worksheet",i,"of #")
        )
      )
    })
  )
  ##add sheet 1 data
  openxlsx::writeDataTable(
    wb,
    sheet = sheet.names[1],
    x = dt,
    tableStyle = "TableStyleLight1"
  )
  ##set sheet 1 column widths
  col.widths<-col.widths.vector(dt)
  openxlsx::setColWidths(
    wb,
    sheet = sheet.names[1],
    cols = 1:ncol(dt),
    widths = col.widths
  )
  ##set sheet 1 page options
  openxlsx::pageSetup(
    wb,
    sheet=sheet.names[1],
    fitToWidth = TRUE
  )
  ##add sheet 2 data
  ##save .png
  png.path<-file.path(tempdir(),paste0(batch.name,".png"))
  grDevices::png(
    filename = png.path,
    width = 8,
    height = 8,
    units = "in",
    res = 300
  )
  print(plot.boxmap)
  grDevices::dev.off()
  ##insert .png
  openxlsx::insertImage(
    wb,
    sheet = sheet.names[2],
    file = png.path,
    width = 8,
    height = 8,
    units = "in",
    dpi = 300
  )
  ##set sheet 2 page options
  openxlsx::pageSetup(
    wb,
    sheet=sheet.names[2],
    fitToHeight = TRUE,
    fitToWidth = TRUE
  )
  ##add sheet 3 data
  ##subset dt to include select columns
  dt.thaw<-(
    dt[,.(stage.name,batch.order,sample.id)]
    [,tube.label:=paste(sample.id,sprintf("%02d",batch.order),sep=paste0(rep(" ",8),collapse = ""))]
  )
  openxlsx::writeDataTable(
    wb,
    sheet = sheet.names[3],
    x = dt.thaw,
    tableStyle = "TableStyleLight1"
  )
  ##set sheet 3 column widths
  col.widths<-col.widths.vector(dt.thaw)
  openxlsx::setColWidths(
    wb,
    sheet = sheet.names[3],
    cols = 1:ncol(dt.thaw),
    widths = col.widths
  )
  ##set sheet 3 page options/styles
  openxlsx::pageSetup(
    wb,
    sheet=sheet.names[3],
    fitToWidth = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet=sheet.names[3],
    cols=c(3L,4L),
    rows=2:(nrow(dt.thaw)+1),
    gridExpand = TRUE,
    style = openxlsx::createStyle(halign = 'right')
  )
  ##write/save the workbook
  if(!dir.exists(workbook.dir)) dir.create(workbook.dir,recursive = T)
  file.out<-file.path(workbook.dir,sprintf("%s_%s.xlsx",batch.name,"workflow"))
  openxlsx::saveWorkbook(wb,file=file.out,overwrite = .overwrite)
  ##remove .png
  invisible(file.remove(png.path))
}
#' @title Plot a COVAIL staging box
#'
#' @param manifest.collapsed as returned from \link{manifest.batch.assign}:
#' \itemize{
#'    \item list element (if in environment)
#'    \item sheet name (if saved as .xlsx).
#' }
#'
#' @return a \link[ggplot2]{ggplot2} object
#' @export
#'
#' @example /R/examples/plot_manifest_stage_example.R
plot_manifest.stage<-function(manifest.collapsed){
  lapply(split(manifest.collapsed[,.(sample.id,stage.name,stage.position)],by='stage.name',sorted = T),function(batch){
    batch[,c('subject.id','visit') := data.table::tstrsplit(sample.id,"_",type.convert = as.character)]
    batch[,subject.id:=factor(subject.id,levels=unique(subject.id))]
    batch[,visit:=factor(visit,levels=unique(visit))]
    batch<-cbind(batch,batch[,data.table::tstrsplit(stage.position,'::')])
    stage<-data.table::melt(
      batch,
      measure.vars=grep("^V[0-9]+$",names(batch),value = T),
      value.name = 'row_col'
    )[,-c('stage.position','variable')]
    stage[,c('row','col'):=data.table::tstrsplit(row_col,"_",type.convert=as.numeric)]
    plate<-merge(stage,slap::plate.layout(9,9),all.y=T)
    ##
    slap::plate.plot(plate,row.label = 'n') +
      ggplot2::geom_point(
        data=stats::na.omit(plate),
        mapping=ggplot2::aes(
          col,
          row,
          color=subject.id,
          shape=visit),
        size=10) +
      ggplot2::labs(
        title=unique(plate[['stage.name']]),
        subtitle = "Staging Box",
        caption =
          paste(
            "Consulting an accompanying 'stage vials' worksheet, pull the following sample vials and store them exactly as indicated.",
            "The order -- row,column -- indicates a batch-specifc order.",
            "If additional HD#### (PBMC control) vials are needed, fill column 9.",
            sep="\n"
          )
      )
  })
}
#' @title Get a numeric vector for setting Excel sheet column widths
#'
#' @param dt `data.frame`/`data.table`
#' @param width.padding Numeric; default 2. Adds padding to determined column widths.
#'
#' @returns Numeric vector; values indicate optimal Excel sheet column widths for printing/readability.
#'
col.widths.vector<-function(dt,width.padding=2){
  ##code from: https://stackoverflow.com/questions/45860085/r-autofit-excel-column-width Rick Park
  width_vec <- apply(dt, 2, function(x) max(nchar(as.character(x)) + width.padding, na.rm = TRUE))
  width_vec_header <- nchar(names(dt)) + width.padding
  max_vec_header <- pmax(width_vec, width_vec_header)
}
