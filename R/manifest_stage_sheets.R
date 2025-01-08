#' @title Generate printable batch-specific worksheets
#'
#' @description
#' Using the result of \link{manifest.batch.assign}, generate printable batch-specific worksheets: a stage list (vials), a stage list box map (9x9), and a 'Day 1: Thaw' sheet. This pre-organization step allows for confirmation of sample vial availability and correct metadata (unique identifier) and expedites assay workflow. Sheets are to be printed, signed, and hole-punched for storage in study-specific binder.
#'
#'
#' @param manifest.collapsed as returned from \link{manifest.batch.assign}:
#' \itemize{
#'    \item list element (if in environment)
#'    \item sheet name (if saved as .xlsx).
#' }
#' @param workbook.dir file.path (character string); excel (.xlsx) workbook(s) will be written to the directory.
#' @param .overwrite logical; default `FALSE`. If `TRUE`, will overwrite an existing batch-specific workbook.
#'
#' @return .xlsx (Excel) workbook with three sheets:
#' \enumerate{
#'    \item pull/stage list of sample vials; banded-row table
#'    \item pull/stage box map; embedded .png plot of a 9x9 box
#'    \item Day 1: Thaw; banded-row table
#' }
#' @export
#'
#' @example /R/examples/manifest_stage_sheets_example.R
manifest.stage.sheets<-function(manifest.collapsed,workbook.dir,.overwrite=FALSE){
  dt<-manifest.collapsed[,
                         .(
                           g.ids
                           ,subject.id
                           ,visit
                           ,subject.id.alias
                           ,visit.alias
                           ,sample.id
                           ,freezer
                           ,rack.location
                           ,box.label
                           ,box.row.cols
                           ,batch.seq
                           ,batch.order
                           ,stage.name
                           ,stage.position
                         )
  ]
  dts<-split(dt,by='stage.name')
  dts.boxes<-lapply(dts,function(batch){
    plot_manifest.stage(batch)
  })
  ##
  invisible(
    lapply(
      names(dts),function(batch){
        ##create an excel workbook
        ##three sheets: banded-row table (pull/stage list); boxmap plot; Day 1: Thaw

        ##sheet 1; banded-row table
        sheet1name<-sprintf("%s_%s",batch,"stage_list")
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(
          wb,
          sheetName = sheet1name
        )
        openxlsx::writeDataTable(
          wb,
          sheet = sheet1name,
          x = dts[[batch]],
          tableStyle = "TableStyleLight1"
        )
        openxlsx::setHeaderFooter(
          wb,
          sheet=sheet1name,
          header=c(
            "COVAILworkflow",
            sprintf("%s: %s",batch,"Stage List (Vials)"),
            "worksheet 1 of #"
          )
        )
        ##set column widths
        ##code from: https://stackoverflow.com/questions/45860085/r-autofit-excel-column-width Rick Park
        width_vec <- apply(dts[[batch]], 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
        width_vec_header <- nchar(names(dts[[batch]])) + 2
        max_vec_header <- pmax(width_vec, width_vec_header)
        openxlsx::setColWidths(
          wb,
          sheet = sheet1name,
          cols = 1:ncol(dts[[batch]]),
          widths = max_vec_header
        )

        ##set page options
        openxlsx::pageSetup(
          wb,
          sheet=sheet1name,
          orientation = "landscape",
          fitToWidth = TRUE,
          paperSize = 1
        )

        ##sheet 2; boxmap plot
        sheet2name<-sprintf("%s_%s",batch,"stage_box")
        openxlsx::addWorksheet(
          wb,
          sheetName = sheet2name,
          gridLines = FALSE
        )
        ##save .png
        png.path<-file.path(tempdir(),paste0(batch,".png"))
        grDevices::png(
          filename = png.path,
          width = 8,
          height = 8,
          units = "in",
          res = 300
        )
        print(dts.boxes[[batch]])
        grDevices::dev.off()
        ##insert .png
        openxlsx::insertImage(
          wb,
          sheet = sheet2name,
          file = png.path,
          width = 8,
          height = 8,
          units = "in",
          dpi = 300
        )

        openxlsx::setHeaderFooter(
          wb,
          sheet=sheet2name,
          header=c(
            "COVAILworkflow",
            sprintf("%s: %s",batch,"Stage List Box Map (9x9)"),
            "worksheet 2 of #"
          )
        )

        ##set page options
        openxlsx::pageSetup(
          wb,
          sheet=sheet2name,
          orientation = "landscape",
          fitToHeight = TRUE,
          fitToWidth = TRUE,
          paperSize = 1
        )

        ##sheet 3; banded-row table
        sheet3name<-sprintf("%s_%s",batch,"Day_1_Thaw")
        openxlsx::addWorksheet(
          wb,
          sheetName = sheet3name
        )
        ##cols to print
        cols_thaw<-c(
          'stage.name',
          'batch.order',
          'sample.id'
        )
        dt.thaw<-dts[[batch]][,..cols_thaw]
        dt.thaw[,tube.label:=paste(sample.id,sprintf("%02d",batch.order),sep=paste0(rep(" ",5),collapse = ""))]
        ##
        openxlsx::writeDataTable(
          wb,
          sheet = sheet3name,
          x = dt.thaw,
          tableStyle = "TableStyleLight1"
        )
        ##set column widths
        ##code from: https://stackoverflow.com/questions/45860085/r-autofit-excel-column-width Rick Park
        width_vec <- apply(dt.thaw, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
        width_vec_header <- nchar(names(dt.thaw)) + 2
        max_vec_header <- pmax(width_vec, width_vec_header)
        openxlsx::setColWidths(
          wb,
          sheet = sheet3name,
          cols = 1:ncol(dt.thaw),
          widths = max_vec_header
        )
        ##
        openxlsx::addStyle(
          wb,
          sheet=sheet3name,
          cols=c(3L,4L),
          rows=2:(nrow(dt.thaw)+1),
          gridExpand = TRUE,
          style = openxlsx::createStyle(halign = 'right')
        )
        ##
        openxlsx::setHeaderFooter(
          wb,
          sheet=sheet3name,
          header=c(
            "COVAILworkflow",
            sprintf("%s: %s",batch,"Day 1: Thaw"),
            "worksheet 3 of #"
          )
        )
        ##set page options
        openxlsx::pageSetup(
          wb,
          sheet=sheet3name,
          orientation = "landscape",
          fitToWidth = TRUE,
          paperSize = 1
        )

        ##write/save the workbook
        file.out<-file.path(workbook.dir,paste0(batch,".xlsx"))
        openxlsx::saveWorkbook(wb,file=file.out,overwrite = .overwrite)

        ##remove .png
        file.remove(png.path)
      }
    )
  )
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
            "Consulting an accompanying 'stage list' worksheet, pull the following sample vials and store them exactly as indicated.",
            "The order -- row,column -- indicates a batch-specifc order.",
            "If additional HD#### (PBMC control) vials are needed, fill column 9.",
            sep="\n"
          )
      )
  })
}
