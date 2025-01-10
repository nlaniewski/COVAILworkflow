#' @title Add cell counts and a plate-based well map to an existing COVAIL workbook
#'
#' @param COVAILworkbook.path file.path (character string); pre-existing COVAILworkbook (.xlsx) as returned from \link{COVAILworkbook.initialize}. The COVAILworkbook file will be updated with two new worksheets.
#' @param cellometer.counts.path file.path (character string); Cellometer K2 cell counts .csv file.
#' @param rest.volume.ml Numeric; default 2.2. The total volume (mL) used to resuspend and rest cells; the total volume the cell count aliquot was taken from.
#'
#' @return an updated COVAILworkbook (.xlsx) that includes cell counts and associated derivatives (viability, conditions, cells per condition, etc.); also includes a plate-based well map for setting up stimulation conditions.
#' @export
#'
#' @example /R/examples/COVAILworkbook_cellcounts_example.R
COVAILworkbook.cellcounts<-function(COVAILworkbook.path,cellometer.counts.path,rest.volume.ml=2.2){
  wb<-openxlsx::loadWorkbook(COVAILworkbook.path)
  batch<-stage.name<-unique(sapply(c(1,3),function(i){unique(openxlsx::read.xlsx(wb,sheet=i)$stage.name)}))
  if(length(batch)>1){
    stop("More than one batch/stage.name detected; check COVAILworkbook...")
  }
  sheets.existing<-openxlsx::sheets(wb)
  if(!all(grepl(batch,sheets.existing))){
    stop("Is this the correct batch/stage?")
  }
  if(length(sheets.existing)!=3){
    stop("Expecting 3 existing sheets...")
  }
  ##"sample.name" value must be enforced or function will break
  ##"COVAIL_[0-9]{3}_[0-9]{2}"; "COVAIL_001_01"
  ##parse cell counts; use default function arguments
  cellometer.counts<-slap::cellometer.counts.parse(
    counts.csv.path = cellometer.counts.path
  )
  if(!any(grepl(batch,cellometer.counts$sample.name))){
    stop("COVAILworkbook batch/stage.name not detected in cellometer cell counts...")
  }
  cellometer.counts<-cellometer.counts[grep(batch,sample.name)]
  ##
  cellometer.counts[,c('study.name','batch.seq','batch.order'):=data.table::tstrsplit(sample.name,"_")]
  cellometer.counts[,batch.order:=as.numeric(batch.order)]
  cellometer.counts[,stage.name:=paste(study.name,batch.seq,sep="_")][,c('sample.name','study.name','batch.seq'):=NULL]
  ##samples in a total volume
  cellometer.counts[,volume.ml := rest.volume.ml]
  ##total number of live cells
  cellometer.counts[,total.live:=formatC(volume.ml*concentration.live,format = "E", digits = 2)]
  ##variable argument?
  cellometer.counts[,N.conditions:=6]
  ##dependent on "N.conditions" value
  cellometer.counts[,N.plates:=2]
  cellometer.counts[,cells.per.condition:=formatC(
    (as.numeric(total.live)/N.conditions),
    format = "E",
    digits = 2
  )]
  ##dependent on "N.conditions" value and lower/upper threshold of cells per well
  cellometer.counts[,resuspension.volume.ul:=600]
  ##existing sheet as dt; has metadata for merging with cell counts
  dt<-data.table::setDT(openxlsx::read.xlsx(wb,sheet=3))[]
  dt[,tube.label:=NULL]
  if(!all(names(dt) %in% c('stage.name','batch.order','sample.id'))){
    stop("c('stage.name','batch.order','sample.id') not found in sheet 3...")
  }
  ##
  cols.on<-c(
    'stage.name',
    'batch.order'
  )
  cols.counts<-c(
    'viability',
    'total.live',
    'N.conditions',
    'N.plates',
    'cells.per.condition',
    'resuspension.volume.ul'
  )
  ##a subset of columns from the manifest merged with a subset of columns from the counts
  dt<-dt[cellometer.counts[,mget(c(cols.on,cols.counts))],on=cols.on]
  ##update "HD" control conditions
  dt[grep("HD",sample.id),N.conditions:=12]
  ##update "HD" control cells per condition
  dt[grep("HD",sample.id),cells.per.condition:=formatC(
    (as.numeric(total.live)/N.conditions),
    format = "E",
    digits = 2
  )]
  ##per plate block
  ##3 subjects + 1 control
  ##3 visits per experimental subject + 1 'visit' per control
  ##3 stim. conditions
  ##(3*3*3)+(1*1*3); 30 total samples/wells
  subject.ids<-dt[,unique(sub("_.*$","",sample.id))]
  subject.ids.experimental<-grep("HD",subject.ids,invert = T,value = T)
  subject.ids.ctrl<-grep("HD",subject.ids,invert = F,value = T)
  ##BLOCK 1
  dt.block1<-dt[grep(paste0(c(subject.ids.experimental[1:3],subject.ids.ctrl),collapse = "|"),sample.id)]
  ##BLOCK 2
  dt.block2<-dt[grep(paste0(c(subject.ids.experimental[4:6],subject.ids.ctrl),collapse = "|"),sample.id)]
  ##
  data.table::setattr(dt.block1,'row.letters',LETTERS[1:3])
  data.table::setattr(dt.block2,'row.letters',LETTERS[6:8])
  ##updates by reference inside the loop
  invisible(
    lapply(list(dt.block1,dt.block2),function(block){
      row.letters<-attributes(block)$row.letters
      for(row.i in block[,seq(.N)]){
        data.table::set(
          block,
          i = row.i,
          j = 'wells',
          value = paste0(row.letters,rep(row.i,3),collapse = "::")
        )
      }
    })
  )
  ##row-bind the blocks
  dt.blocks<-rbind(dt.block1,dt.block2)
  dt.blocks[grep("A|B|C",wells),block:=1]
  dt.blocks[grep("F|G|H",wells),block:=2]
  ##
  if(is.null(COVAILworkbook.path)){
    dt.blocks[]
  }else{
    wb<-openxlsx::loadWorkbook(COVAILworkbook.path)
    sheets.existing<-openxlsx::sheets(wb)
    batch<-dt.blocks[,unique(stage.name)]
    if(!all(grepl(batch,sheets.existing))){
      stop("Is this the correct batch/stage?")
    }
    if(length(sheets.existing)!=3){
      stop("Expecting 3 existing sheets...")
    }
    sheet.name<-paste("4_Day2_Stim",batch,sep="_")
    ##add sheet 4
    openxlsx::addWorksheet(
      wb,
      sheetName = sheet.name,
      orientation = "landscape",
      paperSize = 1,
      header=c(
        "COVAILworkflow",
        sprintf("%s: %s",batch,toupper(sub("_"," ",sub("_COVAIL.*$","",sub("^[0-9]_","",sheet.name))))),
        paste("worksheet",4,"of #")
      )
    )
    ##add sheet 4 data
    openxlsx::writeDataTable(
      wb,
      sheet = sheet.name,
      x = dt.blocks,
      tableStyle = "TableStyleLight1"
    )
    ##set sheet 4 column widths
    col.widths<-col.widths.vector(dt.blocks)
    openxlsx::setColWidths(
      wb,
      sheet = sheet.name,
      cols = 1:ncol(dt.blocks),
      widths = col.widths
    )
    ##set sheet 4 page options
    openxlsx::pageSetup(
      wb,
      sheet=sheet.name,
      fitToWidth = TRUE
    )
    openxlsx::addStyle(
      wb,
      sheet=sheet.name,
      cols=1:ncol(dt.blocks),
      rows=1:(nrow(dt.blocks)+1),
      gridExpand = TRUE,
      style = openxlsx::createStyle(halign = 'center')
    )
    ##plate map
    plate<-dt.blocks[,.(stage.name,batch.order,sample.id,wells)]
    plate[,c('subject.id','visit.id') := data.table::tstrsplit(sample.id,"_",type.convert = as.character)]
    plate[,subject.id:=factor(subject.id,levels=unique(subject.id))]
    plate[,visit.id:=factor(visit.id,levels=unique(visit.id))]
    plate<-cbind(plate,plate[,data.table::tstrsplit(wells,'::')])
    plate.melt<-data.table::melt(
      plate,
      measure.vars=grep("^V[0-9]+$",names(plate),value = T),
      value.name = 'well.id'
    )[,-c('wells','variable')]
    plate.melt<-merge(plate.melt,slap::plate.layout(8,12),all.y=T)
    ##plate plot
    plate.plot<-slap::plate.plot(plate.melt,row.label = 'l') +
      ggplot2::geom_point(
        data=stats::na.omit(plate.melt),
        mapping=ggplot2::aes(
          col,
          row,
          color=subject.id,
          shape=visit.id),
        size=10) +
      ##
      ggplot2::annotate("rect",
                        xmin = rep(0.45,2),
                        xmax = rep(10.55,2),
                        ymin = c(0.45,5.45),
                        ymax = c(3.55,8.55),
                        color="black",
                        fill=NA,
                        linewidth=1.5
      ) +
      ggplot2::annotate("label",
                        x=rep(11,2),
                        y=c(2,7),
                        label=c("BLOCK 1","BLOCK 2"),
                        angle=90,
                        size=8
      ) +
      ggplot2::labs(
        title=unique(plate.melt[['stage.name']]),
        subtitle = paste(
          "Stimulation Plate Layout (Day 2)",
          "Duplicate for AIM and CYTOKINE",
          sep="\n"
        ),
        caption =
          paste(
            "Consulting an accompanying 'Day2_Stim' worksheet, plate the following samples (100 uL volume) exactly as indicated.",
            "The plate layout will need to be duplicated to account for both AIM and CYTOKINE (panel) stimulation conditions.",
            "'Blocks' refer to post-stimulation subject/panel-specific blocks that will be barcoded during staining (Day 3).",
            sep="\n"
          )
      )
    ##sheet 5
    sheet.name<-paste("5_Day2_StimPlate",batch,sep="_")
    ##add sheet 5
    openxlsx::addWorksheet(
      wb,
      sheetName = sheet.name,
      orientation = "landscape",
      paperSize = 1,
      header=c(
        "COVAILworkflow",
        sprintf("%s: %s",batch,toupper(sub("_"," ",sub("_COVAIL.*$","",sub("^[0-9]_","",sheet.name))))),
        paste("worksheet",5,"of #")
      )
    )
    ##add sheet 5 data
    ##save .png
    png.path<-file.path(tempdir(),paste0(batch,".png"))
    grDevices::png(
      filename = png.path,
      width = 8,
      height = 8,
      units = "in",
      res = 300
    )
    print(plate.plot)
    grDevices::dev.off()
    ##insert .png
    openxlsx::insertImage(
      wb,
      sheet = sheet.name,
      file = png.path,
      width = 8,
      height = 8,
      units = "in",
      dpi = 300
    )
    ##set sheet 5 page options
    openxlsx::pageSetup(
      wb,
      sheet=sheet.name,
      fitToHeight = TRUE,
      fitToWidth = TRUE
    )
    ##write/save the workbook
    openxlsx::saveWorkbook(wb,file=COVAILworkbook.path,overwrite = TRUE)
  }
}
