#' @title Add cell counts to a manifest
#'
#' @param manifest.collapsed as returned from \link{manifest.batch.assign}:
#' \itemize{
#'    \item list element (if in environment)
#'    \item sheet name (if saved as .xlsx).
#' }
#' @param cellometer.counts.path file.path (character string); Cellometer K2 cell counts .csv file.
#' @param rest.volume.ml Numeric; default 2.2. The total volume (mL) used to resuspend and rest cells; the total volume the cell count aliquot was taken from.
#'
#' @return an updated manifest that includes total live cells and % viability.
#' @export
#'
#' @example /R/examples/manifest_cell_counts_example.R
manifest.cell.counts<-function(manifest.collapsed,cellometer.counts.path,rest.volume.ml=2.2){
  ##parse cell counts; use default function arguments
  cellometer.counts<-slap::cellometer.counts.parse(
    counts.csv.path = cellometer.counts.path
  )
  ##"sample.name" value must be enforced or function will break
  ##"COVAIL_[0-9]{3}_[0-9]{2}"; "COVAIL_001_01"
  ##should probably add a check/error/stop condition
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
  ##
  cols.on<-c(
    'stage.name',
    'batch.order'
  )
  cols.manifest<-'sample.id'
  cols.counts<-c(
    'viability',
    'total.live',
    'N.conditions',
    'N.plates',
    'cells.per.condition',
    'resuspension.volume.ul'
  )
  ##a subset of columns from the manifest merged with a subset of columns from the counts
  dt<-manifest.collapsed[,mget(c(cols.on,cols.manifest))][
    cellometer.counts[,mget(c(cols.on,cols.counts))],
    on=cols.on
  ]
  ##update "HD" control conditions
  dt[grep("HD",sample.id),N.conditions:=12]
  ##update "HD" control cells per condition
  dt[grep("HD",sample.id),cells.per.condition:=formatC(
    (as.numeric(total.live)/N.conditions),
    format = "E",
    digits = 2
  )]
  ##split dt by batch/stage name
  dts<-split(dt,by='stage.name')
  ##
  lapply(dts,function(dt){
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
    dt.blocks[]
  })
}
