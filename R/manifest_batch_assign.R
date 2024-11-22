#' @title Add batch assignments to a BLIS COVAIL manifest
#'
#' @description
#' Samples from a BLIS COVAIL manifest, as returned by \link{BLIS.manifest.prepare}, are assigned a 'batch.seq' and a 'batch.order'; depending on study requirements, both of these assignments can be randomized.  Based on these assignments, batch-specific 'staging' boxes are pre-mapped.
#'
#'
#' @param manifest a \link[data.table]{data.table} as returned from \link{BLIS.manifest.prepare}.
#' @param vials.per numeric; the number of sample-specific vials to assign to each batch.
#' @param seed.val numeric; a seed value used when randomly sampling subject.ids and/or batch order.
#' @param randomize.batch.order logical; if `TRUE`, the batch order will be randomized instead of ordered by subject.id + visit.
#' @param workbook.path file.path (character string); if defined, an excel (.xlsx) workbook will be created.
#'
#' @return a list containing two `data.table`s; a long-data manifest (individual vials) and a collapsed manifest.
#' @export
#'
#' @examples
#' manifest.BLIS.path<-list.files(
#' system.file("extdata",package="COVAILworkflow"),
#' pattern="SpecimenDetail", full.names = TRUE
#' )
#'
#' manifest.physical.path<-list.files(
#' system.file("extdata",package="COVAILworkflow"),
#' pattern="physical", full.names = TRUE
#' )
#'
#' manifest<-BLIS.manifest.prepare(manifest.BLIS.path,manifest.physical.path)
#' manifest.batch.assign(manifest,vials.per=2)
#' manifest.batch.assign(manifest,vials.per=2,randomize.batch.order=TRUE)
#'
#' fp<-file.path(tempdir(),"COVAIL_manifest.xlsx")
#' manifest.batch.assign(manifest,vials.per=2,workbook.path=fp)
#' openxlsx::read.xlsx(fp,sheet="manifest_collapsed")
manifest.batch.assign<-function(manifest,vials.per=2,seed.val=1337,randomize.batch.order=F,workbook.path=NULL){
  ##batches of 27 samples
  ##27/3 stim conditions/3 visits = 3 subjects per plate 'block'
  ##two plate blocks = 6 subjects
  ##randomly select 6 subjects
  ##full batch assignment for 243 subjects = 243/6 = 40.5 batches
  ##assign batch.seq
  ##build a list of batch.ids
  subject.ids<-manifest[,unique(subject.id.alias)]
  batch.ids<-vector(mode='list',length=ceiling(length(subject.ids)/6))
  for(i in seq(batch.ids)){
    subject.ids.assigned<-unlist(batch.ids)
    l<-length(which(!subject.ids %in% subject.ids.assigned))
    set.seed(seed.val)
    batch.ids[[i]]<-sample(subject.ids[!subject.ids %in% subject.ids.assigned],ifelse(l>=6,6,l))
  }
  length(unique(unlist(batch.ids)))
  names(batch.ids)<-sprintf("%03d",seq(batch.ids))

  ##expand the list of batch.ids to be sample-specific; 'subject.id.alias' and 'visit.alias'
  ##sample.id
  batch.ids<-lapply(batch.ids, function(subject.ids){
    manifest[subject.id.alias %in% subject.ids,unique(sample.id)]
  })
  length(unique(unlist(batch.ids)))

  ##add batch assignments to manifest
  ##vials.per (function argument) each
  invisible(
    lapply(names(batch.ids),function(.batch.seq){
      for(.sample.id in batch.ids[[.batch.seq]]){
        data.table::set(
          manifest,
          i=manifest[,.I[sample.id==.sample.id]][seq(vials.per)],
          j='batch.seq',
          value=.batch.seq
        )
      }
    })
  )

  ##collapse batch assignments and rbindlist
  manifest.collapsed<-data.table::rbindlist(
    lapply(split(manifest[!is.na(batch.seq)],by='sample.id'),function(dt){
      data.table::setorder(dt,box.row.col)
      cols.counts<-sapply(dt,function(j) length(unique(j)))
      cols.collapse<-names(which(cols.counts==1))
      cols.paste<-names(which(cols.counts>1))
      dt.list<-as.list(dt)
      dt.list[cols.collapse]<-lapply(dt.list[cols.collapse],unique)
      dt.list[cols.paste]<-lapply(dt.list[cols.paste],paste0,collapse="::")
      names(dt.list)[names(dt.list) %in% cols.paste]<-paste0(names(dt.list)[names(dt.list) %in% cols.paste],'s')
      data.table::as.data.table(dt.list)
    }),
    use.names=FALSE
  )

  ##set a key on manifest.collapsed and add batch.order
  data.table::setkey(manifest.collapsed,batch.seq,sample.id)
  if(randomize.batch.order){
    manifest.collapsed[,batch.order:={set.seed(seed.val);sample(seq(.N),.N)},by=.(batch.seq)]
    data.table::setorder(manifest.collapsed,batch.seq,batch.order)
  }else{
    manifest.collapsed[,batch.order:=seq(.N),by=.(batch.seq)]
  }

  ##add batch.order to manifest; this reorders manifest
  manifest<-manifest[manifest.collapsed[,.(sample.id,batch.order)],on='sample.id']

  ##add batch-specific staging
  stage<-data.table::rbindlist(
    lapply(
      split(manifest[!is.na(batch.seq),.N,by=.(batch.seq,batch.order,sample.id)],by='batch.seq'),
      function(batch){
        box.vec<-paste(
          row.id=rep(1:9,each=9),
          col.id=rep(1:9,times=9),
          sep="_"
        )
        #
        for(i in batch[['batch.order']]){
          if(!'stage.position' %in% names(batch)){
            batch[batch.order==i,stage.position := paste0(box.vec[seq(N)],collapse = "::")]
          }else{
            box.vec.n<-which(box.vec %in% max(unlist(strsplit(stats::na.omit(batch[['stage.position']]),'::'))))+2
            box.vec.sub<-box.vec[box.vec.n:length(box.vec)]
            batch[batch.order==i,stage.position := paste0(box.vec.sub[seq(N)],collapse = "::")]
          }
        }
        #
        batch[,stage.name:=paste("COVAIL",unique(batch.seq),sep="_")]
        #
        return(batch[,-'N'])
      })
  )

  ##merge
  manifest.collapsed<-manifest.collapsed[stage[,.(sample.id,stage.name,stage.position)],on='sample.id']

  ##list
  manifests<-list(
    manifest.vials=manifest,
    manifest.collapsed=manifest.collapsed
  )

  ##write out an excel workbook
  if(!is.null(workbook.path)){
    ##write out as excel file
    ##two sheets: vials, collapsed
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "manifest_vials")
    openxlsx::writeDataTable(wb, "manifest_vials", x = manifests$manifest.vials,
                             tableStyle = "TableStyleLight1"
    )
    openxlsx::addWorksheet(wb, "manifest_collapsed")
    openxlsx::writeDataTable(wb, "manifest_collapsed", x = manifests$manifest.collapsed,
                             tableStyle = "TableStyleLight1"
    )
    openxlsx::saveWorkbook(wb,file=workbook.path)
  }
  ##
  return(manifests)
}
#' @title Plot a COVAIL staging box
#'
#' @param manifest.collapsed as returned from \link{manifest.batch.assign}
#'
#' @return a \link[ggplot2]{ggplot2} object
#' @export
#'
#' @examples
#'
plot_manifest.stage<-function(manifest.collapsed){
  lapply(split(manifest.collasped[,.(sample.id,stage.name,stage.position)],by='stage.name',sorted = T),function(batch){
    batch[,c('subject.id','visit') := data.table::tstrsplit(sample.id,"_",type.convert = as.factor)]
    batch<-cbind(batch,batch[,data.table::tstrsplit(stage.position,'::')])
    stage<-data.table::melt(
      batch,
      measure.vars=c('V1','V2'),
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
            "Consulting an accompanying 'pull list' worksheet, pull the following sample vials and store them as indicated.",
            "The order -- row,column -- indicates a batch-specifc order.",
            sep="\n"
          )
      )
  })
}
