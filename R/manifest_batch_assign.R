#' @title Add batch assignments to a BLIS COVAIL manifest
#'
#' @param manifest a \link[data.table]{data.table} as returned from \link{BLIS.manifest.prepare}.
#' @param vials.per numeric; the number of sample-specific vials to assign to each batch.
#'
#' @return a list containing two `data.table`s; a long-data manifest and a collapsed manifest.
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
#' manifest<-BLIS.manifest.prepare(manifest.BLIS.path,manifest.physical.path)[]
#' manifest.batch.assign(manifest,vials.per=2)
#'
manifest.batch.assign<-function(manifest,vials.per=2){
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
    set.seed(1337)
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

  ##return a list
  ##long and collapse
  return(
    list(
      manifest,
      manifest.collapsed
    )
  )

  # ##write out as excel file
  # ##two sheets: long, collapsed
  # wb <- openxlsx::createWorkbook()
  # openxlsx::addWorksheet(wb, "manifest_long")
  # openxlsx::writeDataTable(wb, "manifest_long", x = manifest,
  #                          tableStyle = "TableStyleLight1"
  # )
  # openxlsx::addWorksheet(wb, "manifest_collapsed")
  # openxlsx::writeDataTable(wb, "manifest_collapsed", x = manifest.collapsed,
  #                          tableStyle = "TableStyleLight1"
  # )
  # openxlsx::saveWorkbook(wb,file="./data_meta/manifest/manifest.xlsx")
}
