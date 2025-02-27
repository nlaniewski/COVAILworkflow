#' @title Add batch assignments to a BLIS COVAIL manifest
#'
#' @description
#' Samples from a BLIS COVAIL manifest, as returned by \link{BLIS.manifest.prepare}, are assigned a 'batch.seq' and a 'batch.order'; depending on study requirements, both of these assignments can be randomized.  Based on these assignments, batch-specific 'staging' boxes are pre-mapped. Current (2024-11-22) batch assignment conditions:
#'    * 27+3; 30 unique samples per barcoded pool:
#'      + 3 subjects + 1 Healthy Adult Donor PBMC
#'      + 3 visits + 1 Healthy Adult Donor PBMC 'visit'
#'      + 3 stimulation conditions
#'    * (27x2)+(3x2); 60 unique samples per barcoded pool/per panel/plate
#'      * 2 panels/plates (AIM, CYTOKINE)
#'    * ((27x2)+(3x2))x2; 120 unique samples per two barcoded pools/per panel/plate
#'
#'
#' @param manifest a \link[data.table]{data.table} as returned from \link{BLIS.manifest.prepare}.
#' @param vials.per numeric; the number of sample-specific vials to assign to each batch.
#' @param add.controls logical; default `TRUE`. Adds healthy adult control PBMCs ('HD####' placeholder name) to each batch.
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
#'
#' fp<-file.path(tempdir(),"COVAIL_manifest_batched.xlsx")
#' if(file.exists(fp)){file.remove(fp)}
#' invisible(manifest.batch.assign(manifest,vials.per=3,workbook.path=fp))
#' data.table::setDT(openxlsx::read.xlsx(fp,sheet="manifest_collapsed"))[]
manifest.batch.assign<-function(manifest,vials.per=3,add.controls=TRUE,seed.val=1337,randomize.batch.order=FALSE,workbook.path=NULL){
  ##batches of 27 samples + 1 healthy adult control PBMC ('add.controls' argument)
  ##27/3 stim conditions/3 visits = 3 subjects per plate 'block' + 1 HD PBMC
  ##two plate blocks = 6 subjects + 2 HD PBMC
  ##randomly select 6 subjects
  ##full batch assignment for 243 subjects = 243/6 = 40.5 batches
  ##assign batch.seq
  ##build a list of batch.ids

  ##subject.ids with incomplete visits
  subject.ids.visitN2<-(
    manifest[!duplicated(manifest[,.(subject.id.alias,visit.alias)])]
    [,.N,by=.(subject.id.alias)][which(N!=3)][['subject.id.alias']]
  )

  ##randomize subject.ids with incomplete visits
  subject.ids.visitN2<-{set.seed(seed.val);sample(subject.ids.visitN2)}

  ##randomize subject.ids with complete visits
  subject.ids.visitN3<-{
    set.seed(seed.val)
    sample(manifest[!subject.id.alias %in% subject.ids.visitN2,unique(subject.id.alias)])
  }

  ##batch vector
  batch.vector<-c(subject.ids.visitN3,subject.ids.visitN2)
  names(batch.vector)<-sprintf("%03d",rep(seq(floor(length(batch.vector)/6)),each=6))
  names(batch.vector)[is.na(names(batch.vector))]<-sprintf("%03d",as.numeric(max(stats::na.omit(names(batch.vector))))+1)

  ##
  dt.batch<-data.table::data.table(batch.seq=names(batch.vector),subject.id.alias=batch.vector)

  ##
  manifest<-manifest[dt.batch,on='subject.id.alias']

  ##add controls
  if(add.controls){
    n<-manifest[,length(unique(batch.seq))]
    dt.ctrl<-data.table::data.table(
      subject.id=rep("HD####",n),
      subject.id.alias=rep("HD####",n),
      visit=manifest[,unique(batch.seq)],
      visit.alias=manifest[,unique(batch.seq)],
      sample.id=paste(rep("HD####",n),manifest[,unique(batch.seq)],sep = "_"),
      study.id=rep("COVAIL",n),
      batch.seq=manifest[,unique(batch.seq)]
    )
    manifest<-rbind(manifest,dt.ctrl,fill=TRUE)
  }

  ##
  for(.sample.id in manifest[grep("HD",sample.id,invert = T),unique(sample.id)]){
    if(manifest[sample.id %in% .sample.id,.N]>vials.per){
      row.index<-manifest[,.I[sample.id==.sample.id]]
      data.table::set(
        manifest,
        i=row.index[3:length(row.index)],
        j='batch.seq',
        value = NA
      )
    }
  }

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

  # ##set a key on manifest.collapsed and add batch.order
  # data.table::setkey(manifest.collapsed,batch.seq,sample.id)

  ##add batch.order using existing manifest.collapsed batch.seq and sample.id order
  if(randomize.batch.order){
    manifest.collapsed[,batch.order:={set.seed(seed.val);sample(seq(.N),.N)},by=.(batch.seq)]
    data.table::setorder(manifest.collapsed,batch.seq,batch.order)
  }else{
    manifest.collapsed[,batch.order:=seq(.N),by=.(batch.seq)]
  }

  ##add batch.order to manifest; this reorders manifest
  manifest<-manifest[manifest.collapsed[,.(sample.id,batch.order)],on='sample.id']

  ##add batch-specific staging
  ##for 3 vials, prevent 'wrapping' (9 x 9) box
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
            if(batch[batch.order==i,N]==3&grepl("[0-9]{1}_9",box.vec.sub[1])){
              box.vec.sub<-box.vec.sub[-1]
            }
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
    manifest_vials=manifest,
    manifest_collapsed=manifest.collapsed
  )

  ##write out an excel workbook
  if(!is.null(workbook.path)){
    ##write out as excel file
    ##two sheets: vials, collapsed
    wb <- openxlsx::createWorkbook()
    sheets<-paste("manifest",c('vials','collapsed'),sep="_")
    invisible(
      lapply(sheets,function(sheet.name){
        openxlsx::addWorksheet(wb, sheet.name)
        openxlsx::writeDataTable(wb, sheet.name, x = manifests[[sheet.name]],
                                 tableStyle = "TableStyleLight1"
        )
      })
    )
    openxlsx::saveWorkbook(wb,file=workbook.path)
  }
  ##
  return(manifests)
}
