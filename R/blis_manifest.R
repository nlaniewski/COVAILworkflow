#' @title Convert a BLIS COVAIL manifest (.xlsx) to a \link[data.table]{data.table}
#'
#' @description
#' A BLIS COVAIL manifest -- individual vials/rows -- is converted to a \link[data.table]{data.table} along with some formatting, renaming, and re-encoding/alias columns. Additional physical box locations are added.
#'
#'
#' @param BLIS.SpecimenDetail.filepath path to an exported (.xlsx) BLIS COVAIL manifest; the manifest should have individual vials/rows.
#' @param manifest.physical.filepath path to physical box/storage mappings for COVAIL samples.
#'
#' @return a \link[data.table]{data.table}
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
#' BLIS.manifest.prepare(manifest.BLIS.path)[]
#' BLIS.manifest.prepare(manifest.BLIS.path,manifest.physical.path)[]
#'
BLIS.manifest.prepare<-function(BLIS.SpecimenDetail.filepath,manifest.physical.filepath=NULL){
  ##full COVAIL sample manifest downloaded from BLIS
  ##https://blis.urmc.rochester.edu/labkey/specimen/Topham%20Lab/COVAIL_Vaccine_Study/specimens.view?showVials=true#

  ##read excel sheet; convert to data.table
  manifest<-data.table::setDT(
    openxlsx::read.xlsx(BLIS.SpecimenDetail.filepath),
    check.names = TRUE
  )

  ##colnames lower case
  names(manifest)<-tolower(names(manifest))

  ##rename columns
  data.table::setnames(
    manifest,
    c('participant.id','global.unique.id','sequence.num'),
    c('subject.id','g.id','visit')
  )

  ##drop NA cols; 'site'ldms.code'
  ##drop BLIS 'row.id'
  drop.cols<-c(
    names(which(sapply(manifest,function(j) all(is.na(j))))),
    'site.ldms.code',
    'row.id'
  )
  manifest<-manifest[,-..drop.cols];rm(drop.cols)

  ##rename manifest 'row.id.1'
  data.table::setnames(
    manifest,
    'row.id.1',
    'row.id'
  )

  ##tests
  manifest[,length(unique(subject.id))]==243
  manifest[,length(unique(g.id))]==2171
  manifest[,length(unique(visit))]==3
  if(manifest[,digest::digest(sort(g.id))]!="1d72685b3112799e5d764fca909f82ab"){
    "Global ID digest does not match historic value; has the source manifest changed?"
  }

  ##set order
  data.table::setorder(manifest,subject.id,visit)
  manifest[,.N,keyby=.(subject.id,visit)][,unique(N)]

  ##single value for box_row_col
  manifest[,box.row.col:=paste(box.id,row.id,position.in.row,sep=".")]

  ##re-encode subject.id
  ##digest the full length subject.id
  ##take the first four alphanumerics
  length(unique(manifest[,sapply(unique(subject.id),function(id){
    d<-digest::digest(id)
    substr(d,1,4)
  })]))==243
  manifest[,subject.id.alias:=sapply(subject.id,function(id){
    d<-digest::digest(id)
    substr(d,1,4)
  })]

  ##tests
  manifest[,unique(subject.id.alias)];manifest[,length(unique(subject.id.alias))]==243

  ##visit.alias
  ##factor; three levels
  ##V1,V2,V3
  manifest[,visit.alias:=factor(visit,labels=paste0("V",1:3))]

  ##tests
  manifest[,length(unique(subject.id.alias))]==243
  manifest[,length(unique(visit))]==3

  ##unique sample.id
  manifest[,sample.id:=paste(subject.id.alias,visit.alias,sep="_")]

  ##add metadata columns
  manifest[,study.id:="COVAIL"]
  manifest[,batch.seq:=character()]

  ##digest test
  if(manifest[,digest::digest(sample.id)]!="265c7fcbfb224ba1dde829c2e4c3d0ae"){
    stop("Constructed 'sample.id' does not match historic digest; check source script!")
  }

  ##return manifest if 'manifest.physical.filepath' is NULL
  if(is.null(manifest.physical.filepath)){
    return(manifest)
  }

  ##physical locations
  ##read excel sheet; convert to data.table
  manifest.physical<-data.table::setDT(
    openxlsx::read.xlsx(manifest.physical.filepath),
    check.names = TRUE
  )

  ##colnames lower case
  names(manifest.physical)<-tolower(names(manifest.physical))

  ##rename columns
  data.table::setnames(
    manifest.physical,
    c('global.unique.id'),
    c('g.id')
  )

  ##drop NAs; 'Empty'
  manifest.physical<-manifest.physical[g.id!="Empty"]

  ##tests
  all(manifest[['g.id']] %in% manifest.physical[['g.id']])
  manifest.physical[,length(unique(g.id))]==2171

  ##change col types
  for(j in c('box.id','row.id','position.in.row')){
    data.table::set(
      manifest,
      j=j,
      value=as.numeric(manifest[[j]])
    )
  }

  ##set key
  data.table::setkey(manifest,g.id)
  data.table::setkey(manifest.physical,g.id)

  ##tests
  cols.test<-names(manifest.physical)[names(manifest.physical) %in% names(manifest)]
  for(j in cols.test){
    if(all(manifest.physical[[j]]!=manifest[[j]])){
      stop("Tests failed between 'manifest.physical' and 'manifest'")
    }
  }

  ##merge
  cols.merge<-c('g.id',names(manifest.physical)[!names(manifest.physical) %in% names(manifest)])
  manifest<-manifest[manifest.physical[,..cols.merge]]

  ##reorder manifest
  data.table::setorder(manifest,subject.id,visit,box.row.col)

  ##digest test
  if(manifest[,digest::digest(sample.id)]!="265c7fcbfb224ba1dde829c2e4c3d0ae"){
    stop("Constructed 'sample.id' does not match historic digest; check source script!")
  }else{
    return(manifest)
  }
}
