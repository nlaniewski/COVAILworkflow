batch_plate_example<-function(blocks.n=c(1,2)){
  plate<-data.table::setDT(slap::plate.layout(8,12))
  mdat<-data.table::data.table(
    subject.id=rep(rep(paste0('S',1:3),each=3),3),
    visit.id=rep(paste0('V',1:3),times=9),
    stim.id=rep(c('NEG','PEPTIDE','POS'),each=9),
    row=rep(1:3,each=9),
    col=rep(1:9,times=3)
  )
  if(blocks.n==2){
    mdat2<-data.table::copy(mdat)
    mdat2[,subject.id := sub(3,6,sub(2,5,sub(1,4,subject.id)))]
    mdat2[,row := as.integer(sub(3,8,sub(2,7,sub(1,6,row))))]
    mdat<-rbind(mdat,mdat2)
  }
  mdat[,sample.id := paste(subject.id,visit.id,stim.id,sep="_")]
  plate<-merge(plate,mdat,all.x=T)
  # plate<-plate[mdat,on=c('row','col')]
  slap::plate.plot(plate) +
    ggplot2::geom_point(
      data=plate,
      mapping=ggplot2::aes(
        col,
        row,
        color=subject.id,
        shape=visit.id),
      size=10) +
    ggplot2::scale_color_discrete(na.translate=F) +
    ggplot2::scale_shape_discrete(na.translate=F)
}
