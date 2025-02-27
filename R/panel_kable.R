get.panel<-function(){
  ##for NSE R CMD check; data.table vars
  location<-panel.designation<-NULL
  ##
  panel.master<-data.table::fread(
    list.files(
      system.file("panels",package = "COVAILworkflow"),
      full.names = TRUE,
      pattern = "panels_COVAIL.csv"
    )
  )
  ##
  panels<-sapply(c('AIM','CYTOKINE'),function(panel.type){
    panel<-panel.master[panel.designation %in% c('conserved',panel.type)&location!='viability']
    panel[,panel.designation:=panel.type]
    if(panel.type=='AIM'){
      panel[grep('surface',location),location:='extracellular']
    }else if(panel.type=='CYTOKINE'){
      panel[grep('intracellular',location),location:='intracellular']
      panel[location!='intracellular',location:='extracellular']
    }
    # panel<-sapply(c('extracellular','intracellular'),function(.location){panel[location==.location]},simplify = F)
    # panel<-panel[!sapply(panel,nrow)==0]
  },simplify = F)
  ##
  panels<-data.table::rbindlist(panels)
  panels[]
}
##
panel.tables<-function(n=2.2,stain.volume=50){
  ##
  .SD<-buffer.type<-fluor<-marker<-
    location<-panel.designation<-
    titration<-volume.mix.uL<-volume.uL<-NULL#for NSE R CMD check; data.table vars
  ##
  panels<-get.panel()
  cols.keep<-c('panel.designation','marker','fluor','location','volume.mix.uL')
  panels.mix<-data.table::rbindlist(
    lapply(split(panels,by=c('panel.designation','location')),function(panel){
      panel[,volume.mix.uL:=titration*n]
      panel<-panel[,.SD,.SDcols=cols.keep]
      if(panel[,any(volume.mix.uL<1)]){
        panel[volume.mix.uL<1,
              c('volume.mix.uL','volume.mix.note'):=list(volume.mix.uL*10,"of a 1:10 in buffer")
        ]
      }
      panel[]
    }),
    fill = TRUE
  )
  panels.buffer<-data.table::rbindlist(
    lapply(split(panels.mix,by=c('panel.designation','location')),function(panel){
      ##
      .panel.designation<-panel[,unique(panel.designation)]
      .location<-panel[,unique(location)]
      ##
      vols<-data.table::data.table(
        volume.uL=(n*stain.volume)-(panel[,sum(volume.mix.uL)]),
        buffer.type='total buffer'
      )
      if(.location=='extracellular'){
        vols<-rbind(
          vols,
          data.table::data.table(
            volume.uL=vols[buffer.type=='total buffer',volume.uL-10],
            buffer.type='SBn'
          ),
          data.table::data.table(
            volume.uL=10,
            buffer.type='BSB+'
          ),
          fill=TRUE
        )
      }else if(.location=='intracellular'){
        vols<-rbind(
          vols,
          data.table::data.table(
            volume.uL=vols[buffer.type=='total buffer',volume.uL],
            buffer.type='1x Perm. Buffer'
          ),
          fill=TRUE
        )
      }
      vols[,c('panel.designation','location'):=list(.panel.designation,.location)]
    })
  )
  list(
    mix=panels.mix,
    buffer=panels.buffer
  )
}
##
panel.kable<-function(){
  ##
  panels<-get.panel()
  pts<-panel.tables()
  ##
  lapply(pts,function(pt){
    data.table::setcolorder(pt,c('panel.designation','location'))
  })
  ##
  kable.func<-function(dt){
    kableExtra::footnote(
      kable_input=kableExtra::kable_classic_2(
        kableExtra::kbl(
          dt
        ),
        full_width=FALSE,
        c('striped','hover')
      ),
      general_title = "Volume(s) uL:",
      general = "Add the indicated volume(s) to a Protein LoBind 1.5 mL tube",
      footnote_as_chunk = TRUE
    )
  }
  ##
  mixes<-lapply(split(pts$mix,by=c('panel.designation','location')),function(mix){
    drop.cols<-names(which(sapply(mix,function(j) all(is.na(j)))))
    kable.func(mix[,!..drop.cols])
  })
  ##
  buffers<-lapply(split(pts$buffer,by=c('panel.designation','location')),function(mix){
    drop.cols<-names(which(sapply(mix,function(j) all(is.na(j)))))
    kable.func(mix[,!..drop.cols])
  })
  ##
  list(
    mix=mixes,
    buffer=buffers
  )
}
