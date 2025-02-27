buffers.table<-function(buffers.type=c('AIM','CYTOKINE')){
  ##
  if(buffers.type=='AIM'){
    buffers.table<-data.table::copy(buffers.AIM)
  }else if(buffers.type=='CYTOKINE'){
    buffers.table<-data.table::copy(buffers.CYTOKINE)
  }
  ##
  buffers.table <- kableExtra::kable(
    x = buffers.table,
    format = "html",
    escape = FALSE
  )
  buffers.table <- kableExtra::kable_styling(
    kable_input = buffers.table,
    bootstrap_options = c("hover", "condensed", "striped")
  )
  return(buffers.table)
}
