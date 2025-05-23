reagents.table<-function(reagent.type=c('AIM','CYTOKINE'),table.type=c('kable')){
  table.type<-match.arg(table.type)
  ##
  if(reagent.type=='AIM'){
    reagents.table<-data.table::copy(reagents.AIM)
  }else if(reagent.type=='CYTOKINE'){
    reagents.table<-data.table::copy(reagents.CYTOKINE)
  }
  if(table.type=='kable'){
    reagents.table[,Reagent := kableExtra::cell_spec(Reagent,"html",link=href)][,href:=NULL]
    reagents.table <- kableExtra::kable(
      x = reagents.table,
      format = "html",
      escape = FALSE
    )
    reagents.table <- kableExtra::kable_styling(
      kable_input = reagents.table,
      bootstrap_options = c("hover", "condensed", "striped")
    )
  }
  # if(table.type=='flextable'){
  #   reagents.table<-flextable::flextable(
  #     data = reagents,
  #     col_keys = grep("href",names(reagents),invert = T,value = T)
  #   )
  #   reagents.table<-flextable::compose(
  #     x = reagents.table,
  #     j = "Reagent",
  #     value = flextable::as_paragraph(
  #       flextable::hyperlink_text(
  #         x = Reagent,
  #         url = href
  #       )
  #     )
  #   )
  # }
  ##
  return(reagents.table)
}
