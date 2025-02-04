#' @title Get a COVAIL protocol
#' @description
#' Get a COVAIL protocol and open the .html file (knit from R Markdown .Rmd) in a default browser window; the protocol can be directly printed from the browser window and will include per-page headers and footers.
#'
#' @param protocol.name Character string; an alias name for a particular COVAIL protocol:
#'    * 'staining.cytokine'
#' @returns A pre-formatted .html protocol
#' @export
#'
get.protocol<-function(protocol.name=c('staining.cytokine')){
  protocol.name<-match.arg(protocol.name)
  switch(protocol.name,
         staining.cytokine=utils::browseURL(
           list.files(
             system.file("protocols",package="COVAILworkflow"),
             pattern="staining_CYTOKINE.html", full.names = TRUE
           )
         )
  )
}
