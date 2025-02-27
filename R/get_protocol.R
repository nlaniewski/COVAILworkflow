#' @title Get a COVAIL protocol
#' @description
#' Get a COVAIL protocol and open the .html file (knit from R Markdown .Rmd) in a default browser window; the protocol can be directly printed from the browser window and will include per-page headers and footers.
#'
#' @param protocol.name Character string; an alias name for a particular COVAIL protocol:
#'    * 'staining.CYTOKINE'
#'    * 'staining.AIM'
#' @returns A pre-formatted .html protocol
#' @export
#'
get.protocol<-function(protocol.name=c('staining_CYTOKINE','staining_AIM')){
  ##
  protocol.name<-match.arg(protocol.name)
  protocol.name<-sprintf("COVAIL_protocol_%s.Rmd",protocol.name)
  utils::browseURL(
    rmarkdown::render(
      input = list.files(
        system.file("protocols",package="COVAILworkflow"),
        pattern=protocol.name,
        full.names = TRUE
      ),
      output_dir = tempdir(),
      output_file = sub(".Rmd","",protocol.name)
    )
  )
}
