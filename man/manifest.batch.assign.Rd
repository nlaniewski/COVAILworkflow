% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manifest_batch_assign.R
\name{manifest.batch.assign}
\alias{manifest.batch.assign}
\title{Add batch assignments to a BLIS COVAIL manifest}
\usage{
manifest.batch.assign(
  manifest,
  vials.per = 3,
  add.controls = TRUE,
  seed.val = 1337,
  randomize.batch.order = FALSE,
  workbook.path = NULL
)
}
\arguments{
\item{manifest}{a \link[data.table]{data.table} as returned from \link{BLIS.manifest.prepare}.}

\item{vials.per}{numeric; the number of sample-specific vials to assign to each batch.}

\item{add.controls}{logical; default \code{TRUE}. Adds healthy adult control PBMCs ('HD####' placeholder name) to each batch.}

\item{seed.val}{numeric; a seed value used when randomly sampling subject.ids and/or batch order.}

\item{randomize.batch.order}{logical; if \code{TRUE}, the batch order will be randomized instead of ordered by subject.id + visit.}

\item{workbook.path}{file.path (character string); if defined, an excel (.xlsx) workbook will be created.}
}
\value{
a list containing two \code{data.table}s; a long-data manifest (individual vials) and a collapsed manifest.
}
\description{
Samples from a BLIS COVAIL manifest, as returned by \link{BLIS.manifest.prepare}, are assigned a 'batch.seq' and a 'batch.order'; depending on study requirements, both of these assignments can be randomized.  Based on these assignments, batch-specific 'staging' boxes are pre-mapped. Current (2024-11-22) batch assignment conditions:
\itemize{
\item 27+3; 30 unique samples per barcoded pool:
\itemize{
\item 3 subjects + 1 Healthy Adult Donor PBMC
\item 3 visits + 1 Healthy Adult Donor PBMC 'visit'
\item 3 stimulation conditions
}
\item (27x2)+(3x2); 60 unique samples per barcoded pool/per panel/plate
\itemize{
\item 2 panels/plates (AIM, CYTOKINE)
}
\item ((27x2)+(3x2))x2; 120 unique samples per two barcoded pools/per panel/plate
}
}
\examples{
manifest.BLIS.path<-list.files(
system.file("extdata",package="COVAILworkflow"),
pattern="SpecimenDetail", full.names = TRUE
)

manifest.physical.path<-list.files(
system.file("extdata",package="COVAILworkflow"),
pattern="physical", full.names = TRUE
)

manifest<-BLIS.manifest.prepare(manifest.BLIS.path,manifest.physical.path)

fp<-file.path(tempdir(),"COVAIL_manifest_batched.xlsx")
if(file.exists(fp)){file.remove(fp)}
invisible(manifest.batch.assign(manifest,vials.per=3,workbook.path=fp))
data.table::setDT(openxlsx::read.xlsx(fp,sheet="manifest_collapsed"))[]
}
