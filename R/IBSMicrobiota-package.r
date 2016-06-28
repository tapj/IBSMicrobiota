#' IBSMicrobiota.
#'
#' @name IBSMicrobiota
#' @docType package
NULL


#' IBS data
#'
#' This dataset contains all metadata and microbial data from the 
#' IBS Simren study
#'
#'
#' @section Variables:
#'
#' \itemize{
#' \item \code{metadata}: Clinical metadata, sample type and gas exhaled
#'			\itemize{
#'    \item Patient_ID
#'    \item Visit
#'    \item Sample_type
#'    \item study_group
#'    \item Methanogens_qPCR
#'    \item Sample_ID
#'    \item Health
#'    \item Gender
#'    \item Age
#'    \item Height
#'    \item Weight
#'    \item BMI
#'    \item H2
#'    \item CH4
#'    \item H2_group
#'    \item CH4_group
#'    \item IBS.Severity.Score
#'    \item SSgroup
#'    \item Probiotics
#'    \item Lactulose_clusters
#'			}
#' \item \code{otu}: Microbial OTU table
#' \item \code{tax}: OTU Greengenes taxonomy
#' }
#' @docType data
#' @name IBSData
#' @usage IBSData
#' @format A list of 3 data frames
#' @examples
#' head(IBSData$metadata)
NULL
