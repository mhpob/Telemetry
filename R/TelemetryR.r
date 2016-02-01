#' TelemetryR.
#'
#' @name TelemetryR
#' @docType package
#'
#' @importFrom dplyr "%>%"
#' @importFrom data.table ":="
NULL


#' UTM shapefile of the Chesapeake Bay
#'
#' @format A large spatial polygons data frame
#' \describe{
#'    \item{System}{System identifiers of ploygons. Factor with 15 levels:
#'        \itemize{
#'            \item Choptank River
#'            \item Elizabeth River
#'            \item James River
#'            \item Lower Bay
#'            \item Lower Eastern Shore (Tangier)
#'            \item Lower Western Shore (MD)
#'            \item Mid Bay
#'            \item Patapsco and Back Rivers
#'            \item Patuxent River
#'            \item Potomac River
#'            \item Rappahannock River
#'            \item Upper Bay
#'            \item Upper Eastern Shore
#'            \item Upper Western Shore
#'            \item York River
#'        }
#'    }
#' }
#' @docType data
#' @usage data(chesapeake)
#' @source Shapefile used by the \href{http://ian.umces.edu}{Integration and
#'    Application Network} for the Chesapeake Bay Report Card. More information in:
#'    Michael Williams, Ben Longstaff, Claire Buchanan, Roberto Llansó,
#'    William Dennison, Development and evaluation of a spatially-explicit index
#'    of Chesapeake Bay health, Marine Pollution Bulletin, Volume 59, Issues 1-3,
#'    2009, Pages 14-25.
"chesapeake"