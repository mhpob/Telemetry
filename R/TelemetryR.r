#' TelemetryR.
#'
#' @name TelemetryR
#' @docType package
#'
#' @importFrom dplyr "%>%"
#' @importFrom data.table ":="
NULL


#' Transmitters from A.C.T., Updated 2016-01-22
#'
#' Transmitters are submitted to the Atlantic Cooperative Telemetry Network
#' (\url{http://www.theactnetwork.com/}) by individual researchers and archived
#' on a private Dropbox folder.
#'
#' @author Lori Brown \email{east.coast.telemetry@@gmail.com}
#' @format A data frame with 25 variables and an evolving number of rows
#' \describe{
#'   \item{Tag.ID.Code.Standard}{Full transmitter code}
#'   \item{ID.Standard}{Transmitter number (last digits of transmitter code)}
#'   \item{Tag.ID.Code.Sensor.I, Tag.ID.Code.Sensor.II}{Optional transmitter
#'     sensor code}
#'   \item{ID.Sensor.I, ID.Sensor.II}{Optional transmitter sensor number (last
#'     digits of sensor code)}
#'   \item{Release.Date}{Date when tagged individual was released}
#'   \item{Tag.Life}{Estimated transmitter battery life}
#'   \item{Expiration.date}{Release date + tag life}
#'   \item{Tag.Family}{Style/type of tag, or manufaturer if not VEMCO}
#'   \item{Tag.Type}{Is the tag standard, or does it provide other environmental
#'     data?}
#'   \item{Primary.Tagging.Organization}{Organization who attached transmitter}
#'   \item{Primary.Researcher}{Main point of contact for detections}
#'   \item{Collaborators}{Auxiliary points of contact for detections}
#'   \item{Common.Name}{Common name of tagged species}
#'   \item{Scientific.Name}{Scientific name of tagged species}
#'   \item{Release.Location}{General location where tagged individual was
#'     released}
#'   \item{Pectoral.T.bar, Dorsal.T.bar, PIT.tag, Dart.tag,
#'     Roto.Flipper.Disc}{Accessory tag identifiers, if used}
#'   \item{Attachment.method}{How transmitter is attached (external, surgical
#'     implant, etc.)}
#'   \item{Comments}{Optional comments by researcher}
#' }
#' @docType data
#' @usage data(ACTtrans)
#' @source \url{http://www.theactnetwork.com/}
"ACTtrans"


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