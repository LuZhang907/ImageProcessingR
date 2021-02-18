# Internal function used to process data loaded by read_algoseek_equity_taq()
read_algoseek_equity_taq_ <- function(data)
{
  data$h <- lubridate::hour(data$Timestamp)
  data$m <- lubridate::minute(data$Timestamp)
  data$s <- lubridate::second(data$Timestamp)
  data$ms <- data$s%%1
  data$s <- data$s - data$ms
  
  
  # covert conditions to readable codes
  # R.utils::intToBin(strtoi(c('A0','00','20','20'), base = 16L))
  
  data
}


#' Load AlgoSeek Equity TAQ data from zip files
#' 
#' @param zipdata the original zip data provided by AlgoSeek
#' @param whichData the specific data to be loaded; by default load all data in the zip file
#' 
#' @examples
#' \donttest{
#' zipdata <- tempfile()
#' download.file("https://www.algoseek.com/static/files/sample_data/equity_and_etf_etn/IBM.TradesQuotes.012815.zip",zipdata)
#' dat <- read_algoseek_equity_taq(zipdata)
#' }
#
#' # Do not run unless the file 20180108.zip is avaliable
#' # dat <- read_algoseek_equity_taq("20180108.zip", whichData="AMZN.csv")
#' 
#' @author Larry Lei Hua
#' 
#' @export
read_algoseek_equity_taq <- function(zipdata, whichData=NULL)
{
  col_types <- readr::cols(
    Date = readr::col_date(format="%Y%m%d"),
    Timestamp = readr::col_time(format="%H:%M:%OS"),
    EventType = readr::col_character(),
    Ticker = readr::col_character(),
    Quantity = readr::col_integer(),
    Exchange = readr::col_character(),
    Conditions = readr::col_character()
  )
  
  if(is.null(whichData))
  {
    file_names <- utils::unzip(zipdata, list = TRUE)
    data_file_names <- subset(file_names, file_names$Length>0)
    output_file_names <- gsub("/", "_", data_file_names$Name)
    alldata <- lapply(data_file_names$Name, function(file){
      rawdata <- readr::read_csv(unz(zipdata, file), col_types=col_types)
      read_algoseek_equity_taq_(rawdata)
    })
    names(alldata) <- output_file_names
  }else
  {
    output_file_names <- gsub("/", "_", whichData)
    alldata <- lapply(whichData, function(file){
      rawdata <- readr::read_csv(unz(zipdata, file), col_types=col_types)
      read_algoseek_equity_taq_(rawdata)
    })
    names(alldata) <- output_file_names
  }
  alldata
}


# Internal function used to process data loaded by read_algoseek_equity_fullDepth()
read_algoseek_equity_fullDepth_ <- function(data)
{
  data$h <- lubridate::hour(data$Timestamp)
  data$m <- lubridate::minute(data$Timestamp)
  data$s <- lubridate::second(data$Timestamp)
  data$ms <- data$s %% 1
  data$s <- data$s - data$ms
  data
}

#' Load AlgoSeek equity Full Depth data from zip files
#' 
#' @param zipdata the original zip data provided by AlgoSeek
#' @param whichData the specific data to be loaded; by default load all data in the zip file
#' 
#' 
#' @examples
#' \donttest{
#' zipdata <- tempfile()
#' download.file("https://www.algoseek.com/static/files/sample_data/equity_and_etf_etn/IBM.FullDepth.20140128.csv.zip",zipdata)
#' dat <- read_algoseek_equity_fullDepth(zipdata)
#' }
#'
#' # Do not run unless the file 20180108.zip is avaliable
#' # dat <- read_algoseek_equity_fullDepth("20180108.zip", whichData="AMZN.csv")
#' 
#' @author Larry Lei Hua
#' 
#' @export
read_algoseek_equity_fullDepth <- function(zipdata, whichData=NULL)
{
  col_types <- readr::cols(
    Date = readr::col_number(),
    Timestamp = readr::col_time(format="%H:%M:%OS"),
    OrderNumber = readr::col_number(),
    EventType = readr::col_character(),
    Ticker = readr::col_character(),
    Price = readr::col_number(),
    Quantity = readr::col_integer(),
    MPID = readr::col_character(),
    Exchange = readr::col_character()
  )
  
  if(is.null(whichData))
  {
    file_names <- utils::unzip(zipdata, list = TRUE)
    data_file_names <- subset(file_names, file_names$Length>0)
    output_file_names <- gsub("/", "_", data_file_names$Name)
    alldata <- lapply(data_file_names$Name, function(file){
      rawdata <- readr::read_csv(unz(zipdata, file), col_types=col_types)
      read_algoseek_equity_fullDepth_(rawdata)
    })
    names(alldata) <- output_file_names
  }else
  {
    output_file_names <- gsub("/", "_", whichData)
    alldata <- lapply(whichData, function(file){
      rawdata <- readr::read_csv(unz(zipdata, file), col_types=col_types)
      read_algoseek_equity_fullDepth_(rawdata)
    })
    names(alldata) <- output_file_names
  }
  alldata
}
#' Obtain readable condition codes for AlgoSeek equity TAQ
#' 
#' @param con raw condition codes such as "a0000020" provided with Algoseek data
#' @return a vector of integers for the condition codes defined by algoseek
#' 
#' @examples
#' condition_algoseek_equity_taq("a0000020")
#' 
#' @author Larry Lei Hua
#' 
#' @export
condition_algoseek_equity_taq <- function(con)
{
  tmp <- strsplit(con, "")[[1]]
  tmp <- paste0(tmp[c(TRUE, FALSE)], tmp[c(FALSE, TRUE)])
  codes <- rev((31:0)[unlist(binaryLogic::as.binary(strtoi(tmp, base = 16L), n=8))])
  codes
}


#' Map conditions to conditions codes for AlgoSeek equity TAQ
#' 
#' @param codes a vector of integers for readable condition codes
#' @param tq a string to indicate "trade" or "quote"; default is "trade"
#' @return a vector of meanings of the condition codes defined by algoseek
#' 
#' @examples
#' condition_maps_algoseek_equity_taq(c(0, 9), tq="trade")
#' condition_maps_algoseek_equity_taq(c(0, 9), tq="quote")
#' 
#' @author Larry Lei Hua
#' 
#' @export
condition_maps_algoseek_equity_taq <- function(codes, tq="trade")
{
  cond_trade <- c("tRegular", 
                  "tCash",
                  "tNextDay",
                  "tSeller",
                  "tYellowFlag",
                  "tIntermarketSweep",
                  "tOpeningPrints",
                  "tClosingPrints",
                  "tReOpeningPrints",
                  "tDerivativelyPriced",
                  "tFormT",
                  "tSold",
                  "tStopped",
                  "tExtendedHours",
                  "tOutOfSequence",
                  "tSplit",
                  "tAcquisition",
                  "tBunched",
                  "tStockOption",
                  "tDistribution",
                  "tAveragePrice",
                  "tCross",
                  "tPriceVariation",
                  "tRule155",
                  "tOfficialClose",
                  "tPriorReferencePrice",
                  "tOfficialOpen",
                  "tCapElection",
                  "tAutoExecution",
                  "tTradeThroughExempt",
                  "",
                  "tOddLot")
  
  cond_quote <- c("qRegular",
                  "qSlow",
                  "qGap",
                  "qClosing",
                  "qNewsDissemination",
                  "qNewsPending",
                  "qTradingRangeIndication",
                  "qOrderImbalance",
                  "qClosedMarketMaker",
                  "qVolatilityTradingPause",
                  "qNonFirmQuote",
                  "qOpeningQuote",
                  "qDueToRelatedSecurity",
                  "qResume",
                  "qInViewOfCommon",
                  "qEquipmentChangeover",
                  "qSubPennyTrading",
                  "qNoOpenNoResume",
                  "qLimitUpLimitDownPriceBand",
                  "qRepublishedLimitUpLimitDownPriceBand",
                  "qManual",
                  "qFastTrading",
                  "qOrderInflux")
  if(tq=="trade")
  {
    out <- cond_trade[codes+1]
  }else
  {
    out <- cond_quote[codes+1]
  }
  out
}

