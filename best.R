## best.R identifies the "best" hospital in a particular state for a particular outcome

best <- function( state, outcome ) {
  ## Read outcome data
  
  hospData <- read.csv( 'data/outcome-of-care-measures.csv', colClasses='character' )
  
  outcomeList <- c( `heart attack` = 11, `heart failure` = 17, `pneumonia` = 23 )
  
  ## Check that state and outcome are valid
  if( !( state %in% hospData[, 'State' ] ) ) {
    stop( "invalid state" )
  }
  if( !( outcome %in% names( outcomeList ) ) ) {
    stop( "invalid outcome" )
  }
  
  ## Return hospital name in the state with lowest 30-day death rate
  workFrame <- data.frame( State = hospData[, 'State' ], MortRate = hospData[, outcomeList[[ outcome ]] ],
                           HospName = hospData[, 'Hospital.Name'], stringsAsFactors=FALSE)
  workFrame[, 'MortRate' ] <- suppressWarnings( as.numeric( workFrame[, 'MortRate' ] ) )
  
  workFrame <- subset( workFrame, State == state )
  workFrame <- workFrame[ order( workFrame$MortRate, workFrame$HospName ), ]
  
  return( workFrame[ 1, 'HospName' ] )
}
