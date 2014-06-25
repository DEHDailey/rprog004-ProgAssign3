## rankhospital.R -- We want the hospital within a state that has a particular ranking on a certain outcome.
## 1 = best (lowest) mortality rate

## We will duplicate some code from best.R

rankhospital <- function( state, outcome, num = 'best' ) {
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
  
  workFrame <- subset( workFrame, State == state & !is.na( MortRate ) )
  workFrame <- workFrame[ order( workFrame$MortRate, workFrame$HospName ), ]
  
  numNum <- suppressWarnings( as.numeric( num ) )
  if( is.na( numNum ) ) {
    numNum <- if( num == 'best' ) 1 else if ( num == 'worst' ) nrow( workFrame )
  }
  
  if( numNum <= 0 || numNum > nrow( workFrame ) || is.na( numNum ) ) return( NA )
  
  return( workFrame[ numNum, 'HospName' ] )
}