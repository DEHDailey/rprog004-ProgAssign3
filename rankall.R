## Ranking hospitals in all states

rankall <- function( outcome, num='best' ) {
  ## Read outcome data
  
  hospData <- read.csv( 'data/outcome-of-care-measures.csv', colClasses='character' )
  
  outcomeList <- c( `heart attack` = 11, `heart failure` = 17, `pneumonia` = 23 )
  
  ## Check that outcome is valid
  if( !( outcome %in% names( outcomeList ) ) ) {
    stop( "invalid outcome" )
  }
  
  ## Return hospital name in each state with the identified ranking for 30-day death rate
  workFrame <- data.frame( State = hospData[, 'State' ], MortRate = hospData[, outcomeList[[ outcome ]] ],
                           HospName = hospData[, 'Hospital.Name'], stringsAsFactors=FALSE)
  workFrame[, 'MortRate' ] <- suppressWarnings( as.numeric( workFrame[, 'MortRate' ] ) )
  workFrame <- subset( workFrame, !is.na( MortRate ) )
  
  splitByStates <- split( workFrame, workFrame$State )  ## Result will be sorted by states, automatically
  extractedData <- data.frame( t( sapply( splitByStates, function( sbs ) {
    sbs <- sbs[ order( sbs$MortRate, sbs$HospName ), ]
    numNum <- suppressWarnings( as.numeric( num ) )
    if( is.na( numNum ) ) {
      numNum <- if( num == 'best' ) 1 else if ( num == 'worst' ) nrow( sbs )
    }
    if( numNum <= 0 || numNum > nrow( sbs ) || is.na( numNum ) ) return( c( hospital = NA, state = NA ) )
    
    return( c( hospital = sbs[ numNum, 'HospName' ], state = sbs[1, 'State' ] ) )
    
  } ) ) )
  
  return( extractedData )
}