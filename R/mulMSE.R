mulMSE <-
function( vector, index, tmp = vector()){
   if( min( as.numeric( index == 0) ) != 0){
      tmo <- vector[ ( length( vector ) / 2 + 1 ):length( vector ) ]-vector[ 1 : ( length( vector ) / 2 )]
      d <- as.factor( index[ length( vector ):( length( vector ) / 2 + 1 ) ] )
      names( tmo ) <- d
      #return(tmo)
      hm <- harmonicMean( names ( tmo ) )
      #return(hm)
      b <- sqrt( sum( mulSSE( tmo, d ) )/( length( tmo ) - length( levels( factor( d ) ) ) ) * 2 / hm )
      return( b )
   }else{
      hm <- harmonicMean( index )
      sqrt( sum( mulSSE( vector, index ))/( length( vector )-length( levels( factor( index ) ) ) ) * 2 / hm )
   }
}

