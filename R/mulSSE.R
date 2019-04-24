mulSSE <-
function( vec, index ){
   tapply( vec, index, function( vectmp ){ sum( vectmp ^ 2 ) - ( sum( vectmp ) ^ 2 ) / length( vectmp ) } )
}

