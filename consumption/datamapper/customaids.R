require(systemfit)
require(miscTools)


run_test <-function(dat){
  if (missing(dat)){
    data( Blanciforti86 )
    Blanciforti86 <- Blanciforti86[ 1:32, ]
    
  } else {
    Blanciforti86 <- dat
  }
  customAidsEst( c( "pFood1", "pFood2", "pFood3", "pFood4" ),
                 c( "wFood1", "wFood2", "wFood3", "wFood4" ), "xFood",
                 data = Blanciforti86, priceIndex = "SL", maxiter = 100 , method = "MK")
  
}

customAidsCalc <- function( priceNames, totExpName, coef, data,
                      priceIndex = "TL", basePrices = NULL, baseShares = NULL ) {
  
  # check argument 'coef' (coefficients)
  coefCheckResult <- customAidsCheckCoef( coef, variables = list(
    list( length( priceNames ), "prices", "goods"  ) ) )
  if( !is.null( coefCheckResult ) ){
    stop( coefCheckResult )
  }
  
  # checking argument 'data'
  if( class( data ) != "data.frame" ) {
    stop( "argument 'data' must be a data frame" )
  }
  
  # checking (mainly) argument 'priceIndex'
  if( is.character( priceIndex ) ) {
    if( ! priceIndex %in% c( "TL", "S", "SL", "P", "L", "Ls", "T" ) ) {
      stop( "argument 'priceIndex' must be either",
            " 'TL' (translog), 'S' (Stone), 'SL' (Stone index with lagged shares),",
            " 'P' (Paasche), 'L' (Laspeyres),",
            " 'Ls' (Laspeyres, simplified), 'T' (Tornqvist), or a numeric vector",
            " providing the log values of the price index" )
    }
    if( priceIndex == "TL" && is.null( coef$alpha0 ) ) {
      stop( "calculations with the translog (TL) price index require",
            " coefficient alpha_0 (coef$alpha0)" )
    }
  } else if( is.numeric( priceIndex ) ) {
    if( length( priceIndex ) != nrow( data ) ) {
      stop( "if argument 'priceIndex' provides the values",
            " of the log price index,",
            " it must have the same length as there are observations",
            " in argument 'data'" )
    }
  } else {
    stop( "argument 'priceIndex' must be either a character string",
          " or a numeric vector" )
  }
  
  # tests for arguments basePrices and baseShares
  if( is.character( priceIndex ) ) {
    # basePrices
    if( priceIndex %in% c( "P", "L", "T" ) ) {
      if( is.null( basePrices ) ) {
        stop( "calculations with Paasche ('L'), Laspeyres ('L'),",
              " or Tornqvist ('T') price index require",
              " argument 'basePrices'" )
      }
      if( ! is.numeric( basePrices ) ) {
        stop( "argument 'basePrices' must be numeric" )
      }
      if( length( basePrices ) != length( priceNames ) ) {
        stop( "arguments 'basePrices' and 'priceNames' must have",
              " the same length" )
      }
    }
    # baseShares
    if( priceIndex %in% c( "L", "Ls", "T" ) ) {
      if( is.null( baseShares ) ) {
        stop( "calculations with Laspeyres ('Ls'),",
              " simplified Laspeyres ('Ls'), or",
              " Tornqvist ('T') price index require",
              " argument 'baseShares'" )
      }
      if( ! is.numeric( baseShares ) ) {
        stop( "argument 'baseShares' must be numeric" )
      }
      if( length( baseShares ) != length( priceNames ) ) {
        stop( "arguments 'baseShares' and 'priceNames' must have",
              " the same length" )
      }
    }
  }
  
  if( is.character( priceIndex ) ) {
    if( priceIndex == "TL" ) {
      # calculation of translog price index
      priceIndex <- customAidsPx( priceIndex, priceNames, data = data, coef = coef )
    } else if( priceIndex == "L" ) {
      # calculation of Laspeyres price index
      priceIndex <- customAidsPx( priceIndex, priceNames, data = data,
                            coef = coef, base = list( prices = basePrices, shares = baseShares ) )
    } else if( priceIndex == "Ls" ) {
      # calculation of simplified Laspeyres price index
      priceIndex <- customAidsPx( priceIndex, priceNames, data = data,
                            coef = coef, base = list( shares = baseShares ) )
    }
  }
  
  # number of goods
  nGoods <- length( priceNames )
  
  shareData <- as.data.frame( matrix( NA, nrow = nrow( data ), ncol = nGoods ) )
  names( shareData ) <- paste( "w", as.character( 1:nGoods ), sep = "" )
  rownames( shareData ) <- rownames( data )
  quant <- as.data.frame( matrix( 0, nrow = nrow( data ), ncol = nGoods ) )
  names( quant ) <- paste( "q", as.character( 1:nGoods ), sep = "" )
  rownames( quant ) <- rownames( data )
  if( is.numeric( priceIndex ) ) {
    for( i in 1:nGoods ) {
      shareData[ , i ] <- coef$alpha[ i ] + coef$beta[ i ] *
        ( log( data[[ totExpName ]] ) - priceIndex )
      for( j in 1:nGoods ) {
        shareData[ , i ] <- shareData[ , i ] + coef$gamma[ i, j ] *
          log( data[[ priceNames[ j ] ]] )
      }
    }
  } else if( priceIndex == "S" ) {
    for( i in 1:nrow( data ) ) {
      logPrices <- log( as.numeric( data[ i, priceNames ] ) )
      logTotExp <- log( data[ i, totExpName ] )
      if( all( !is.na( c( logPrices, logTotExp ) ) ) ) {
        shareData[ i, ] <-
          solve( diag( nGoods ) + coef$beta %*% t( logPrices ),
                 coef$alpha + coef$gamma %*% logPrices + coef$beta * logTotExp )
      }
    }
  } else if( priceIndex == "SL" ) {
    logPrices <- log( as.numeric( data[ 1, priceNames ] ) )
    logTotExp <- log( data[ 1, totExpName ] )
    if( all( !is.na( c( logPrices, logTotExp ) ) ) ) {
      shareData[ 1, ] <-
        solve( diag( nGoods ) + coef$beta %*% t( logPrices ),
               coef$alpha + coef$gamma %*% logPrices + coef$beta * logTotExp )
    }
    for( i in 2:nrow( data ) ) {
      logPrices <- log( as.numeric( data[ i, priceNames ] ) )
      logTotExp <- log( data[ i, totExpName ] )
      if( all( !is.na( c( logPrices, logTotExp ) ) ) ) {
        shareData[ i, ] <-
          coef$alpha + coef$gamma %*% logPrices + coef$beta * logTotExp -
          coef$beta * drop( crossprod( logPrices, as.numeric( shareData[ i-1, ] ) ) )
      }
    }
  } else if( priceIndex == "P" ) {
    for( i in 1:nrow( data ) ) {
      prices <- as.numeric( data[ i, priceNames ] )
      logTotExp <- log( data[ i, totExpName ] )
      if( all( !is.na( c( prices, logTotExp ) ) ) ) {
        shareData[ i, ] <-
          solve( diag( nGoods ) + coef$beta %*% t( log( prices / basePrices ) ),
                 coef$alpha + coef$gamma %*% log( prices ) + coef$beta * logTotExp )
      }
    }
  } else if( priceIndex == "T" ) {
    for( i in 1:nrow( data ) ) {
      prices <- as.numeric( data[ i, priceNames ] )
      logTotExp <- log( data[ i, totExpName ] )
      if( all( !is.na( c( prices, logTotExp ) ) ) ) {
        shareData[ i, ] <-
          solve( diag( nGoods ) + 0.5 * coef$beta %*%
                   t( log( prices / basePrices ) ),
                 coef$alpha + coef$gamma %*% log( prices ) +
                   coef$beta * logTotExp - 0.5 * coef$beta *
                   drop( crossprod( log( prices / basePrices ), baseShares ) ) )
      }
    }
  } else {
    stop( "internal error" )
  }
  for( i in 1:nGoods ) {
    quant[ , i ] <- shareData[ , i ] * data[[ totExpName ]] / data[[ priceNames[ i ] ]]
  }
  result <- list()
  result$shares <- shareData
  result$quant  <- quant
  return( result )
}


customAidsJacobian <- function( allCoef, priceNames, totExpName, data = NULL,
                           shifterNames = NULL, omitLast = TRUE, alpha0 = 0 ) {
  nObs <- nrow( data )
  nGoods <- length( priceNames )
  nShifter <- length( shifterNames )
  nExogEq <- 2 + nGoods + nShifter
  coef <- customAidsCoef( allCoef, nGoods = nGoods, nShifter = nShifter, alpha0 = alpha0 )
  hom <- all.equal( rowSums( coef$gamma ), rep( 0, nGoods ) ) == TRUE
  sym <- all.equal( coef$gamma, t( coef$gamma ) ) == TRUE
  lnp <- customAidsPx( "TL", priceNames, coef = coef, data = data )
  result <- matrix( 0, nrow = nObs * ( nGoods - 1 ),
                    ncol = nExogEq * ( nGoods - 1 ) )
  colnames( result ) <- customAidsCoefNamesEst( nGoods, nShifter,
                                           hom = FALSE, sym = FALSE )
  aName <- paste( "alpha", c( 1:nGoods ) )
  bName <- paste( "beta", c( 1:nGoods ) )
  gName <- matrix( paste( "gamma", rep( 1:nGoods, nGoods ),
                          rep( 1:nGoods, each = nGoods ) ), nrow = nGoods, ncol = nGoods )
  if( nShifter > 0 ) {
    dName <- matrix( paste( "delta", rep( 1:nGoods, nShifter ),
                            rep( 1:nShifter, each = nGoods ) ), nrow = nGoods, ncol = nShifter )
  }
  
  for( eq in 1:( nGoods - 1 ) ) {
    myRows <- ( ( eq - 1 ) * nObs + 1 ):( eq * nObs )
    # derivatives of alphas
    for( i in 1:( nGoods - 1 ) ) {
      result[ myRows, aName[ i ] ] <- ( i == eq ) -
        coef$beta[ eq ] *
        ( log( data[[ priceNames[ i ] ]] ) -
            log( data[[ priceNames[ nGoods ] ]] ) )
    }
    # derivatives of betas
    result[ myRows, bName[ eq ] ] <- log( data[[ totExpName ]] ) - lnp
    # derivatives of gammas
    for( i in 1:( nGoods - 1 ) ) {
      for( j in 1:( nGoods - hom ) ) {
        result[ myRows, gName[ i, j ] ] <-
          ( i == eq ) * ( log( data[[ priceNames[ j ] ]] ) -
                            hom * log( data[[ priceNames[ nGoods ] ]] ) ) -
          0.5 * coef$beta[ eq ] *
          ( log( data[[ priceNames[ i ] ]] ) -
              log( data[[ priceNames[ nGoods ] ]] ) ) *
          ( log( data[[ priceNames[ j ] ]] ) -
              hom * log( data[[ priceNames[ nGoods ] ]] ) )
      }
    }
    # derivatives of deltas
    if( nShifter > 0 ) {
      for( i in 1:nShifter ) {
        result[ myRows, dName[ eq, i ] ] <-
          data[[ shifterNames[ i ] ]]
      }
    }
  }
  delCols <- NULL
  for( i in 1:( nGoods - 1 ) ) {
    if( hom ) {
      delCols <- c( delCols, gName[ i, nGoods ] )
    }
    if( sym && i >= 2 ) {
      for( j in 1:( i - 1 ) ) {
        delCol <- gName[ i, j ]
        stayCol <- gName[ j, i ]
        result[ , stayCol ] <- result[ , stayCol ] + result[ , delCol ]
        delCols <- c( delCols, delCol )
      }
    }
  }
  if( !is.null( delCols ) ) {
    result <- result[ , !colnames( result ) %in% delCols ]
  }
  return( result )
}


customAidsSystem <- function( nGoods, nShifter = 0, LA = TRUE ) {
  if( LA ) {
    system <- list()
    for(i in 1:( nGoods - 1 ) ) {
      system[[ i ]] <- paste( "w", as.character( i ), " ~ lxtr", sep = "" )
      for( j in 1:nGoods ) {
        system[[ i ]] <- paste( system[[ i ]], " + lp",
                                as.character( j ), sep = "" )
      }
      if( nShifter > 0 ) {
        for( j in 1:nShifter ) {
          system[[ i ]] <- paste( system[[ i ]], " + s",
                                  as.character( j ), sep = "" )
        }
      }
      system[[ i ]] <- as.formula( system[[ i ]] )
    }
  }
  return( system )
}

customAidsCoefNamesEst <- function( nGoods, nShifter, hom, sym ) {
  result <- NULL
  for( i in 1:( nGoods - 1 ) ) {
    result <- c( result, paste( "alpha", i ),
                 paste( "beta", i ) )
    start <- ifelse( sym , i, 1 )
    stop  <- ifelse( hom , nGoods - 1, nGoods )
    for( j in start:stop ){
      result <- c( result, paste( "gamma", i, j ) )
    }
    if( nShifter > 0 ) {
      for( j in 1:nShifter ){
        result <- c( result, paste( "delta", i, j ) )
      }
    }
  }
  return( result )
}

customAidsCoefNamesAll <- function( nGoods, nShifter ) {
  result <- c(
    paste( "alpha", c( 1:nGoods ) ),
    paste( "beta", c( 1:nGoods ) ),
    paste( "gamma", rep( 1:nGoods, each = nGoods ),
           rep( 1:nGoods, nGoods ) ) )
  if( nShifter > 0 ) {
    result <- c( result,
                 paste( "delta", rep( 1:nGoods, each = nShifter ),
                        rep( 1:nShifter, nGoods ) ) )
  }
  return( result )
}


customAidsCoef <- function( coef, nGoods, nShifter = 0, cov = NULL, df = 1,
                       LA = TRUE, priceNames = NULL, shareNames = NULL, shifterNames = NULL,
                       alpha0 = NULL ) {
  # nGoods <- -0.5 + ( 2.25 + nrow( array( coef ) ) )^0.5
  nExogEq <- nGoods + 2 + nShifter
  if( LA ) {
    M <- matrix( 0, nGoods * nExogEq, ( nGoods - 1 ) * nExogEq )
    rownames( M ) <- customAidsCoefNamesAll( nGoods, nShifter )
    colnames( M ) <- customAidsCoefNamesEst( nGoods, nShifter, hom = FALSE,
                                        sym = FALSE )
    aName <- paste( "alpha", c( 1:nGoods ) )
    bName <- paste( "beta", c( 1:nGoods ) )
    gName <- matrix( paste( "gamma", rep( 1:nGoods, nGoods ),
                            rep( 1:nGoods, each = nGoods ) ), nrow = nGoods, ncol = nGoods )
    if( nShifter > 0 ) {
      dName <- matrix( paste( "delta", rep( 1:nGoods, nShifter ),
                              rep( 1:nShifter, each = nGoods ) ), nrow = nGoods, ncol = nShifter )
    }
    for(i in 1:( nGoods - 1 ) ) {
      M[ aName[ i ], aName[ i ] ]   <-  1   # alpha[i]
      M[ aName[ nGoods ], aName[ i ] ]   <- -1   # alpha[nGoods]
      M[ bName[ i ], bName[ i ] ] <-  1   # beta[i]
      M[ bName[ nGoods ], bName[ i ] ] <- -1 # beta[ nGoods ]
      for( j in 1:nGoods ) {
        M[ gName[ i, j ], gName[ i, j ] ] <-  1   # gamma[i,j]
        M[ gName[ nGoods, j ], gName[ i, j ] ] <- -1   # gamma[nGoods,j]
      }
      if( nShifter > 0 ){
        for( j in 1:nShifter ) {
          M[ dName[ i, j ], dName[ i, j ] ] <-  1   # gamma[i,j]
          M[ dName[ nGoods, j ], dName[ i, j ] ] <- -1   # gamma[nGoods,j]
        }
      }
    }
    all     <- c( M %*% coef )
    names( all ) <- customAidsCoefNamesAll( nGoods, nShifter )
    all[ aName[ nGoods ] ]  <- all[ aName[ nGoods ] ] + 1
    alpha   <- all[1:nGoods]
    beta    <- all[(nGoods+1):(2*nGoods)]
    gamma   <- matrix( all[(2*nGoods+1):(nGoods*(nGoods+2))],
                       nrow = nGoods, ncol = nGoods, byrow = TRUE )
    if( nShifter > 0 ){
      delta <- matrix( all[ ( nGoods * ( nGoods + 2 ) + 1 ):length( all ) ],
                       nrow = nGoods, ncol = nShifter, byrow = TRUE )
    } else {
      delta <- NULL
    }
    allcov <- NULL
    stat    <- NULL
    if(!is.null(cov)) {
      allcov   <- M %*% cov %*% t(M)
      rownames( allcov ) <- names( all )
      colnames( allcov ) <- names( all )
      stat     <- coefTable( all, sqrt( diag( allcov ) ), df )
    }
  }
  if( !is.null( shareNames ) ) {
    names( alpha ) <- shareNames
    names( beta ) <- shareNames
    rownames( gamma ) <- shareNames
    if( !is.null( delta ) ){
      rownames( delta ) <- shareNames
    }
  }
  if( !is.null( priceNames ) ) {
    colnames( gamma ) <- priceNames
  }
  if( !is.null( delta ) & !is.null( shifterNames ) ){
    colnames( delta ) <- shifterNames
  }
  result <- list()
  result$alpha0 <- alpha0
  result$alpha <- alpha
  result$beta  <- beta
  result$gamma <- gamma
  result$delta <- delta
  result$all   <- all
  result$allcov <- allcov
  result$stat  <- stat
  return( result )
}

customAidsCheckCoef <- function( coef, argCoef = "coef",
                            variables = NULL ) {
  
  # checking argument 'variables' of *this* function
  if( !is.null( variables ) ){
    if( !is.list( variables ) ){
      stop( "internal error: argument 'variables' must be of class 'list'" )
    }
    for( i in 1:length( variables ) ){
      if( !is.list( variables[[ i ]] ) ){
        stop( "internal error: each element of argument 'variables'",
              " must be of class 'list'" )
      }
      if( length( variables[[ i ]] ) != 3 ){
        stop( "internal error: each element of argument 'variables'",
              " must have 3 elements" )
      }
      if( !( is.numeric( variables[[ i ]][[ 1 ]] ) || 
             is.na( variables[[ i ]][[ 1 ]] ) ) || 
          length( variables[[ i ]][[ 1 ]] ) != 1 ){
        stop( "internal error: 'variables[[ ", i , " ]][[ 1 ]]' must be",
              " a numeric scalar" )
      }
      if( !is.character( variables[[ i ]][[ 2 ]] ) ){
        stop( "internal error: 'variables[[ ", i , " ]][[ 2 ]]' must be",
              " a character string" )
      }
      if( !( variables[[ i ]][[ 3 ]] %in% c( "goods", "shifters" ) ) ){
        stop( "internal error: 'variables[[ ", i , " ]][[ 3 ]]' must be",
              " either 'goods' or 'shifters'" )
      }
    }
  }
  
  ## checking coefficients
  # alpha
  if( is.null( coef$alpha ) ){
    return( paste( "'", argCoef, "$alpha' is missing", sep = "" ) )
  }
  if( !is.numeric( coef$alpha ) ){
    return( paste( "'", argCoef, "$alpha' must be numeric vector", sep = "" ) )
  }
  
  # beta
  if( is.null( coef$beta ) ){
    return( paste( "'", argCoef, "$beta' is missing", sep = "" ) )
  }
  if( !is.numeric( coef$beta ) ){
    return( paste( "'", argCoef, "$beta' must be numeric vector", sep = "" ) )
  }
  if( length( coef$alpha ) != length( coef$beta ) ) {
    return( paste( "'", argCoef, "$alpha' and '", argCoef, "$beta'",
                   " must have the same length", sep = "" ) )
  }
  
  # gamma
  if( is.null( coef$gamma ) ){
    return( paste( "'", argCoef, "$gamma' is missing", sep = "" ) )
  }
  if( !is.matrix( coef$gamma ) ){
    return( paste( "'", argCoef, "$gamma' must be a matrix", sep = "" ) )
  }
  if( nrow( coef$gamma ) != ncol( coef$gamma ) ) {
    return( paste( "argument '", argCoef, "$gamma' must be a square matrix",
                   sep = "" ) )
  }
  if( length( coef$alpha ) != nrow( coef$gamma ) ) {
    return( paste( "the number of rows of '", argCoef, "$gamma'",
                   " must be equal to the length of '", argCoef, "$alpha'",
                   sep = "" ) )
  }
  
  # delta
  if( !is.null( coef$delta ) ){
    if( !is.matrix( coef$delta ) ){
      return( paste( "'", argCoef, "$delta' must be a matrix", sep = "" ) )
    }
    if( length( coef$alpha ) != nrow( coef$delta ) ) {
      return( paste( "the number of rows of '", argCoef, "$delta'",
                     " must be equal to the length of '", argCoef, "$alpha'",
                     sep = "" ) )
    }
  }
  
  # checking variables
  if( !is.null( variables ) ){
    for( i in 1:length( variables ) ){
      if( variables[[ i ]][[ 3 ]] == "goods" ){ 
        if( variables[[ i ]][[ 1 ]] != length( coef$alpha ) && 
            !is.na( variables[[ i ]][[ 1 ]] ) ) {
          return( paste( "'", argCoef, "$alpha' and '", variables[[ i ]][[ 2 ]],
                         "' must have the same length", sep = "" ) )
        }
      } else if( variables[[ i ]][[ 3 ]] == "shifters" ){
        if( variables[[ i ]][[ 1 ]] != ncol( coef$delta ) && 
            !is.na( variables[[ i ]][[ 1 ]] ) ) {
          return( paste( "the number of columns of '", argCoef, "$delta'",
                         " must be equal to the length of '", variables[[ i ]][[ 2 ]], "'", 
                         sep = "" ) )
        }
      } else {
        stop( "internal error: 'variables[[ ", i , " ]][[ 3 ]]' must be",
              " either 'goods' or 'shifters'" )
      }
    }
  }
  
  return( NULL )
}
customAidsRestr <- function( nGoods, nShifter = 0, hom = TRUE, sym = TRUE,
                        LA = TRUE, restrict.regMat = FALSE ) {
  
  if( sym && !hom ) {
    hom <- TRUE  # symmetry implies homogeneity
    warning( "symmetry implies homogeneity: imposing additionally homogeniety" )
  }
  nExogEq <- nGoods + 2 + nShifter # number of exog. variables per equation
  if( restrict.regMat ) {
    nExog <- ( nGoods - 1 ) * ( nExogEq )
    restr <- diag( nExog )
    delCols <- NULL
    if( hom ) {
      for( i in 1:( nGoods - 1 ) ) {
        delCol <- ( i - 1 ) * ( nExogEq ) + 2 + nGoods
        addCols <- ( ( i - 1 ) * ( nExogEq ) + 3 ):(
          ( i - 1 ) * ( nExogEq ) + 2 + ( nGoods - 1 ) )
        restr[ delCol, addCols ] <- -1
        delCols <- c( delCols, delCol )
      }
    }
    if( sym ) {
      for( i in 1:( nGoods - 2 ) ) {
        for( j in ( i + 1 ):( nGoods - 1 ) ) {
          delCol <- ( j - 1 ) * ( nExogEq ) + 2 + i
          addCol <- ( i - 1 ) * ( nExogEq ) + 2 + j
          restr[ , addCol ] <- restr[ , addCol ] + restr[ , delCol ]
          delCols <- c( delCols, delCol )
        }
      }
    }
    if( hom || sym ) {
      restr <- restr[ , -delCols ]
    } else {
      restr <- NULL
    }
    if( !is.null( restr ) ) {
      rownames( restr ) <- customAidsCoefNamesEst( nGoods = nGoods,
                                              nShifter = nShifter, hom = FALSE, sym = FALSE )
      colnames( restr ) <- customAidsCoefNamesEst( nGoods = nGoods,
                                              nShifter = nShifter, hom = hom, sym = sym )
    }
  } else {
    restr <- NULL
    if( LA ) {
      if( hom ) {
        restr <- matrix( 0, nGoods - 1, ( nGoods - 1 ) * ( nExogEq ) )
        for( i in 1:( nGoods - 1 ) ) {
          for( j in 1:nGoods ) {
            restr[ i, ( i - 1 ) * ( nExogEq ) + 2 + j ] <- 1
          }
        }
      }
      if( sym ) {
        restr <- rbind( restr, matrix( 0, ( nGoods - 1 ) * ( nGoods - 2 ) / 2,
                                       ( nGoods - 1 ) * ( nExogEq ) ) )
        k <- 0
        for( i in 1:( nGoods - 2 ) ) {
          for( j in ( i + 1 ):( nGoods - 1 ) ) {
            k <- k + 1
            restr[ nGoods - 1 + k, ( i - 1 ) * ( nExogEq ) +
                     2 + j ] <-  1
            restr[ nGoods - 1 + k, ( j - 1 ) * ( nExogEq ) +
                     2 + i ] <- -1
          }
        }
      }
    }
    if( !is.null( restr ) ) {
      colnames( restr ) <- customAidsCoefNamesEst( nGoods = nGoods,
                                              nShifter = nShifter, hom = FALSE, sym = FALSE )
    }
  }
  return( restr )
}

customAidsPx <- function (priceIndex, priceNames, data, shareNames = NULL, base = 1, 
          coef = NULL, shifterNames = NULL) 
{
  if (priceIndex == "TL") {
    if (is.null(coef)) {
      stop("argument 'coef' must be specified to calculate the translog", 
           " price index")
    }
    else {
      coefCheckResult <- customAidsCheckCoef(coef, variables = list(list(length(priceNames), 
                                                                    "priceNames", "goods"), list(ifelse(is.null(shareNames), 
                                                                                                        NA, length(shareNames)), "shareNames", "goods")))
      if (!is.null(coefCheckResult)) {
        stop(coefCheckResult)
      }
      if (is.null(coef$alpha0)) {
        stop("argument 'coef' must have element 'alpha0'")
      }
    }
  }
  else {
    if (is.null(shareNames) && !(priceIndex %in% c("L", 
                                                   "Ls") && class(base) == "list")) {
      stop("argument 'shareNames' must must be specified to calculate", 
           " price index '", priceIndex, "'")
    }
  }
  nGoods <- length(priceNames)
  nShifter <- length(shifterNames)
  nObs <- nrow(data)
  lnp <- numeric(nObs)
  if (priceIndex %in% c("L", "P", "T")) {
    if (class(base) == "list") {
      if (is.null(base$prices)) {
        stop("if argument 'priceIndex' is '", priceIndex, 
             "'", " and argument 'base' is a list,", " this list must have an element 'prices'")
      }
      else if (length(base$prices) != nGoods) {
        stop("element 'prices' of argument 'base'", 
             " must have the same length as argument 'priceNames'")
      }
      else {
        basePrices <- base$prices
      }
    }
    else {
      basePrices <- rep(NA, nGoods)
      for (i in 1:nGoods) {
        basePrices[i] <- mean(data[[priceNames[i]]][base])
      }
    }
  }
  if (priceIndex %in% c("L", "Ls", "T")) {
    if (class(base) == "list") {
      if (is.null(base$shares)) {
        stop("if argument 'priceIndex' is '", priceIndex, 
             "'", " and argument 'base' is a list,", " this list must have an element 'shares'")
      }
      else if (length(base$shares) != nGoods) {
        stop("element 'shares' of argument 'base'", 
             " must have the same length as argument 'priceNames'")
      }
      else {
        if (all.equal(sum(base$shares), 1) != TRUE) {
          warning("the base expenditure shares specified", 
                  " by element 'shares' of argument 'base'", 
                  " do not sum up to 1 (deviation from 1 = ", 
                  formatC(sum(base$shares) - 1, digits = 3, 
                          format = "g"), ")")
        }
        baseShares <- base$shares
      }
    }
    else {
      baseShares <- rep(NA, nGoods)
      for (i in 1:nGoods) {
        baseShares[i] <- mean(data[[shareNames[i]]][base])
      }
    }
  }
  if (priceIndex == "S") {
    for (i in 1:nGoods) {
      lnp <- lnp + data[[shareNames[i]]] * log(data[[priceNames[i]]])
    }
  }
  else if (priceIndex == "SL") {
    lnp[1] <- NA
    for (i in 1:nGoods) {
      lnp[2:nObs] <- lnp[2:nObs] + data[[shareNames[i]]][1:(nObs - 
                                                              1)] * log(data[[priceNames[i]]][2:nObs])
    }
  }
  else if (priceIndex == "P") {
    for (i in 1:nGoods) {
      lnp <- lnp + data[[shareNames[i]]] * log(data[[priceNames[i]]]/basePrices[i])
    }
  }
  else if (priceIndex == "L") {
    for (i in 1:nGoods) {
      lnp <- lnp + baseShares[i] * log(data[[priceNames[i]]]/basePrices[i])
    }
  }
  else if (priceIndex == "Ls") {
    for (i in 1:nGoods) {
      lnp <- lnp + baseShares[i] * log(data[[priceNames[i]]])
    }
  }
  else if (priceIndex == "T") {
    for (i in 1:nGoods) {
      lnp <- lnp + c(0.5 * (data[[shareNames[i]]] + baseShares[i] * 
                              matrix(1, nrow = nObs)) * log(data[[priceNames[i]]]/basePrices[i]))
    }
  }
  else if (priceIndex == "TL") {
    lnp <- array(coef$alpha0, c(nObs))
    for (i in 1:nGoods) {
      lnp <- lnp + coef$alpha[i] * log(data[[priceNames[i]]])
      for (j in 1:nGoods) {
        lnp <- lnp + 0.5 * coef$gamma[i, j] * log(data[[priceNames[i]]]) * 
          log(data[[priceNames[j]]])
      }
      if (nShifter > 0) {
        for (j in 1:nShifter) {
          lnp <- lnp + coef$delta[i, j] * data[[shifterNames[j]]] * 
            log(data[[priceNames[i]]])
        }
      }
    }
  }
  else {
    stop("the argument 'priceIndex' (price index) must be either 'S',", 
         " 'SL', 'P', 'L', 'Ls', 'T' or 'TL'")
  }
  if (!is.null(row.names(data))) {
    names(lnp) <- row.names(data)
  }
  if (exists("basePrices")) {
    attributes(lnp)$basePrices <- basePrices
  }
  if (exists("baseShares")) {
    attributes(lnp)$baseShares <- baseShares
  }
  return(lnp)
}


customAidsEst <- function( priceNames, shareNames, totExpName,
                     data, method = "LA", priceIndex = "Ls", pxBase = 1,
                     hom = TRUE, sym = TRUE,
                     shifterNames = NULL, instNames = NULL,
                     estMethod = ifelse( is.null( instNames ), "SUR", "3SLS" ),
                     ILmaxiter = 50, ILtol = 1e-5, alpha0 = 0, restrict.regMat = FALSE, ... ) {
  
  if( length( priceNames ) != length( shareNames ) ) {
    stop( "arguments 'priceNames' and 'shareNames' must have the same length" )
  }
  nGoods <- length( priceNames )
  nShifter <- length( shifterNames )
  extractPx <- function( method ) {
    px <- substr( method, 4, nchar( method ) )
    return( px )
  }
  
  if( ! method %in% c( "LA", "IL", "MK" ) ) {
    if( nchar( method ) >= 4 && substr( method, 3, 3 ) == ":" &&
        substr( method, 1, 2 ) %in% c( "LA", "IL", "MK" ) ) {
      priceIndex <- extractPx( method )
      warning( "using price index specified in argument 'method',",
               " ignoring argument 'priceIndex'" )
      method <- substr( method, 1, 2 )
    } else {
      stop( "argument 'method' must be either",
            " 'LA' (for 'Linear Approximation') or",
            " 'IL' (for 'Iterated Linear Least Squares')" )
    }
  } 
  
  if( !( priceIndex %in% c( "S", "SL", "P", "L", "Ls", "T" ) ) ) {
    stop( "argument 'priceIndex' that specifies the price index must be either",
          " 'S' (Stone index), 'SL' (Stone index with lagges shares),",
          " 'P' (Paasche index), 'L' (Laspeyres index),",
          " 'Ls' (Laspeyres index, simplified), or",
          " 'T' (Tornqvist index)" )
  }
  
  if( sym && !hom ) {
    hom <- TRUE  # symmetry implies homogeneity
    warning( "symmetry implies homogeneity: imposing additionally homogeniety" )
  }
  allVarNames <- c( priceNames, shareNames, totExpName, instNames, shifterNames )
  if( sum( is.na( data[ , allVarNames ] ) ) > 0 ) {
    warning( "there are some NAs in the data,",
             " all observations (rows) with NAs are excluded from the analysis" )
    data <- data[ !is.na( rowSums( data[ , allVarNames ] ) ), ]
  }
  nObs   <- nrow( data )      # number of observations
  sample <- if( priceIndex == "SL") c( 2:nObs ) else c( 1:nObs )
  result <- list()
  result$call <- match.call()
  wMeans <- numeric( nGoods )  # mean expenditure shares
  pMeans <- numeric( nGoods )  # mean prices
  xtMean <- mean( data[[ totExpName ]][ sample ] )
  for( i in seq( nGoods ) ) {
    wMeans[ i ] <- mean( data[[ shareNames[ i ] ]][ sample ] )
    pMeans[ i ] <- mean( data[[ priceNames[ i ] ]][ sample ] )
  }
  # log of price index
  lnp  <- customAidsPx( priceIndex, priceNames, shareNames = shareNames, data = data,
                  base = pxBase )
  # prepare data.frame
  sysData <- data.frame( xt = data[[ totExpName ]],
                         lxtr = ( log( data[[ totExpName ]] ) - lnp ) )
  for( i in 1:nGoods ) {
    sysData[[ paste( "w", i, sep = "" ) ]] <- data[[ shareNames[ i ] ]]
    sysData[[ paste( "lp", i, sep = "" ) ]] <- log( data[[ priceNames[ i ] ]] )
  }
  if( is.null( instNames )) {
    ivFormula <- NULL
  } else {
    estMethod <- "3SLS"
    ivFormula <- "~"
    for( i in 1:length( instNames ) ) {
      sysData[[ paste( "i", i, sep = "" ) ]] <- data[[ instNames[ i ] ]]
      ivFormula <- paste( ivFormula, " + i", as.character( i ), sep = "" )
    }
    ivFormula <- as.formula( ivFormula )
  }
  if( is.null( shifterNames )) {
    shifterFormula <- NULL
  } else {
    for( i in 1:length( shifterNames ) ) {
      sysData[[ paste( "s", i, sep = "" ) ]] <- data[[ shifterNames[ i ] ]]
    }
  }
  restr <- customAidsRestr( nGoods = nGoods, hom = hom, sym = sym, restrict.regMat = restrict.regMat, nShifter = nShifter )
  # restrictions for homogeneity and symmetry
  system <- customAidsSystem( nGoods = nGoods, nShifter = nShifter ) # LA-AIDS equation system
  # estimate system
  if( restrict.regMat ) {
    est <- systemfit( system, estMethod, data = sysData, restrict.regMat = restr,
                      inst = ivFormula, ... )
  } else {
    est <- systemfit( system, estMethod, data = sysData, restrict.matrix = restr,
                      inst = ivFormula, ... )
  }
  if( method == "LA" ) {
    result$coef <- customAidsCoef( coef( est ), nGoods = nGoods, nShifter = nShifter,
                              cov = vcov( est ), priceNames = priceNames, shareNames = shareNames,
                              shifterNames = shifterNames, df = df.residual( est ) )   # coefficients
    result$wFitted <- customAidsCalc( priceNames, totExpName, data = data,
                                coef = result$coef, priceIndex = lnp )$shares   # estimated budget shares
    iter <- est$iter
  } else if( method %in% c( "MK", "IL" ) ) {
    b       <- coef( est )# coefficients
    bd      <- b          # difference of coefficients between
    # this and previous step
    iter    <- est$iter   # iterations of each SUR estimation
    ILiter <- 1          # iterations of IL Loop
    while( ( ( t( bd ) %*% bd ) / ( t( b ) %*% b ) )^0.5 > ILtol &&
           ILiter < ILmaxiter ) {
      ILiter <- ILiter + 1      # iterations of IL Loop
      bl     <- b              # coefficients of previous step
      sysData$lxtr <- log( data[[ totExpName ]] ) -
        customAidsPx( "TL", priceNames, shareNames = shareNames, data = data,
                coef = customAidsCoef( coef( est ), nGoods = nGoods, nShifter = nShifter,
                                  alpha0 = alpha0 ) )
      # real total expenditure using Translog price index
      if( restrict.regMat ) {
        est <- systemfit( system, estMethod, data = sysData, restrict.regMat = restr,
                          inst = ivFormula, ... )    # estimate system
      } else {
        est <- systemfit( system, estMethod, data = sysData, restrict.matrix = restr,
                          inst = ivFormula, ... )    # estimate system
      }
      iter <- c( iter, est$iter ) # iterations of each estimation
      weightNewCoef <- 1
      b    <- weightNewCoef * coef( est ) + ( 1 - weightNewCoef ) * b # coefficients
      bd   <- b - bl  # difference between coefficients from this
      # and previous step
    }
    # calculating log of "real" (deflated) total expenditure
    sysData$lxtr <- log( data[[ totExpName ]] ) -
      customAidsPx( "TL", priceNames, data = data,
              coef = customAidsCoef( coef( est ), nGoods = nGoods, nShifter = nShifter,
                                alpha0 = alpha0 ) )
    # calculating matrix G
    Gmat <- cbind( rep( 1, nObs ), sysData$lxtr )
    for( i in 1:( nGoods ) ) {
      Gmat <- cbind( Gmat, sysData[[ paste( "lp", i, sep = "" ) ]] )
    }
    if( nShifter > 0 ) {
      for( i in 1:nShifter ) {
        Gmat <- cbind( Gmat, sysData[[ paste( "s", i, sep = "" ) ]] )
      }
    }
    # testing matrix G
    if( FALSE ) {
      for( i in 1:( nGoods - 1 ) ) {
        print( fitted( est$eq[[ i ]] ) - Gmat %*%
                 coef( est )[ ( ( i - 1 ) * ( nGoods + 2 ) + 1 ):( i * (nGoods + 2 ) ) ] )
      }
    }
    # calculating matrix J
    jacobian <- customAidsJacobian( coef( est ), priceNames, totExpName, data = data,
                               shifterNames = shifterNames, alpha0 = alpha0 )
    if( hom ) {
      modRegMat <- customAidsRestr( nGoods = nGoods, nShifter = nShifter,
                               hom = hom, sym = sym, restrict.regMat = TRUE )
    } else {
      modRegMat <- diag( ( nGoods - 1 ) * ( nGoods + 2 + nShifter ) )
    }
    # Jmat <- t( modRegMat ) %*% ( diag( nGoods - 1 ) %x% t( Gmat ) ) %*% jacobian
    # JmatInv <- modRegMat %*% solve( Jmat ) %*% t( modRegMat )
    # bcov <- JmatInv  %*% ( est$residCov %x% ( t( Gmat ) %*% Gmat ) ) %*%
    #    t( JmatInv )
    Jmat <- crossprod( modRegMat, ( diag( nGoods - 1 ) %x% t( Gmat ) ) ) %*% jacobian
    JmatInv <- modRegMat %*% solve( Jmat, t( modRegMat ) )
    bcov <- JmatInv  %*% ( est$residCov %x% crossprod( Gmat ) ) %*%
      t( JmatInv )
    result$coef <- customAidsCoef( coef( est ), nGoods = nGoods, nShifter = nShifter,
                              cov = bcov, priceNames = priceNames, shareNames = shareNames,
                              shifterNames = shifterNames, df = df.residual( est ) )  # coefficients
    result$coef$alpha0 <- alpha0
    result$wFitted <- customAidsCalc( priceNames, totExpName, data = data,
                                coef = result$coef, priceIndex = "TL" )$shares
    # estimated budget shares
    result$ILiter <- ILiter
  }
  names( result$wFitted ) <- paste( "wFitted", as.character( 1:nGoods ),
                                    sep = "" )
  result$wResid <- data.frame( matrix( NA, nrow = nObs, ncol = nGoods ) )
  # residuals of shares
  names( result$wResid ) <- paste( "wResid", as.character( 1:nGoods ), sep = "" )
  result$qObs <- data.frame( matrix( NA, nrow = nObs, ncol = nGoods ) )
  # observed quantities
  names( result$qObs ) <- paste( "qObs", as.character( 1:nGoods ), sep = "" )
  result$qFitted <- data.frame( matrix( NA, nrow = nObs, ncol = nGoods ) )
  # observed quantities
  names( result$qFitted ) <- paste( "qFitted", as.character( 1:nGoods ),
                                    sep = "" )
  result$qResid <- data.frame( matrix( NA, nrow = nObs, ncol = nGoods ) )
  # observed quantities
  names( result$qResid ) <- paste( "qResid", as.character( 1:nGoods ), sep = "" )
  for( i in 1:nGoods ) {
    result$wResid[ , i ] <- data[[ shareNames[ i ] ]] - result$wFitted[ , i ]
    result$qObs[ , i ]   <- data[[ shareNames[ i ] ]] * data[[ totExpName ]] /
      data[[ priceNames[ i ] ]]
    result$qFitted[ , i ] <- result$wFitted[ i ] * data[[ totExpName ]] /
      data[[ priceNames[ i ] ]]
    result$qResid[ , i ] <- result$qObs[ , i ] - result$qFitted[ , i ]
  }
  result$r2 <- numeric( nGoods )
  for( i in 1:( nGoods - 1 ) ) {
    result$r2[ i ] <- summary( est$eq[[ i ]] )$r.squared
  }
  result$r2[ nGoods ] <- rSquared( data[ sample, shareNames[ nGoods ] ],
                                   result$wResid[ sample, nGoods ] )
  names( result$r2 ) <- shareNames
  result$r2q <- numeric( nGoods ) # R2 values for consumed quantities
  for( i in 1:nGoods ) {
    result$r2q[ i ] <- rSquared( result$qObs[ sample , i ],
                                 result$qResid[ sample, i ] )
  }
  names( result$r2q ) <- paste( "q_", shareNames, sep = "" )
  result$iter <- iter
  result$est <- est
  result$method <- method
  result$priceIndex <- priceIndex
  result$lnp <- lnp
  result$wMeans <- wMeans
  result$pMeans <- pMeans
  result$xtMean <- xtMean
  result$shareNames <- shareNames
  result$priceNames <- priceNames
  result$totExpName <- totExpName
  result$basePrices <- attributes( result$lnp )$basePrices
  result$baseShares <- attributes( result$lnp )$baseShares
  
  class( result ) <- "aidsEst"
  return( result )
}
