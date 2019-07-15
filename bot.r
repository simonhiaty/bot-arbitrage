TradingStrat = function(prices, spreads, quantity=1000)
{
  isShort = FALSE
  isLong = FALSE
  isOpened = FALSE
  
  nb_profits = 0
  nb_losses = 0
  
  losses = 0
  profits = 0
  
  ## -1 if long, 1 is short, 0 is we do nothing
  ## Long if spread < 0 else short
  
  std_spread = sd(spreads)
  stopLoss = 2.5*std_spread
  openTrade = 1.5*std_spread
  
  allOperations = matrix(data=0,nrow=length(spreads))
  
  ##We loop on spreads
  for (i in seq(length(spreads))){
    
    ##Current spread
    spread = spreads[i]
    difference = 0
    
    ##If we have no position and taking a position is possible
    if (abs(spread) >= openTrade & isOpened == FALSE)
    {
      if (spread < 0)
      {
        isLong = TRUE
        allOperations[i] = -1
      }
      else
      {
        isShort = TRUE
        allOperations[i] = 1
      }
      
      actionPrice = allOperations[i]*prices[i]
      isOpened = TRUE
    }
    
    ##Else if we have a position
    else if (isOpened == TRUE)
    {
      if (isLong == TRUE)
      {
        if (spread <= -stopLoss | spread >= 0)
        {
          isLong = FALSE
          isOpened = FALSE
          allOperations[i] = 1
          
          difference = actionPrice + allOperations[i]*prices[i]
          if(difference < 0){losses = losses + abs(difference); nb_losses = nb_losses + 1}
          else{profits = profits + abs(difference); nb_profits = nb_profits + 1}
        }
      }
      else if (isShort == TRUE)
      {
        if (spread >= stopLoss | spread <= 0)
        {
          isShort = FALSE
          isOpened = FALSE
          allOperations[i] = -1
          
          difference = actionPrice + allOperations[i]*prices[i]
          if(difference < 0){losses = losses + abs(difference); nb_losses = nb_losses + 1}
          else{profits = profits + abs(difference); nb_profits = nb_profits + 1}
        }
      }
    }
  }
  
  ##If something is opened at the end
  if (isOpened)
  {
    if (allOperations[length(spreads)] != 0)
    {
      allOperations[length(spreads)] = 0
    }
    else
    {
      allOperations[length(spreads)] = -sum(allOperations)
      difference = actionPrice + allOperations[length(spreads)]*prices[length(spreads)]
      if(difference < 0){losses = losses + abs(difference); nb_losses = nb_losses + 1}
      else{profits = profits + abs(difference); nb_profits = nb_profits + 1}
    }
  }
  
  total_profits = profits*quantity - losses*quantity
  another = sum(allOperations * prices)
  
  return(list(operations=allOperations, 
              total=total_profits,
              profits=profits*quantity, 
              losses=losses*quantity, 
              PL = (profits/nb_profits)/(losses/nb_losses), 
              nb_profits=nb_profits, 
              nb_losses=nb_losses))
  
}
