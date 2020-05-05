ConfidenceInterval <- function(x,n,confidence){
  #x is number of disconnections observed
  #n is the number of inverters in the sample
  #confidence is the level of desired confidence (eg 0.95)
  
  ### While loop to narrow in on the value of the true number of disconnections that would 
  #lead to our observation of x or below to be less than half our margin of confidence error.
  
  
  # This determines how accurate we want our confidence interval to be.
  # Currently it is set to a tenth of a percent
  
  ErrorMargin <- 0.001
  
  #Start our first gues as the centre of the confidence interval
  
  CurrentGuessLeft <- x/n
  CurrentGuessRight <- x/n
  DoneFlagLeft = 0
  DoneFlagRight = 0
  
  while(DoneFlagLeft != 1 || DoneFlagRight != 1){
    
    # qbinom(p,size,prob) returns the largest number of disconnections that
    # has a less than p chance of occuring in a sample size of 'size' with a true proportion
    # of disconnections equal to 'prob'. Here we're essentially asking, with our given confidence
    # level and sample size, is our observed number of disconnections outside our range of
    # confidence under our current guess of the true number of disconnections? Once we reach a guess
    # for which this answer is yes, we will stop and say that that is the point at which our evidence
    # gives us confidence that the true number of disconnections cannot be any less than the guess
    # we end up with.
    
    # Same process for left and right ends of the interval they look at opposite ends
    # and search in opposite directions
    
    if(qbinom(confidence + (1-confidence)/2,n,CurrentGuessLeft) >= x && CurrentGuessLeft!=0)
    {
      CurrentGuessLeft <- max(0,CurrentGuessLeft - ErrorMargin)
    }
    else
    {
      DoneFlagLeft <- 1
    }
    
    if(qbinom((1-confidence)/2,n,CurrentGuessRight) <= x)
    {
      CurrentGuessRight <- CurrentGuessRight + ErrorMargin
    }
    else
    {
      DoneFlagRight <- 1
    }
    
  }
  
  return(c(CurrentGuessLeft,CurrentGuessRight))
}