playgame <- function(){
  print ("Welcome to Rock Paper Scissors Games")
  print ("Play Game? (Y/N)? ")
  ans <- toupper(readLines("stdin",n=1))
  all <- c('Rock','Paper','Scissors')
  #print (sample(all,size=1))
  if (ans=="Y"){
    ##Start Game
    winloss=c()
    c<-1
    print ("If you want stop, Please type X")
    while(TRUE){
      print (paste("Round",c,"Please type R/P/S for Rock/Paper/Scossors (Select one) "))
      user <- toupper(readLines("stdin",n=1))
      pc<-sample(all,size=1)
      if (user=="R"){
        if ( pc=="Rock" ){
          winloss[length(winloss)+1] <- "Draw"
          print ("Draw")
        }else if( pc=="Paper" ){
          winloss[length(winloss)+1] <- "Loss"
          print ("Loss")
        }else if( pc=="Scissors" ){
          winloss[length(winloss)+1] <- "Win"
          print ("Win")
        }
        c<-c+1
      }else if(user=="P"){
        if ( pc=="Rock" ){
          winloss[length(winloss)+1] <- "Win"
          print ("Win")
        }else if( pc=="Paper" ){
          winloss[length(winloss)+1] <- "Draw"
          print ("Draw")
        }else if( pc=="Scissors" ){
          winloss[length(winloss)+1] <- "Loss"
          print ("Loss")
        }
        c<-c+1
      }else if(user=="S"){
        if ( pc=="Rock" ){
          winloss[length(winloss)+1] <- "Loss"
          print ("Loss")
        }else if( pc=="Paper" ){
          winloss[length(winloss)+1] <- "Win"
          print ("Win")
        }else if( pc=="Scissors" ){
          winloss[length(winloss)+1] <- "Draw"
          print ("Draw")
        }
        c<-c+1
      }else if(user=="X"){
        break
      }else{
        print ("Please type only R/P/S")
      }
        
    }
    print (paste("Summary round:",length(winloss)))
    stat <- table(winloss)
    print (stat)
    
  }else{
    print ("We hope you will come back to play game again")
  }
}
playgame()
