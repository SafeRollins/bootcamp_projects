# Chatbot (rule-based)
# Ordering Pizza

pizza <- function(){
  catalog <- list(
    list("Hawaiian","M","Pan Medium",359),
    list("Hawaiian","M","Cris Thin Medium",319),
    list("Hawaiian","M","Extream Cheese Medium",459),
    list("Hawaiian","L","Pan Large",479),
    list("Hawaiian","L","Cris Thin Large",439),
    list("Hawaiian","L","Extream Cheese Large",629),
    list("Sea Food Cocktail","M","Pan Medium",419),
    list("Sea Food Cocktail","M","Cris Thin Medium",379),
    list("Sea Food Cocktail","M","Extream Cheese Medium",519),
    list("Sea Food Cocktail","L","Pan Large",539),
    list("Sea Food Cocktail","L","Cris Thin Large",499),
    list("Sea Food Cocktail","L","Extream Cheese Large",669),
    list("Spicy Super Seafood","M","Pan Medium",419),
    list("Spicy Super Seafood","M","Cris Thin Medium",379),
    list("Spicy Super Seafood","M","Extream Cheese Medium",519),
    list("Spicy Super Seafood","L","Pan Large",539),
    list("Spicy Super Seafood","L","Cris Thin Large",499),
    list("Spicy Super Seafood","L","Extream Cheese Large",669),
    list("Seafood Deluxe","M","Pan Medium",419),
    list("Seafood Deluxe","M","Cris Thin Medium",379),
    list("Seafood Deluxe","M","Extream Cheese Medium",519),
    list("Seafood Deluxe","L","Pan Large",539),
    list("Seafood Deluxe","L","Cris Thin Large",499),
    list("Seafood Deluxe","L","Extream Cheese Large",669),
    list("Tom Yum Kung","M","Pan Medium",419),
    list("Tom Yum Kung","M","Cris Thin Medium",379),
    list("Tom Yum Kung","M","Extream Cheese Medium",519),
    list("Tom Yum Kung","L","Pan Large",539),
    list("Tom Yum Kung","L","Cris Thin Large",499),
    list("Tom Yum Kung","L","Extream Cheese Large",669),
    list("Meat Deluxe","M","Pan Medium",359),
    list("Meat Deluxe","M","Cris Thin Medium",319),
    list("Meat Deluxe","M","Extream Cheese Medium",459),
    list("Meat Deluxe","L","Pan Large",479),
    list("Meat Deluxe","L","Cris Thin Large",439),
    list("Meat Deluxe","L","Extream Cheese Large",629),
    list("Super Deluxe","M","Pan Medium",359),
    list("Super Deluxe","M","Cris Thin Medium",319),
    list("Super Deluxe","M","Extream Cheese Medium",459),
    list("Super Deluxe","L","Pan Large",479),
    list("Super Deluxe","L","Cris Thin Large",439),
    list("Super Deluxe","L","Extream Cheese Large",629)
  )
  df <- do.call(rbind,catalog)
  df <- as.data.frame(df)
  colnames(df) <- c("Name", "Size", "Crust", "Price")
  
  name<-"Hawaiian"
  size<-"L"
  crust<-"Cris Thin Medium"
  
  #test=subset(df,(Name==name & Size==size),select=c(Crust))
  #print (test[[1]][1])
  #print (test[[1]][2])
  #print (test[[1]][3])
  
  
  print ("Welcome to our app!")
  print ("Hello There! ")
  print ("What's your name?: ")
  user_name <- readLines("stdin",n=1)   # One line #Stdin= Standard Input
  print (paste("Hi ",user_name))

  ##Emthy list
  your_bucket=list()
  err=0
  while(TRUE){
      ##Select Pizza
    print ("Please select your pizza by enter number?: ")
    get_pizza <- unique(df$Name)
    i<-1
    for (x in get_pizza){
      print (paste(i,x))
      i<-i+1
    } 
    select_pizza <- readLines("stdin",n=1)
    select_pizza <- as.numeric(select_pizza)
    
    if (select_pizza>i-1 || select_pizza==0 ){
      print ("Please select again")
      err=1
      break
    }
    
    ##Select Size
    print ("Please select size (Enter M or L): ")
    select_size<-toupper(readLines("stdin",n=1))
    if (select_size=="M" || select_size=="L"){
      invisible()
    }else{
      print ("Please select again")
      err=1
      break
    }
    #print (paste("You choose",get_pizza[[select_pizza]],"Size",select_size))
    
    ##Select Burst
    print ("Please select crust by enter number: ")
    get_curst <-subset(df,(Name==get_pizza[[select_pizza]] & Size==select_size),select=c(Crust))
    j<-1
    for (y in get_curst[[1]]){
      print (paste(j,y))
      j<-j+1
    }
    select_curst <- readLines("stdin",n=1)
    select_curst <- as.numeric(select_curst)
    if (select_curst>j-1 || select_curst==0 ){
      print ("Please select again")
      err=1
      break
    }
    #print (paste("You choose",get_pizza[[select_pizza]],"Size",select_size,"Curst",get_curst[[1]][[select_curst]])) 
  
    #Get price per 1 unit
    lookup_price <- subset(df,(Name==get_pizza[[select_pizza]] & Size==select_size & Crust==get_curst[[1]][[select_curst]]),select=c(Price))
    get_price=lookup_price[[1]][[1]]
  
    ##Select quantity 
    print ("Please select quantity: ")
    select_quantity <- readLines("stdin",n=1)
    select_quantity <- as.numeric(select_quantity)
  
    ##Update bucket
    your_bucket[[length(your_bucket)+1]] <- list(get_pizza[[select_pizza]],
                         select_size,
                         get_curst[[1]][[select_curst]],
                         get_price,
                         select_quantity)

    ##Anything else
    print ("Anything Else(Y/N): ")
    ans <- toupper(readLines("stdin",n=1))
    if (ans=="Y"){
      print ("")
    }else if(ans=="N"){
      break
    }else{
      break   ## Any text assume to N
    }
  }

  if (err==0){
    df_bucket <- do.call(rbind,your_bucket)
    df_bucket <- as.data.frame(df_bucket)
    colnames(df_bucket) <- c("Name", "Size", "Crust", "Price_per_Unit","Quantity")
    df_bucket$sum <- as.numeric(df_bucket$Price_per_Unit) * as.numeric(df_bucket$Quantity)
    #print (df_bucket)
    pay_m <- c("Cash","Credit Card","Linepay","Shopeepay","True wallet")
    print ("Please select payment method: ")
    i=1
    for (pay in pay_m){
      print(paste(i,pay))
      i<-i+1
    }
    while (TRUE){

      ans_pay <- toupper(readLines("stdin",n=1))
      ans_pay <- as.numeric(ans_pay)
      if (ans_pay>i-1){
        print ("Please select payment method again:")
      }else{
        break
      }
    }
    print ("Summary")
    print (df_bucket)
    print (paste("Payment:",pay_m[ans_pay]))
    print ("Do you want to confirm your order? (Y=to comfirm, Any key=Cancel) ")
    ans_c<-toupper(readLines("stdin",n=1))
    if (ans_c=="Y"){
      print ("THANKS FOR YOUR ORDER")
      print (paste("The Net total is ",sum(df_bucket$sum),"Bath."))
    }else{
      print ("We hope we can be of service to you in the future.")
    }
  }                   
}
pizza()
