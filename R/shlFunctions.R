

#' Looks into the DB for what happened that day
#'
#' @param today todays date
#' @export
#'

teamResult <- function(today, teamID){
  ### Todays Date and teamID
  teamQuery <- paste0("SELECT * FROM Team_Events WHERE strftime('%m-%d',date)=  '",today,"' and teamID = ",teamID," ")
  getData <- dbGetQuery(con, teamQuery )
  max <- nrow(getData)
  if (max == 0 ) {
    return()
  } else {
    for (i in 1:max){
      print(getData$Event[i])
      # Check to see if its a round lost
      if (grepl("Round Lost",getData$Event[i], fixed = TRUE) ) {
        send_webhook_message(paste0("On ", getData$date[i], " (S", getData$season[i],") ", getData$teamName[i], " Had a ", getData$Event[i], " to ", getData$Extra[i] ))
      } else {
        result <- switch(getData$Event[i],
                         "GM" = paste0("On ", getData$date[i],  " (S", getData$season[i],") ", getData$userName[i], " became the GM of ", getData$teamName[i]),
                         "Expansion" = paste0("On ", getData$date[i], " (S", getData$season[i],") ", getData$teamName[i] , " became the the expansion team with ", getData$userName[i], "being the GM" ),
                         "Move" = paste0("On ", getData$date[i], " (S", getData$season[i],") ", getData$userName[i], " moved the team to ",  getData$teamName[i] ),
                         "HOF" = paste0("On ", getData$date[i], " (S", getData$season[i],") ", getData$userName[i], " got into the SHL HOF"),
                         "Cup Win" = paste0("On ", getData$date[i], " (S", getData$season[i],") ", getData$teamName[i], " won the Challenge Cup" ),
                         "Finals Lost"  = paste0("On ", getData$date[i], " (S", getData$season[i],") ", getData$teamName[i], " Lost in the Challenge Cup Finals" ),
                         "Awards" = paste0("On ", getData$date[i], " (S", getData$season[i],") ", getData$userName[i], " won the ", getData$Extra[i]),
                         "Team Award" = paste0("On ", getData$date[i], " (S", getData$season[i],") ", getData$teamName[i], " won the ", getData$Extra[i]),
                         "Team HOF" = paste0("On ", getData$date[i], " (S", getData$season[i],") ", getData$Extra[i], " (", getData$userName[i], ") Entered the ", getData$teamName, " Hall of Fame"),
                         "CO-GM" = paste0("On ", getData$date[i],  " (S", getData$season[i],") ", getData$userName[i], " became the CO-GM of ", getData$teamName[i]),
                         "Regular Season" = Lst,
        )
        print(result)
        send_webhook_message(result)

      }

    }
  }
  return(result)
}


#' Looks into the DB for what happened that day for trades
#'
#' @param today todays date
#' @export
#'
tradeResult <- function(today, teamID){
  ### Todays Date
  tradeQuery <- paste0("SELECT * FROM Trades WHERE strftime('%m-%d',date)=  '",today,"'")
  getTradeData <- dbGetQuery(con, tradeQuery )
  max<-nrow(getTradeData)
  if(max == 0){
    return()
  } else {
    for (i in 1:max){
      print(i)
      if(is_empty(getTradeData$Team3Recieves[i]) == TRUE){
        tradeResult <- paste0("On ", getTradeData$Date[i], "(S", getTradeData$Season[i] ,") \n", getTradeData$Team1[i], "(",
                              getTradeData$GM1[i],") Recieves: ", getTradeData$Team1Recieves[i], "\n",
                              getTradeData$Team2[i], "(", getTradeData$GM2[i],") Recieves: ", getTradeData$Team2Recieves[i], "\n and",
                              getTradeData$Team3[i], "(", getTradeData$GM3[i],") Recieves: ", getTradeData$Team3Recieves[i],
                              "<",getTradeData$Link[i], ">"
        )


      } else {
        tradeResult <- paste0("On ", getTradeData$Date[i], "(S", getTradeData$Season[i] ,") \n", getTradeData$Team1[i], "(",
                              getTradeData$GM1[i],") Recieves: ", getTradeData$Team1Recieves[i], "\n",
                              getTradeData$Team2[i], "(", getTradeData$GM2[i],") Recieves: ", getTradeData$Team2Recieves[i], "\n",
                              "<",getTradeData$Link[i], ">"
        )
      }
      send_webhook_message(tradeResult)
    }
  }
  return(tradeResult)
}

#' Looks into the DB for what happened that day for draft
#'
#' @param today todays date
#' @export
#'
draftResult <- function(today, teamID){
  ### Todays Date
  draftQuery <- paste0("SELECT * FROM Drafts WHERE strftime('%m-%d',date)=  '",today,"' and teamID = ",teamID," ")
  getDraftData <- dbGetQuery(con, draftQuery )
  max<-nrow(getDraftData)
  if(max == 0){
    return()
  } else {
    send_webhook_message(paste0("On " ,getDraftData$Date[1], " ", getDraftData$Team[1], " Drafted" ) )
    for (i in 1:max){
      draftResult <- paste0("S", getDraftData$Season[i], " Pick #", getDraftData$DraftedNumber[i], ": ", getDraftData$Player[i])
      send_webhook_message(ifelse(is_empty(draftResult) == FALSE,draftResult))
    }

  }
  return(result)

}

#' Looks into the DB for what trades happened that day
#'
#' @param today todays date
#' @export
#' @export
#'
tradeResultAll <- function(today, teamName){
  ### Todays Date
  tradeQuery <- paste0("SELECT * FROM TradesAll WHERE strftime('%m-%d',date)=  '",today,"' and  ( team1 = '",teamName,"' or team2 = '",teamName,"' or team3 = '",teamName,"' ) ")
  getTradeData <- dbGetQuery(con, tradeQuery )
  max<-nrow(getTradeData)
  print(max)
  if(max == 0){
    return()
  } else {
    for (i in 1:max){
      print(i)
      if(is_empty(getTradeData$Team3Recieves[i]) == TRUE){
        tradeResult <- paste0("On ", getTradeData$Date[i], "(", getTradeData$Season[i] ,") \n", getTradeData$Team1[i], "(",
                              getTradeData$GM1[i],") Recieves: ", getTradeData$Team1Recieves[i], "\n",
                              getTradeData$Team2[i], "(", getTradeData$GM2[i],") Recieves: ", getTradeData$Team2Recieves[i], "\n and",
                              getTradeData$Team3[i], "(", getTradeData$GM3[i],") Recieves: ", getTradeData$Team3Recieves[i],
                              "<",getTradeData$Link[i], ">"
        )


      } else {
        tradeResult <- paste0("On ", getTradeData$Date[i], "(", getTradeData$Season[i] ,") \n", getTradeData$Team1[i], "(",
                              getTradeData$GM1[i],") Recieves: ", getTradeData$Team1Recieves[i], "\n",
                              getTradeData$Team2[i], "(", getTradeData$GM2[i],") Recieves: ", getTradeData$Team2Recieves[i], "\n",
                              "<",getTradeData$Link[i], ">"
        )
      }
      send_webhook_message(tradeResult)
      print(tradeResult)
    }
  }
  return(result)
}
