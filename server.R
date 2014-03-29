library(gbm)
library(rpart)
library(rpart.plot)
library(FNN)


shinyServer(function(input, output) {
  output$playerinfo <- renderTable({
       
    players.data <- read.csv(file='players.csv', sep=',', h=T)
    player.data<-subset(players.data, PlayerName == input$player & CategoryName == input$category)
    
    output.data <- player.data [c("PlayerName", "CategoryName", "AuctionCount", "AuctionSaleCount","AuctionAvgPrice","AuctionMedianPrice","MaxPrice","MinPrice")]
    output.data
    
    })
  
  output$sellprob <- renderPrint({
    
    if (is.null(input$seller))
     return(NULL)
    
    ###########################################################
    ## load reference data
    ###########################################################    
    players.data <- read.csv(file='players.csv', sep=',', h=T)
    player.data<-subset(players.data, PlayerName == input$player & CategoryName == input$category)

    sellers.data <- read.csv(file='sellers.csv', sep=',', h=T)
    seller.data<-subset(sellers.data, SellerName == input$seller)
    
    ###########################################################
    ## build test set data frame
    ###########################################################
    N <- 1
    test.data <- data.frame(SellerClosePercent = numeric(N),
               StartingBidPercent = numeric(N),
               ItemAuctionSellPercent = numeric(N),
               AuctionSaleCount = numeric(N),
               AuctionMedianPrice = numeric(N))

    SellerClosePercent <- seller.data$SellerClosePercent*100
    StartingBidPercent <- (input$minbid/player.data$AvgPrice)*100
    ItemAuctionSellPercent <- player.data$ItemAuctionSellPercent*100
    AuctionSaleCount <- player.data$AuctionSaleCount
    AuctionMedianPrice <- player.data$AuctionMedianPrice
    
    test.data[1, ] <- c(SellerClosePercent, StartingBidPercent,  ItemAuctionSellPercent, AuctionSaleCount, AuctionMedianPrice)
    
    ###########################################################
    ## load training data
    ###########################################################
    
    train.raw.data  <- read.csv(file='C:\\Users\\jay\\Desktop\\DataScienceClass\\JayFinalProject\\code\\data\\TrainingSet.csv', sep=',', h=T)
    train.data.success <- train.raw.data [c("QuantitySold","AuctionMedianPrice", "Price","PricePercent","StartingBidPercent","SellerClosePercent","StartingBid","AvgPrice","HitCount","AuctionAvgHitCount","ItemAuctionSellPercent","SellerSaleAvgPriceRatio","AuctionHitCountAvgRatio","BestOffer", "IsHOF","ItemListedCount","AuctionCount","AuctionSaleCount","SellerAuctionCount","SellerAuctionSaleCount")]
    # standardize training data points between 1 and 100
    train.data.success$SellerClosePercent <-train.data.success$SellerClosePercent*100
    train.data.success$StartingBidPercent <-train.data.success$StartingBidPercent*100
    train.data.success$ItemAuctionSellPercent <-train.data.success$ItemAuctionSellPercent*100

    model.data<-train.data.success [c("QuantitySold","AuctionMedianPrice", "AuctionSaleCount","StartingBidPercent","SellerClosePercent","ItemAuctionSellPercent")]
    
    # perform LR model
    glm.out = glm(QuantitySold ~ AuctionSaleCount+AuctionMedianPrice+StartingBidPercent+SellerClosePercent+ItemAuctionSellPercent, family=binomial(logit), data=model.data)
    #summary(glm.out)
    
    # make predictions
    lm.pred<-predict(glm.out, test.data, type="response")
    
    # add fields to test.data
    paste("The probability this item will result in a sale is ", toString(round(lm.pred[1]*100,digits=2)),"%")
    
  })
  
  output$price <- renderPrint({
  
    
    ###########################################################
    ## load reference data
    ###########################################################    
    players.data <- read.csv(file='players.csv', sep=',', h=T)
    player.data<-subset(players.data, PlayerName == input$player & CategoryName == input$category)
    
    sellers.data <- read.csv(file='sellers.csv', sep=',', h=T)
    seller.data<-subset(sellers.data, SellerName == input$seller)
    
    ###########################################################
    ## build test set data frame
    ###########################################################
       
    N <- 1
    test.data <- data.frame(AuctionMedianPrice = numeric(N),
                            AvgPrice = numeric(N),
                            ItemAuctionSellPercent = numeric(N),
                            StartingBidPercent = numeric(N),
                            StartingBid = numeric(N),
                            Authenticated = numeric(N),
                            SellerSaleAvgPriceRatio = numeric(N),
                            IsHOF = numeric(N),
                            AuctionCount = numeric(N),
                            SellerAuctionSaleCount = numeric(N))
    
    AuctionMedianPrice <- player.data$AuctionMedianPrice
    AvgPrice <- player.data$AvgPrice
    ItemAuctionSellPercent <- player.data$AuctionSaleCount/player.data$AuctionCount
    StartingBid <- input$minbid
    StartingBidPercent <- input$minbid/player.data$AvgPrice
    Authenticated <- 0
    if (input$auth == TRUE)
        Authenticated <- 1
    SellerSaleAvgPriceRatio <- seller.data$SellerSaleAvgPriceRatio
    IsHOF <- 1
    AuctionCount <- player.data$AuctionCount
    SellerAuctionSaleCount <- seller.data$AuctionSaleCount
    
    test.data[1, ] <- c(AuctionMedianPrice, AvgPrice, ItemAuctionSellPercent, StartingBidPercent, StartingBid, Authenticated, SellerSaleAvgPriceRatio, IsHOF, AuctionCount, SellerAuctionSaleCount)
    ###########################################################
    ## load training data
    ###########################################################
    
    train.raw.data  <- read.csv(file='C:\\Users\\jay\\Desktop\\DataScienceClass\\JayFinalProject\\code\\data\\TrainingSubset.csv', sep=',', h=T)
    # optimized set of features
    train.data <- train.raw.data[c("AuctionMedianPrice", "Price", "AvgPrice", "ItemAuctionSellPercent", "StartingBidPercent", "StartingBid", "Authenticated", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")]
    train.data$Price=log(train.data$Price)
    
    
    ########################################################
    # BUILD CART Prediction
    ########################################################
    
    # make predictions
    tree<-rpart(Price ~.,data=train.data)
    
    predCart<-predict(tree, test.data, type = "vector")
    
    prediction = exp(predCart)
    
    
    #########################################################################
    # perform KNN prediction on PriceBucket for non AvgPrice features
    #########################################################################
    
    trainKnn.data  <- train.raw.data[c("PriceBuckets", "Price", "AuctionMedianPrice", "ItemAuctionSellPercent",  "StartingBid", "Authenticated", "SellerSaleAvgPriceRatio", "IsHOF", "AuctionCount", "SellerAuctionSaleCount")]
    cl <- factor(trainKnn.data$PriceBuckets)
    predKnn<-knn(trainKnn.data, test.data, cl, k = 3, prob=TRUE)
    
    PriceBucket <- as.numeric(as.character(predKnn))
    Confidence <- "High"
    
    if(prediction<input$minbid) {
      Confidence <- "Low"      
    } else if(abs(prediction-PriceBucket)<5) {
      Confidence <- "High"
    } else if(abs(prediction-PriceBucket)<10) {
      Confidence <- "Medium"
    } else {
      Confidence <- "Low"      
    }
    
    paste("Predicted Sale Price : $", toString(round(prediction,digits=2)), ", Predicted PriceBucket : $", toString(PriceBucket), "- $", toString(PriceBucket+5),", Confidence Level : ", Confidence)
   
  })
  
})