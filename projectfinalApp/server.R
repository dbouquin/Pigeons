
library(shiny)
library(ggplot2)
library(randomForest)
library(caret)
library(car)
library(ipred)
library(klaR)
library(corrplot)
library(RCurl)
library(ggthemes)
library(knitr)
library(RColorBrewer)
library(sqldf)

#wd.datapath = paste0(getwd(),"/data")
#wd.init = getwd()
#setwd(wd.datapath)


#options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))  

#download.file(url = "https://raw.githubusercontent.com/dbouquin/Pigeons/master/American_Racing_Pigeon_Union_Young_Bird_Data.csv",destfile = "/American_Racing_Pigeon_Union_Young_Bird_Data.csv", method="curl")
#y_birds <- read.csv("American_Racing_Pigeon_Union_Young_Bird_Data.csv", na.strings = "NA")

y_birds <- read.csv(file="./data/American_Racing_Pigeon_Union_Young_Bird_Data.csv", na.strings = "NA")

colnames(y_birds)[8] <- "COLOR"
colnames(y_birds)[9] <- "SEX"

fit <- aov(YPM ~ COLOR, data = y_birds)

y_birds$NDB.Std.Pts <- as.numeric(y_birds$NDB.Std.Pts)

# create a binary value to indicate high scoring bird (80+ points)
y_birds$highscore  <- with(y_birds, ifelse(y_birds$NDB.Std.Pts >= 80, 'high_score', y_birds$NDB.Std.Pts >= 80))
y_birds$highscore <- as.factor(y_birds$highscore)

y_birds$COLOR <- recode(y_birds$COLOR, "c('B', 'BB', 'BBAR', 'BBPD', 'BBSP', 'BBTC', 'BBWF')='black_bar'; c('BC', 'BCSP', 'BCPD', 'BCWF')='blue_check'; c('DC', 'DCSP', 'DCPD', 'DCWF')='dark_check'; c('RB', 'RBSP', 'RBPD', 'RBWF')='red_bar'; c('RC', 'RCSP', 'RCPD', 'RCWF')='red_check'; c('RED', 'RSPL')='red'; c('BSPL', 'DBB')='blue_var'; c('BLK')='black'; c('WHT')='white'; c('GRIZ')= 'grizzled'; c('IND')='indigo'; c('LAV')='lavender'; c('AND')='andalusian'; c('SLT')='slate'; c('BRWN', 'CHOC', 'BRNZ')='brown_var'; c('TIC')= 'ticked'; c('PEN', 'YLW')= 'yellow_var'; else='complex'")

set.seed(415)

# Use just a few variables to keep from overloading memory
m <-randomForest(highscore ~ COLOR + SEX + ORGANIZATION + YEAR, data=y_birds, mtry=2,importance=TRUE, do.trace=100)


birds2 <- read.csv(file="./data/birds2.csv", na.strings = "NA")



YB1_url <- read.csv(file="./data/American_Racing_Pigeon_Union_Young_Bird_Data.csv", header=TRUE, sep=",")
OB_url <- read.csv(file="./data/OldBirds.csv", header=TRUE, sep=",")
War_url <- read.csv(file="./data/Dickin_Medal_Pigeons.csv", header=TRUE, sep=",")

YB1_table <- data.frame(YB1_url[c(1, 5, 11, 13)])
YB1_table$TYPE <- "YOUNG BIRD (RACING PIGEONS)"
OB1_table <- data.frame(OB_url[c(2, 4, 12, 14)])
colnames(OB1_table)[1] <- "ORGANIZATION"
OB1_table$TYPE <- "OLD BIRD (RACING PIGEONS)"

Bird_table <- rbind.data.frame(YB1_table, OB1_table)
Bird_table$MILES <- as.numeric(format(Bird_table$MILES))

#mile_tb <- subset(Bird_table, MILES > 1)
#mile_tb_total <- mile_tb %>% group_by(TYPE, ORGANIZATION) %>% summarise(AVERAGE_MILES = mean(MILES)) %>% data.frame()
mile_tb_total<- sqldf("select TYPE, ORGANIZATION, AVG(MILES) as AVERAGE_MILES from Bird_table where MILES>1 group by TYPE, ORGANIZATION")

mile_tb_total$ORGANIZATION[mile_tb_total$ORGANIZATION == "Wichita Friendly"] <- "Wichita Friendly Pigeon Flyers"
mile_tb_total$ORGANIZATION[mile_tb_total$ORGANIZATION == "Dallas"] <- "Dallas Homing RPC" 
mile_tb_total$ORGANIZATION[mile_tb_total$ORGANIZATION == "Evergreen State"] <- "Evergreen State Concourse" 
mile_tb_total$ORGANIZATION[mile_tb_total$ORGANIZATION == "75 Combines"] <- "75 Combine"

#Medal_tb <- subset(War_url, Distance.mi. > 1)
#Medal_tb_total <- Medal_tb %>%  summarise(AVERAGE_MILES = mean(Distance_mi)) %>% data.frame()

Medal_tb_total<- sqldf("select AVG(Distance_mi) as AVERAGE_MILES from War_url where Distance_mi>1")
TYPE <- "WAR PIGEONS"
ORGANIZATION <- "Dickin Medal Winners"
vec1 <- data.frame(TYPE, ORGANIZATION, Medal_tb_total)

Final_Bird_table <- rbind.data.frame(mile_tb_total, vec1)
Final_Bird_table$AVERAGE_MILES <- round(Final_Bird_table$AVERAGE_MILES, 1)

tab<-kable(Final_Bird_table, align = "c", caption = "Average distance flown by racing pigeons today, compared to historical pigeons distance(medal winning).")

# Angus Analysis



# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
  #n<-datasetInput()
    if(input$Analysis == "1") {     
      bp <- ggplot(data=y_birds, aes(x=COLOR, y=YPM, fill=COLOR)) + geom_boxplot()
      bp + guides(fill=FALSE)
      
    }else if(input$Analysis=="2") {
      fit <- aov(YPM ~ COLOR, data = y_birds) # color must be datatype factor
      
      plot(fit)
    } else if(input$Analysis == "4") {
      colors <- c("turquoise4","seagreen3")
      M <- cor(birds2)
      corrplot.mixed(M, tl.pos="d", tl.col="black", tl.cex=.7, col=colors)
      
    } else if(input$Analysis == "6") {
      ggplot(data =  Final_Bird_table, aes(x=ORGANIZATION, y = AVERAGE_MILES, fill=TYPE)) + scale_colour_tableau()  + theme_solarized(light=FALSE) + theme(legend.position="top") + geom_bar(stat="identity", position = "dodge") + theme(axis.text.x  = element_text(angle=10, vjust=.9, hjust=.6)) + ggtitle(" Average distance flown by racing pigeons today, \ncompared to historical (medal winning) pigeons' distance.") + ylab("AVERAGE DISTANCE TRAVELED (MILES)")
      #kable(Final_Bird_table, align = "c", caption = "Average distance flown by racing pigeons today, compared to historical pigeons distance(medal winning).")
      
      
    } else if(input$PigF == "8") {
      #barplot(summary (oldbird$YPM))
      oldbird <- read.csv(file="./data/OldBirds.csv",header=TRUE , sep=",") 
      op <- par(mar=c(12,4,4,2))
      barplot((table(oldbird$Regions)), las =2, col = 'skyblue')
      
    }else if(input$PigF == "9") {
      
      youngbird <-  read.csv(file="./data/American_Racing_Pigeon_Union_Young_Bird_Data.csv", header=TRUE , sep=",")
      youngbird3 <- sqldf("select * from youngbird where POS=1 order by YPM DESC")
      ggplot(data = youngbird3, aes(x =NAME , y = YPM))+geom_point(aes(color=YPM, size =4))+scale_fill_brewer(palette = "Accent") +geom_text(aes(label = round(YPM)), hjust = -0.5, vjust = -0.5, size = 3)+ylab("Speed in Yard / Minutes") +xlab ("Owners ") + theme(axis.text.x  = element_text(angle=15, vjust=.6, hjust=.6,  size = 14))+ggtitle("FASTEST YOUNG PIGEONS")+expand_limits(y = 1580) + scale_y_continuous(expand = c(0,5)) 
      
    }else if(input$PigF == "10") {
      oldbird <- read.csv(file="./data/OldBirds.csv",header=TRUE , sep=",") 
      oldbird3<- sqldf("select * from oldbird where POS=1 order by YPM DESC") 
       ggplot(data = oldbird3, aes(x =NAME , y = YPM))+geom_point(aes(color=YPM, size =4))+scale_fill_brewer(palette = "Accent") +geom_text(aes(label = round(YPM)), hjust = 0.5, vjust = -0.5, size = 3)+ylab("Speed in Yard / Minutes") +xlab ("Owners") + theme(axis.text.x  = element_text(angle=15, vjust=.6, hjust=.6,  size = 14))+ggtitle("FASTEST OLD PIGEONS")
      
    }else if(input$PigF == "11") {
      fastanimal <- read.csv(file="./data/fast_animals.csv",header=TRUE , sep=",")
      fastanimal3<-sqldf("select * from fastanimal")
      ggplot(data = fastanimal3, aes(x = animal, y = max_speed_ym))+geom_point()+scale_fill_brewer(palette = "Accent")+geom_text(aes(label = round( max_speed_ym)), hjust = 0.5, vjust = -0.5, size = 3) +ylab("Speed in Yard / Minutes") + xlab (" ") + theme(axis.text.x  = element_text(angle=90, vjust=0.8, hjust=.8,  size = 14)) +ggtitle("FASTEST ANIMALS' SPEED")+geom_point(aes(color=animal, size =4))+expand_limits(y = 2000) + scale_y_continuous(expand = c(0,2000)) 
      
      
    }
    
  
    
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
   # if(input$Analysis == "0") {
    # print("No Summary Available")
    #} else if (input$PigF == "7") {
     # print("No Summary Available")
    #}
    #else 
      
      if(input$Analysis == "2") {
      summary(fit)
    } else if (input$Analysis == "3") {
      set.seed(415)
      
      # Use just a few variables to keep from overloading memory
      
      m <-randomForest(highscore ~ COLOR + SEX + ORGANIZATION + YEAR, data=y_birds, mtry=2,importance=TRUE, do.trace=100)
     
      
       print(m)
       
       print("Error found in Cross Validation")
       
       set.seed(131)
       error.m <-numeric(10)
       for(i in 1:10)error.m[i]<- errorest(highscore ~ COLOR + SEX + ORGANIZATION + YEAR,data=y_birds,model=randomForest,mtry=2)$error
       summary(error.m)
       
      
    }else if(input$Analysis == "5") {
      keeps2 <- c("COLOR", "SEX", "YPM", "NDB.Std.Pts", "YEAR", "highscore", "ORGANIZATION")
      birds3 <- y_birds[keeps2]
      
      set.seed(10)
      
      # tried with 2 clusters
      cl <- kmodes(birds3, 2)
      modes<-cl$modes
      clsize<- cl$size
      
      print(modes)
      print(clsize)
    }else if(input$Analysis == "6") {
      
      print(tab)
    }
    
    
  })
  
  
  
})

