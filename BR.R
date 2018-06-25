setwd("C:\\academics\\stat\\stat")
library(rJava)
library(xlsx)
library(varhandle)
library(dplyr)
#png("BR_plot1.png")
count1=0
count2=0
count3=0
count4=0
count5=0
p.ad1<-c()
p.ad<-c()
star=0
BR_plot<-function(t){ #function starts
  cd_phase1<-c()
  cd_phase2<-c()
  cd_phase3<-c()
  cd_phase4<-c()
  cd_phase5<-c()
  folder<-paste("./*/*",t,sep="")
  print(folder)
  folder_session<-Sys.glob(folder)
  for(f in folder_session){ #for loop for all the CD, ED, MD
    count1=count1+1
    nd<-substr(f,0,7)
    nd<-paste(nd,"*ND/*.BR",sep="")
    find_nd<-Sys.glob(nd)#nd BR file
    temp<-paste(f,"/*.BR",sep="")
    find_BR<-Sys.glob(temp)#cd BR file
    temp<-paste(f,"/*.stm",sep="")
    find_stm<-Sys.glob(temp)#cd stm file
    if(length(find_BR)!=0 & length(find_stm)!=0)       # if case for BR and stm values not zero
    {
      count2=count2+1
      cols<-c(1,2)
      stmdata <- read.xlsx2(find_stm,sheetIndex=1,startRow = 9,endRow = 11,colIndex = cols)
      br<-unfactor(stmdata)
      if(nrow(stmdata)==2 & ncol(stmdata)==2 )       # if case for stmdata exactly 2*2
      {
        if(br$StartTime[1]>=0 & br$StartTime[2]>=0 & br$EndTime[1]>=0 & br$EndTime[2]>=0)
        {
          # print (sum(is.na(br$StartTime)))
          #print(sum(is.na(br$EndTime)))

        count3=count3+1
        CD_BR_data<-read.xlsx2(find_BR,sheetIndex=1,startRow = 9)# read BR file in CD
        CD_BR<-unfactor(CD_BR_data[,3])
        CD_BR_data1<-filter(CD_BR_data, CD_BR<4 | CD_BR>70)
        CD_BR_time<-CD_BR_data[,2]#timeframe values
        CD_BR_time<-unfactor(CD_BR_time)
        #CD_max<-max(CD_BR_time,na.rm=T)
        # #ND BR dataset
        ND_BR_data<-read.xlsx2(find_nd,sheetIndex = 1,startRow = 9)# read BR file in ND
        ND_BR<-unfactor(ND_BR_data[,3])
        ND_BR_data1<-filter(ND_BR_data, ND_BR<4 | ND_BR>70)
        ND_BR_time<-ND_BR_data[,2]#timeframe values
        ND_BR_time<-unfactor(ND_BR_time)
        #ND_max<-max(ND_BR_time)
        if(nrow(CD_BR_data1)==0 & nrow(ND_BR_data1)==0){
          count4=count4+1 # number of BR files
        # #mean of CD phase1
        CD_BR_frame<-subset(CD_BR_data,CD_BR_time >=0 & CD_BR_time < br$StartTime[1])
        ND_BR_frame<-subset(ND_BR_data,ND_BR_time >=0 & ND_BR_time < br$StartTime[1])

        if(nrow(CD_BR_frame)>0 & nrow(ND_BR_frame)>0){
            CD_BR<-unfactor(CD_BR_frame[,3])
            ND_BR<-unfactor(ND_BR_frame[,3])
            cd_m1<-mean(CD_BR,na.rm = T)
            nd_m1<-mean(ND_BR,na.rm = T)
            mean_1<-(cd_m1-nd_m1)
            cd_phase1<-c(cd_phase1,mean_1)
            #print(cd_phase1)
            
        }
        
        # #mean of CD phase2
        CD_BR_frame2<-subset(CD_BR_data,CD_BR_time >=br$StartTime[1] & CD_BR_time < br$EndTime[1])
        ND_BR_frame2<-subset(ND_BR_data,ND_BR_time >=br$StartTime[1] & ND_BR_time < br$EndTime[1])

        if(nrow(CD_BR_frame2)>0 & nrow(ND_BR_frame2)>0){
          
          CD_BR2<-unfactor(CD_BR_frame2[,3])
          ND_BR2<-unfactor(ND_BR_frame2[,3])
          cd_m2<-mean(CD_BR2,na.rm = T)
          nd_m2<-mean(ND_BR2,na.rm = T)
          mean_2<-(cd_m2-nd_m2)
          cd_phase2<-c(cd_phase2,mean_2)
          
        }
      

        CD_BR_frame3<-subset(CD_BR_data,CD_BR_time >=br$EndTime[1] & CD_BR_time < br$StartTime[2])
        ND_BR_frame3<-subset(ND_BR_data,ND_BR_time >=br$EndTime[1] & ND_BR_time < br$StartTime[2])
        if(nrow(CD_BR_frame3)>0 & nrow(ND_BR_frame3)>0){

          CD_BR3<-unfactor(CD_BR_frame3[,3])
          ND_BR3<-unfactor(ND_BR_frame3[,3])
          cd_m3<-mean(CD_BR3,na.rm = T)
          nd_m3<-mean(ND_BR3,na.rm = T)
          mean_3<-(cd_m3-nd_m3)
          cd_phase3<-c(cd_phase3,mean_3)
        }

         CD_BR_frame4<-subset(CD_BR_data,CD_BR_time >br$StartTime[2] & CD_BR_time < br$EndTime[2])
         ND_BR_frame4<-subset(ND_BR_data,ND_BR_time >br$StartTime[2] & ND_BR_time < br$EndTime[2])
         if(nrow(CD_BR_frame4)>0 & nrow(ND_BR_frame4)>0){
        
           CD_BR4<-unfactor(CD_BR_frame4[,3])
           ND_BR4<-unfactor(ND_BR_frame4[,3])
           cd_m4<-mean(CD_BR4,na.rm = T)
           nd_m4<-mean(ND_BR4,na.rm = T)
           mean_4<-(cd_m4-nd_m4)
           cd_phase4<-c(cd_phase4,mean_4)
         }
        
        CD_BR_frame5<-subset(CD_BR_data,CD_BR_time >br$EndTime[2])
        ND_BR_frame5<-subset(ND_BR_data,ND_BR_time >br$EndTime[2])
        if(nrow(CD_BR_frame5)>0 & nrow(ND_BR_frame5)>0){

          CD_BR5<-unfactor(CD_BR_frame5[,3])
          ND_BR5<-unfactor(ND_BR_frame5[,3])
          cd_m5<-mean(CD_BR5,na.rm = T)
          nd_m5<-mean(ND_BR5,na.rm = T)
          mean_5<-(cd_m5-nd_m5)
          cd_phase5<-c(cd_phase5,mean_5)
        }
        
        
          
        }
        
        
        }# value of stm>=0
      } # end if case for stmdata exactly 2*2
    }# end if case for BR and stm values not zero
  }# end of CD,MD,ED folder
  
  lim<-c(-10,10)
  plotdata<-do.call(rbind, Map(data.frame, A=cd_phase1, B=cd_phase2,C=cd_phase3,D=cd_phase4,E=cd_phase5))
  # for (i in 1:length(plotdata)){
  #   refined_plotdata<-plotdata[,i]
  #   if((t=="ED"& i==3)){
  #     refined_plotdata<-log(log(refined_plotdata))
  #         shapiro<- shapiro.test(refined_plotdata)
  #         print(shapiro$p.value)
  #   }
  #   else if((t=="MD"& i==5)){
  #     refined_plotdata<-log(refined_plotdata)
  #     shapiro<- shapiro.test(refined_plotdata)
  # print(shapiro$p.value)
  #   }
  #   # if((t=="CD"& i==1)){
  #   #
  #   #   refined_plotdata<-log(refined_plotdata)
  #   #   shapiro<- shapiro.test(refined_plotdata)
  #   #   # qqnorm(refined_plotdata)
  #   #   # qqline(refined_plotdata)
  #   #   #print(shapiro$p.value)
  #   #
  #   # }
  #   #
  #   # else if((t=="ED"& i==1)|(t=="ED"& i==3)){
  #   #
  #   #     refined_plotdata<-log(refined_plotdata)
  #   #     shapiro<- shapiro.test(refined_plotdata)
  #   #     #print(shapiro$p.value)
  #   #     # qqnorm(refined_plotdata)
  #   #     # qqline(refined_plotdata)
  #   #
  #   # }
  #   #
  #   # else if((t=="MD"& i==1)|(t=="MD"& i==4)){
  #   #
  #   #   refined_plotdata<-log(refined_plotdata)
  #   #    shapiro<- shapiro.test(refined_plotdata)
  #   #    # qqnorm(refined_plotdata)
  #   #    # qqline(refined_plotdata)
  #   #    #print(shapiro$p.value)
  #   #   #
  #   # }
  #   # # else{
  #   # #   qqnorm(refined_plotdata)
  #   # #   qqline(refined_plotdata)
  #   # # }
  #   first<- t.test(refined_plotdata,alternative="greater") 
  #   #print(first$p.value)
  #   p.ad1<-c(p.ad1,first$p.value)
  #   p.ad<-c(p.ad,first$p.value)
  #   # if(t=="CD"){
  #   #   
  #   #   if(i==1){
  #   #     mtext(expression("Phase1"),side=3,line=0.55)
  #   #     
  #   #   }
  #   #   if(i==2){
  #   #     mtext(expression("Phase2"),side=3,line=0.55)
  #   #     
  #   #   }
  #   #   if(i==3){
  #   #     mtext(expression("Phase3"),side=3,line=0.55)
  #   #     
  #   #   }
  #   #   if(i==4){
  #   #     mtext(expression("Phase4"),side=3,line=0.55)
  #   #     
  #   #   }
  #   #   if(i==5){
  #   #     mtext(expression("Phase5"),side=3,line=0.55)
  #   #     
  #   #   }
  #   #   
  #   # } # end if case i=CD
  #   
  # }
  # 
  # p.ad<-p.adjust(p.ad,method = "bonferroni")
  # print(p.ad)
  for (i in 1:length(plotdata)) {

    boxplot(plotdata[,i], type="l",ylim=lim)
    text(1,8,paste("n=",length(plotdata[,i])),cex=1.5, pos=4, col="black")
    abline(h=0,lty=3)
    refined_plotdata<-plotdata[,i]
    # if((t=="ED"& i==3)){
    #   refined_plotdata<-log(refined_plotdata)
    #   shapiro<- shapiro.test(refined_plotdata)
    #   print(shapiro$p.value)
    # }
    # else if((t=="MD"& i==5)){
    #   refined_plotdata<-log(refined_plotdata)
    #   shapiro<- shapiro.test(refined_plotdata)
    #   print(shapiro$p.value)
    # }
    # first<- t.test(refined_plotdata,alternative="greater") 
    #print(first$p.value)
   # p.ad1<-c(p.ad1,first$p.value)
    
    # if(p.ad1[i]<0.001){star=3}
    # if(p.ad1[i]<0.01&p.ad1[i]>0.001){star=2}
    # if(p.ad1[i]<0.05&p.ad1[i]>0.01){star=1}
    
    
    first<- t.test(refined_plotdata,alternative="greater",conf.level = 0.9875)
    if(first$p.value<0.001){star=3}
    if(first$p.value<0.01&first$p.value>0.001){star=2}
    if(first$p.value<0.05&first$p.value>0.01){star=1}
    

    if(star==3){mtext(expression("***"),side=3,line=0.1)}
    if(star==2){mtext(expression("**"),side=3,line=0.1)}
    if(star==1){mtext(expression("*"),side=3,line=0.1)}


    star=0


    if(i==1){
      mtext(expression("BR[in bpm]"),side=2,line=2)

    }
    if(t=="CD"){

      if(i==1){
        mtext(expression("phase1"),side=3,line=0.55)

      }
      if(i==2){
        mtext(expression("phase2"),side=3,line=0.55)

      }
      if(i==3){
        mtext(expression("phase3"),side=3,line=0.55)

      }
      if(i==4){
        mtext(expression("phase4"),side=3,line=0.55)

      }
      if(i==5){
        mtext(expression("phase5"),side=3,line=0.55)

      }

    } # end if case i=CD



  }# end for looBR for 5 plot
  # print(count1)
  # print(count2)
  # print(count3)
  # print(count4)
} # end of function
par(mfrow=c(3,5))
par(mar = c(2,3.5, 1, 2), oma=c(2, 0, 1, 0))

BR_plot("CD")
par(new=FALSE)
mtext(expression("CD"),side=4,line=1)

BR_plot("ED")
par(new=FALSE)
mtext(expression("ED"),side=4,line=1)

BR_plot("MD")
par(new=FALSE)
mtext(expression("MD"),side=4,line=1)
#dev.off()