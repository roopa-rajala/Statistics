setwd("C:\\academics\\stat\\stat")
library(rJava)
library(xlsx)
library(varhandle)
library(dplyr)
png("peda_plot.png")
count1=0
count2=0
count3=0
count4=0
count5=0
star=0
p.ad<-c()
peda_plot<-function(t){ #function starts
  cd_phase1<-c()
  cd_phase2<-c()
  cd_phase3<-c()
  cd_phase4<-c()
  cd_phase5<-c()
  cd5_phase5<-c()
  cd4_phase4<-c()
  cd3_phase3<-c()
  cd2_phase2<-c()
  cd1_phase1<-c()
  folder<-paste("./*/*",t,sep="")
  print(folder)
  folder_session<-Sys.glob(folder)
  for(f in folder_session){ #for loop for all the CD, ED, MD
    count1=count1+1
    nd<-substr(f,0,7)
    nd<-paste(nd,"*ND/*.peda",sep="")
    find_nd<-Sys.glob(nd)#nd peda file
    temp<-paste(f,"/*.peda",sep="")
    find_peda<-Sys.glob(temp)#cd peda file
    temp<-paste(f,"/*.stm",sep="")
    find_stm<-Sys.glob(temp)#cd stm file
    if(length(find_peda)!=0 & length(find_stm)!=0)       # if case for peda and stm values not zero
    {
      count2=count2+1
      cols<-c(1,2)
      stmdata <- read.xlsx2(find_stm,sheetIndex=1,startRow = 9,endRow = 11,colIndex = cols)
      peda<-unfactor(stmdata)
      if(nrow(stmdata)==2 & ncol(stmdata)==2 )       # if case for stmdata exactly 2*2
      {
        if(peda$StartTime[1]>=0 & peda$StartTime[2]>=0 & peda$EndTime[1]>=0 & peda$EndTime[2]>=0)
        {
          # print (sum(is.na(peda$StartTime)))
          #print(sum(is.na(peda$EndTime)))
          
          count3=count3+1
          CD_peda_data<-read.xlsx2(find_peda,sheetIndex=1,startRow = 9)# read peda file in CD
          CD_peda<-unfactor(CD_peda_data[,3])
          CD_peda_data1<-filter(CD_peda_data, CD_peda<0 | CD_peda>4500)
          CD_peda_time<-CD_peda_data[,2]#timeframe values
          CD_peda_time<-unfactor(CD_peda_time)
          #CD_max<-max(CD_peda_time,na.rm=T)
          # #ND peda dataset
          ND_peda_data<-read.xlsx2(find_nd,sheetIndex = 1,startRow = 9)# read peda file in ND
          ND_peda<-unfactor(ND_peda_data[,3])
          ND_peda_data1<-filter(ND_peda_data, ND_peda<0 | ND_peda>4500)
          ND_peda_time<-ND_peda_data[,2]#timeframe values
          ND_peda_time<-unfactor(ND_peda_time)
          #ND_max<-max(ND_peda_time)
          if(nrow(CD_peda_data1)==0 & nrow(ND_peda_data1)==0){
            count4=count4+1 # number of peda files
            # #mean of CD phase1
            CD_peda_frame<-subset(CD_peda_data,CD_peda_time >=0 & CD_peda_time < peda$StartTime[1])
            ND_peda_frame<-subset(ND_peda_data,ND_peda_time >=0 & ND_peda_time < peda$StartTime[1])
            
            if(nrow(CD_peda_frame)>0 & nrow(ND_peda_frame)>0){
              CD_peda<-unfactor(CD_peda_frame[,3])
              ND_peda<-unfactor(ND_peda_frame[,3])
              cd_m1<-mean(CD_peda,na.rm = T)
              nd_m1<-mean(ND_peda,na.rm = T)
              mean1_1<-(cd_m1-nd_m1)
              cd1_phase1<-c(cd1_phase1,mean1_1)
              cd_m1<-log(cd_m1)
              nd_m1<-log(nd_m1)
              mean_1<-(cd_m1-nd_m1)
              cd_phase1<-c(cd_phase1,mean_1)
              #print(cd_phase1)
            }
            
            # #mean of CD phase2
            CD_peda_frame2<-subset(CD_peda_data,CD_peda_time >=peda$StartTime[1] & CD_peda_time < peda$EndTime[1])
            ND_peda_frame2<-subset(ND_peda_data,ND_peda_time >=peda$StartTime[1] & ND_peda_time < peda$EndTime[1])
            
            if(nrow(CD_peda_frame2)>0 & nrow(ND_peda_frame2)>0){
              
              CD_peda2<-unfactor(CD_peda_frame2[,3])
              ND_peda2<-unfactor(ND_peda_frame2[,3])
              cd_m2<-mean(CD_peda2,na.rm = T)
              nd_m2<-mean(ND_peda2,na.rm = T)
              mean2_2<-(cd_m2-nd_m2)
              cd2_phase2<-c(cd2_phase2,mean2_2)
              cd_m2<-log(cd_m2)
              nd_m2<-log(nd_m2)
              mean_2<-(cd_m2-nd_m2)
              cd_phase2<-c(cd_phase2,mean_2)
              
            }
            
            
            CD_peda_frame3<-subset(CD_peda_data,CD_peda_time >=peda$EndTime[1] & CD_peda_time < peda$StartTime[2])
            ND_peda_frame3<-subset(ND_peda_data,ND_peda_time >=peda$EndTime[1] & ND_peda_time < peda$StartTime[2])
            if(nrow(CD_peda_frame3)>0 & nrow(ND_peda_frame3)>0){
              
              CD_peda3<-unfactor(CD_peda_frame3[,3])
              ND_peda3<-unfactor(ND_peda_frame3[,3])
              cd_m3<-mean(CD_peda3,na.rm = T)
              nd_m3<-mean(ND_peda3,na.rm = T)
              mean3_3<-(cd_m3-nd_m3)
              cd3_phase3<-c(cd3_phase3,mean3_3)
              cd_m3<-log(cd_m3)
              nd_m3<-log(nd_m3)
              mean_3<-(cd_m3-nd_m3)
              cd_phase3<-c(cd_phase3,mean_3)
            }
            
            CD_peda_frame4<-subset(CD_peda_data,CD_peda_time >peda$StartTime[2] & CD_peda_time < peda$EndTime[2])
            ND_peda_frame4<-subset(ND_peda_data,ND_peda_time >peda$StartTime[2] & ND_peda_time < peda$EndTime[2])
            if(nrow(CD_peda_frame4)>0 & nrow(ND_peda_frame4)>0){
              
              CD_peda4<-unfactor(CD_peda_frame4[,3])
              ND_peda4<-unfactor(ND_peda_frame4[,3])
              cd_m4<-mean(CD_peda4,na.rm = T)
              nd_m4<-mean(ND_peda4,na.rm = T)
              mean4_4<-(cd_m4-nd_m4)
              cd4_phase4<-c(cd4_phase4,mean4_4)
              cd_m4<-log(cd_m4)
              nd_m4<-log(nd_m4)
              mean_4<-(cd_m4-nd_m4)
              cd_phase4<-c(cd_phase4,mean_4)
            }
            
            CD_peda_frame5<-subset(CD_peda_data,CD_peda_time >peda$EndTime[2])
            ND_peda_frame5<-subset(ND_peda_data,ND_peda_time >peda$EndTime[2])
            if(nrow(CD_peda_frame5)>0 & nrow(ND_peda_frame5)>0){
              
              CD_peda5<-unfactor(CD_peda_frame5[,3])
              ND_peda5<-unfactor(ND_peda_frame5[,3])
              cd_m5<-mean(CD_peda5,na.rm = T)
              nd_m5<-mean(ND_peda5,na.rm = T)
              mean5_5<-(cd_m5-nd_m5)
              cd5_phase5<-c(cd5_phase5,mean5_5)
              cd_m5<-log(cd_m5)
              nd_m5<-log(nd_m5)
              mean_5<-(cd_m5-nd_m5)
              cd_phase5<-c(cd_phase5,mean_5)
            }
            
            
            
          }
          
          
        }# value of stm>=0
      } # end if case for stmdata exactly 2*2
    }# end if case for peda and stm values not zero
  }# end of CD,MD,ED folder
  
  plotdata<-do.call(rbind, Map(data.frame, A=cd_phase1, B=cd_phase2,C=cd_phase3,D=cd_phase4,E=cd_phase5))
  plotdata1<-do.call(rbind, Map(data.frame, A=cd1_phase1, B=cd2_phase2,C=cd3_phase3,D=cd4_phase4,E=cd5_phase5))
  
  for (i in 1:length(plotdata1)){
    refined_plotdata<-plotdata1[,i]
    
     #shapiro<- shapiro.test(refined_plotdata)
       # qqnorm(refined_plotdata)
       # qqline(refined_plotdata)
       # print(shapiro$p.value)
    
     first<- t.test(refined_plotdata,alternative="greater")
    #  # print(first$p.value)
      p.ad<-c(p.ad,first$p.value)
    # if(t=="CD"){
    # 
    #   if(i==1){
    #     mtext(expression("Phase1"),side=3,line=0.55)
    # 
    #   }
    #   if(i==2){
    #     mtext(expression("Phase2"),side=3,line=0.55)
    # 
    #   }
    #   if(i==3){
    #     mtext(expression("Phase3"),side=3,line=0.55)
    # 
    #   }
    #   if(i==4){
    #     mtext(expression("Phase4"),side=3,line=0.55)
    # 
    #   }
    #   if(i==5){
    #     mtext(expression("Phase5"),side=3,line=0.55)
    # 
    #   }
    # 
    #  } # end if case i=CD
    
  }
  # print(p.ad)
   lim=c(-1.5,1.0)
  for (i in 1:length(plotdata)) {

    boxplot(plotdata[,i], type="l",ylim=lim)
    text(1,0.8,paste("n=",length(plotdata[,i])),cex=1.0, pos=4, col="red")
    abline(h=0,lty=3)
    if(p.ad[i]<0.001){star=3}
    if(p.ad[i]<0.01&p.ad[i]>0.001){star=2}
    if(p.ad[i]<0.05&p.ad[i]>0.01){star=1}

    #
    if(star==3){mtext(expression("***"),side=3,line=0.05)}
    if(star==2){mtext(expression("**"),side=3,line=0.05)}
    if(star==1){mtext(expression("*"),side=3,line=0.05)}
    star=0
    if(i==1){
      mtext(expression("[k"~Omega~"]"),side=2,line=2)

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



  }# end for loopeda for 5 plot
  # 
} # end of function
par(mfrow=c(3,5))
par(mar = c(2,3.5, 1, 2), oma=c(2, 0, 1, 0))

peda_plot("CD")
par(new=FALSE)
mtext(expression("CD"),side=4,line=1)

peda_plot("ED")
par(new=FALSE)
mtext(expression("ED"),side=4,line=1)

peda_plot("MD")
par(new=FALSE)
mtext(expression("MD"),side=4,line=1)
mtext("Fig:Validation of Palm PEDA channel", side = 1, outer = TRUE)
dev.off()