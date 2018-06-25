setwd("C:\\academics\\stat\\stat")
library(rJava)
library(xlsx)
library(varhandle)
library(dplyr)
#pdf("pp_plot.pdf")
pp_plot<-function(t){ #function starts
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
  p.ad<-c()
  star=0
  folder<-paste("./*/*",t,sep="")
  print(folder)
  folder_session<-Sys.glob(folder)
  for(f in folder_session){ #for loop for all the CD, ED, MD
    nd<-substr(f,0,7)
    nd<-paste(nd,"*ND/*.pp",sep="")
    find_nd<-Sys.glob(nd)#nd pp file
    temp<-paste(f,"/*.pp",sep="")
    find_pp<-Sys.glob(temp)#cd pp file
    temp<-paste(f,"/*.stm",sep="")
    find_stm<-Sys.glob(temp)#cd stm file
    if(length(find_pp)!=0 & length(find_stm)!=0)       # if case for pp and stm values not zero
    {
      cols<-c(1,2)
      stmdata <- read.xlsx2(find_stm,sheetIndex=1,startRow = 9,endRow = 11,colIndex = cols)
      br<-unfactor(stmdata)
      if(nrow(stmdata)==2 & ncol(stmdata)==2 )       # if case for stmdata exactly 2*2
      {#CD pp datset
        CD_pp_data<-read.xlsx2(find_pp,sheetIndex=1,startRow = 9)# read pp file in CD
        CD_pp_time<-CD_pp_data[,2]#timeframe values
        CD_pp_time<-unfactor(CD_pp_time) #
        # #ND pp dataset
        ND_pp_data<-read.xlsx2(find_nd,sheetIndex = 1,startRow = 9)# read pp file in ND
        ND_pp_time<-ND_pp_data[,2]#timeframe values
        ND_pp_time<-unfactor(ND_pp_time)
        # # #mean of CD phase1
        CD_pp_frame<-subset(CD_pp_data,CD_pp_time >=0 & CD_pp_time < br$StartTime[1])
        ND_pp_frame<-subset(ND_pp_data,ND_pp_time >=0 & ND_pp_time < br$StartTime[1])
        
        if(nrow(CD_pp_frame)>0 & nrow(ND_pp_frame)>0){
          CD_pp<-unfactor(CD_pp_frame[,4])
          ND_pp<-unfactor(ND_pp_frame[,4])
          cd_m1<-mean(CD_pp,na.rm = T)
          nd_m1<-mean(ND_pp,na.rm = T)
          mean1_1<-(cd_m1-nd_m1)
          cd1_phase1<-c(cd1_phase1,mean1_1)
          cd_m1<-log(cd_m1)
          nd_m1<-log(nd_m1)
          mean_1<-(cd_m1-nd_m1)
          cd_phase1<-c(cd_phase1,mean_1)
        }
        #mean of CD phase2
        CD_pp_frame2<-subset(CD_pp_data,CD_pp_time >=br$StartTime[1] & CD_pp_time < br$EndTime[1])
        ND_pp_frame2<-subset(ND_pp_data,ND_pp_time >=br$StartTime[1] & ND_pp_time < br$EndTime[1])
        # print(ND_pp_frame2)
        # print(nrow(ND_pp_frame2))
        if(nrow(CD_pp_frame2)>0 & nrow(ND_pp_frame2)>0){
          CD_pp2<-unfactor(CD_pp_frame2[,4])
          ND_pp2<-unfactor(ND_pp_frame2[,4])
          cd_m2<-mean(CD_pp2,na.rm = T)
          nd_m2<-mean(ND_pp2,na.rm = T)
          mean2_2<-(cd_m2-nd_m2)
          cd2_phase2<-c(cd2_phase2,mean2_2)
          cd_m2<-log(cd_m2)
          nd_m2<-log(nd_m2)
          mean_2<-(cd_m2-nd_m2)
          cd_phase2<-c(cd_phase2,mean_2)
        }
        
        CD_pp_frame3<-subset(CD_pp_data,CD_pp_time >=br$EndTime[1] & CD_pp_time < br$StartTime[2])
        ND_pp_frame3<-subset(ND_pp_data,ND_pp_time >=br$EndTime[1] & ND_pp_time < br$StartTime[2])
        if(nrow(CD_pp_frame3)>0 & nrow(ND_pp_frame3)>0){
          CD_pp3<-unfactor(CD_pp_frame3[,4])
          ND_pp3<-unfactor(ND_pp_frame3[,4])
          cd_m3<-mean(CD_pp3,na.rm = T)
          nd_m3<-mean(ND_pp3,na.rm = T)
          mean3_3<-(cd_m3-nd_m3)
          cd3_phase3<-c(cd3_phase3,mean3_3)
          cd_m3<-log(cd_m3)
          nd_m3<-log(nd_m3)
          mean_3<-(cd_m3-nd_m3)
          cd_phase3<-c(cd_phase3,mean_3)
        }
        # 
        CD_pp_frame4<-subset(CD_pp_data,CD_pp_time >br$StartTime[2] & CD_pp_time < br$EndTime[2])
        ND_pp_frame4<-subset(ND_pp_data,ND_pp_time >br$StartTime[2] & ND_pp_time < br$EndTime[2])
        if(nrow(CD_pp_frame4)>0 & nrow(ND_pp_frame4)>0){
          CD_pp4<-unfactor(CD_pp_frame4[,4])
          ND_pp4<-unfactor(ND_pp_frame4[,4])
          cd_m4<-mean(CD_pp4,na.rm = T)
          nd_m4<-mean(ND_pp4,na.rm = T)
          mean4_4<-(cd_m4-nd_m4)
          cd4_phase4<-c(cd4_phase4,mean4_4)
          cd_m4<-log(cd_m4)
          nd_m4<-log(nd_m4)
          mean_4<-(cd_m4-nd_m4)
          cd_phase4<-c(cd_phase4,mean_4)
        }
        # 
        CD_pp_frame5<-subset(CD_pp_data,CD_pp_time >br$EndTime[2] & CD_pp_time < max(CD_pp_time,na.rm=T))
        ND_pp_frame5<-subset(ND_pp_data,ND_pp_time >br$EndTime[2] & ND_pp_time < max(CD_pp_time,na.rm=T))
        if(nrow(CD_pp_frame5)>0 & nrow(ND_pp_frame5)>0){
          CD_pp5<-unfactor(CD_pp_frame5[,4])
          ND_pp5<-unfactor(ND_pp_frame5[,4])
          cd_m5<-mean(CD_pp5,na.rm = T)
          nd_m5<-mean(ND_pp5,na.rm = T)
          mean5_5<-(cd_m5-nd_m5)
          cd5_phase5<-c(cd5_phase5,mean5_5)
          cd_m5<-log(cd_m5)
          nd_m5<-log(nd_m5)
          mean_5<-(cd_m5-nd_m5)
          cd_phase5<-c(cd_phase5,mean_5)
        }
        
        
      } # end if case for stmdata exactly 2*2
    }# end if case for pp and stm values not zero
  }# end of CD,MD,ED folder
  
  plotdata<-do.call(rbind, Map(data.frame, A=cd_phase1, B=cd_phase2,C=cd_phase3,D=cd_phase4,E=cd_phase5))
  plotdata1<-do.call(rbind, Map(data.frame, A=cd1_phase1, B=cd2_phase2,C=cd3_phase3,D=cd4_phase4,E=cd5_phase5))
  
  
  for (i in 1:length(plotdata)){
    refined_plotdata<-plotdata[,i]
    shapiro<- shapiro.test(refined_plotdata)
    
    # qqnorm(refined_plotdata)
    # qqline(refined_plotdata)
    # print(shapiro$p.value)
    
    #  first<- t.test(refined_plotdata,alternative="greater")
    # #  print(first$p.value)
    #  p.ad<-c(p.ad,first$p.value)
    # # if(t=="CD"){
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
    # } # end if case i=CD
    # 
  }
  #print(p.ad)
  lim<-c(-0.2,0.8)
  for (i in 1:length(plotdata)) {

    boxplot(plotdata[,i], type="l",ylim=lim)
    text(1,0.7,paste("n=",length(plotdata[,i])),cex=1.5, pos=4, col="black")
    abline(h=0,lty=3)
    # if(p.ad[i]<0.001){star=3}
    # if(p.ad[i]<0.01&p.ad[i]>0.001){star=2}
    # if(p.ad[i]<0.05&p.ad[i]>0.01){star=1}

    first<- t.test(plotdata[,i],alternative="greater",conf.level = 0.9875)
    print(first$p.value)
    if(first$p.value<0.001){star=3}
    if(first$p.value<0.01&first$p.value>0.001){star=2}
    if(first$p.value<0.05&first$p.value>0.01){star=1}
    if(star==3){mtext(expression("***"),side=3,line=0.1)}
    if(star==2){mtext(expression("**"),side=3,line=0.1)}
    if(star==1){mtext(expression("*"),side=3,line=0.1)}

    star=0

    if(i==1){
      mtext(expression("PP[in pp]"),side=2,line=2)

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

  } # end if case for stmdata exactly 2*2



   }# end for loopp for 5 plot
  
} # end of function
par(mfrow=c(3,5))
par(mar = c(2,3.5, 1, 2), oma=c(2, 0, 1, 0))
pp_plot("CD")
par(new=FALSE)

mtext(expression("CD"),side=4,line=1)

pp_plot("ED")
par(new=FALSE)
#mtext(expression("pp"),side=2,line=1)
mtext(expression("ED"),side=4,line=1)

pp_plot("MD")
par(new=FALSE)
#mtext(expression("pp"),side=2,line=1)
mtext(expression("MD"),side=4,line=1)
mtext("Fig:QQ Plot of PP channel before log Transformation", side = 1, outer = TRUE)
#dev.off()