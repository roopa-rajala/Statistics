setwd("C:\\academics\\stat\\stat")
library(rJava)
library(xlsx)
library(varhandle)
library(dplyr)
star=0
#png("HR_plot1.png")
HR_plot<-function(t){ #function starts
  cd_phase1<-c()
  cd_phase2<-c()
  cd_phase3<-c()
  cd_phase4<-c()
  cd_phase5<-c()
  cd1_phase<-c()
  cd2_phase<-c()
  cd3_phase<-c()
  cd4_phase<-c()
  cd5_phase<-c()
  ND_phase1<-c()
  ND_phase2<-c()
  ND_phase3<-c()
  ND_phase4<-c()
  ND_phase5<-c()
  p.ad<-c()
  folder<-paste("./*/*",t,sep="")
  print(folder)
  folder_session<-Sys.glob(folder)
  for(f in folder_session){ #for loop for all the CD, ED, MD
    nd<-substr(f,0,7)
    nd<-paste(nd,"*ND/*.HR",sep="")
    find_nd<-Sys.glob(nd)#nd HR file
    temp<-paste(f,"/*.HR",sep="")
    find_HR<-Sys.glob(temp)#cd HR file
    temp<-paste(f,"/*.stm",sep="")
    find_stm<-Sys.glob(temp)#cd stm file
    if(length(find_HR)!=0 & length(find_stm)!=0)       # if case for HR and stm values not zero
    {
      cols<-c(1,2)
      stmdata <- read.xlsx2(find_stm,sheetIndex=1,startRow = 9,endRow = 11,colIndex = cols)
      br<-unfactor(stmdata)
      if(nrow(stmdata)==2 & ncol(stmdata)==2 )       # if case for stmdata exactly 2*2
      {
        if(br$StartTime[1]>=0 & br$StartTime[2]>=0 & br$EndTime[1]>=0 & br$EndTime[2]>=0)
        {
          # print (sum(is.na(br$StartTime)))
          #print(sum(is.na(br$EndTime)))
          
          
          CD_HR_data<-read.xlsx2(find_HR,sheetIndex=1,startRow = 9)# read HR file in CD
          CD_HR<-unfactor(CD_HR_data[,3])
          CD_HR_data1<-filter(CD_HR_data, CD_HR<40 | CD_HR>120)
          CD_HR_time<-CD_HR_data[,2]#timeframe values
          CD_HR_time<-unfactor(CD_HR_time)
          #CD_max<-max(CD_HR_time,na.rm=T)
          # #ND HR dataset
          ND_HR_data<-read.xlsx2(find_nd,sheetIndex = 1,startRow = 9)# read HR file in ND
          ND_HR<-unfactor(ND_HR_data[,3])
          ND_HR_data1<-filter(ND_HR_data, ND_HR<40 | ND_HR>120)
          ND_HR_time<-ND_HR_data[,2]#timeframe values
          ND_HR_time<-unfactor(ND_HR_time)
          #ND_max<-max(ND_HR_time)
          if(nrow(CD_HR_data1)==0 & nrow(ND_HR_data1)==0){ 
            # number of HR files
            # #mean of CD phase1
            CD_HR_frame<-subset(CD_HR_data,CD_HR_time >=0 & CD_HR_time < br$StartTime[1])
            ND_HR_frame<-subset(ND_HR_data,ND_HR_time >=0 & ND_HR_time < br$StartTime[1])
            
            if(nrow(CD_HR_frame)>0 & nrow(ND_HR_frame)>0){
              
              CD_HR<-unfactor(CD_HR_frame[,3])
              ND_HR<-unfactor(ND_HR_frame[,3])
              cd_m1<-mean(CD_HR,na.rm = T)
              nd_m1<-mean(ND_HR,na.rm = T)
              mean_1<-(cd_m1-nd_m1)
              cd_phase1<-c(cd_phase1,mean_1)
              cd1_phase<-c(cd1_phase,cd_m1)
              ND_phase1<-c(ND_phase1,nd_m1)
              
            }
            
            # #mean of CD phase2
            CD_HR_frame2<-subset(CD_HR_data,CD_HR_time >=br$StartTime[1] & CD_HR_time < br$EndTime[1])
            ND_HR_frame2<-subset(ND_HR_data,ND_HR_time >=br$StartTime[1] & ND_HR_time < br$EndTime[1])
            
            if(nrow(CD_HR_frame2)>0 & nrow(ND_HR_frame2)>0){
              
              CD_HR2<-unfactor(CD_HR_frame2[,3])
              ND_HR2<-unfactor(ND_HR_frame2[,3])
              cd_m2<-mean(CD_HR2,na.rm = T)
              nd_m2<-mean(ND_HR2,na.rm = T)
              mean_2<-(cd_m2-nd_m2)
              cd_phase2<-c(cd_phase2,mean_2)
              cd2_phase<-c(cd2_phase,cd_m2)
              ND_phase2<-c(ND_phase2,nd_m2)
              
            }
            
            
            CD_HR_frame3<-subset(CD_HR_data,CD_HR_time >=br$EndTime[1] & CD_HR_time < br$StartTime[2])
            ND_HR_frame3<-subset(ND_HR_data,ND_HR_time >=br$EndTime[1] & ND_HR_time < br$StartTime[2])
            if(nrow(CD_HR_frame3)>0 & nrow(ND_HR_frame3)>0){
              
              CD_HR3<-unfactor(CD_HR_frame3[,3])
              ND_HR3<-unfactor(ND_HR_frame3[,3])
              cd_m3<-mean(CD_HR3,na.rm = T)
              nd_m3<-mean(ND_HR3,na.rm = T)
              mean_3<-(cd_m3-nd_m3)
              cd_phase3<-c(cd_phase3,mean_3)
              
              cd3_phase<-c(cd3_phase,cd_m3)
              ND_phase3<-c(ND_phase3,nd_m3)
              
              
            }
            
            CD_HR_frame4<-subset(CD_HR_data,CD_HR_time >br$StartTime[2] & CD_HR_time < br$EndTime[2])
            ND_HR_frame4<-subset(ND_HR_data,ND_HR_time >br$StartTime[2] & ND_HR_time < br$EndTime[2])
            if(nrow(CD_HR_frame4)>0 & nrow(ND_HR_frame4)>0){
              
              CD_HR4<-unfactor(CD_HR_frame4[,3])
              ND_HR4<-unfactor(ND_HR_frame4[,3])
              cd_m4<-mean(CD_HR4,na.rm = T)
              nd_m4<-mean(ND_HR4,na.rm = T)
              mean_4<-(cd_m4-nd_m4)
              cd_phase4<-c(cd_phase4,mean_4)
              cd4_phase<-c(cd4_phase,cd_m4)
              ND_phase4<-c(ND_phase4,nd_m4)
              
            }
            
            CD_HR_frame5<-subset(CD_HR_data,CD_HR_time >br$EndTime[2])
            ND_HR_frame5<-subset(ND_HR_data,ND_HR_time >br$EndTime[2])
            if(nrow(CD_HR_frame5)>0 & nrow(ND_HR_frame5)>0){
              
              CD_HR5<-unfactor(CD_HR_frame5[,3])
              ND_HR5<-unfactor(ND_HR_frame5[,3])
              cd_m5<-mean(CD_HR5,na.rm = T)
              nd_m5<-mean(ND_HR5,na.rm = T)
              mean_5<-(cd_m5-nd_m5)
              cd_phase5<-c(cd_phase5,mean_5)
              cd5_phase<-c(cd1_phase,cd_m5)
              ND_phase5<-c(ND_phase5,nd_m5)
              #print(cd5_phase)
            }
            
            
            
            
          }
          
          
        }# value of stm>=0
      } # end if case for stmdata exactly 2*2
    }# end if case for HR and stm values not zero
  }# end of CD,MD,ED folder
  
  
  
  plotdata<-do.call(rbind, Map(data.frame, A=cd_phase1, B=cd_phase2,C=cd_phase3,D=cd_phase4,E=cd_phase5))
  # for (i in 1:length(plotdata)){
  #   refined_plotdata<-plotdata[,i]
  #   
  #   
  #   first<- t.test(refined_plotdata,alternative="greater")
  #  # print(first$p.value)
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
  #  print(p.ad)
  # 
  lim<-c(-40,20)
  
  for (i in 1:length(plotdata)) {
    
    boxplot(plotdata[,i], type="l",ylim=lim)
    text(1,15,paste("n=",length(plotdata[,i])),cex=1.5, pos=4, col="black")
    abline(h=0,lty=3)
    refined_plotdata<-plotdata[,i]
    # if(p.ad[i]<0.001){star=3}
    # if(p.ad[i]<0.01&p.ad[i]>0.001){star=2}
    # if(p.ad[i]<0.05&p.ad[i]>0.01){star=1}
    # if(t=="MD"& i==1){
    #   refined_plotdata<-subset(plotdata[,i],plotdata[,i]>-0.2 & plotdata[,i]<5)
    # }
    # if(t=="MD" & i==2){
    # 
    #       refined_plotdata<-subset(plotdata[,i],plotdata[,i]<10 & plotdata[,i]>0)
    #       
    #     }
    # 
    first<- t.test(refined_plotdata,alternative="greater",conf.level = 0.9875)
    print(first$p.value)
    if(first$p.value<0.001){star=3}
    if(first$p.value<0.01&first$p.value>0.001){star=2}
     if(first$p.value<0.05&first$p.value>0.01){star=1}
    # 
    if(star==3){mtext(expression("***"),side=3,line=0.1)}
    if(star==2){mtext(expression("**"),side=3,line=0.1)}
    if(star==1){mtext(expression("*"),side=3,line=0.1)}
    
    star=0
    
    
    if(i==1){
      mtext(expression("HR[in bpm]"),side=2,line=2)
      
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
    
    
  }# end for looHR for 5 plot
  
} # end of function
par(mfrow=c(3,5))
par(mar = c(2,3.5, 1, 2), oma=c(2, 0, 1, 0))
HR_plot("CD")
par(new=FALSE)

mtext(expression("CD"),side=4,line=1)

HR_plot("ED")
par(new=FALSE)
mtext(expression("ED"),side=4,line=1)

HR_plot("MD")
par(new=FALSE)
mtext(expression("MD"),side=4,line=1)
mtext("Fig:Validation of Heart rate channel", side = 1, outer = TRUE)
#dev.off()