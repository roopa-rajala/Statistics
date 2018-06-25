setwd("C:\\Users\\divya\\Downloads\\Stat")
library(rJava)
library(xlsx)
library(varhandle)
library(dplyr)
count=0
cd_phase1<-c()
cd_phase2<-c()
cd_phase3<-c()
cd_phase4<-c()
cd_phase5<-c()
folder<-paste("./*/*CD")
print(folder)
folder_session<-Sys.glob(folder)
for(f in folder_session){
  nd<-substr(f,0,7)
  nd<-paste(nd,"*ND/*.pp",sep="")
  find_nd<-Sys.glob(nd)#nd pp file
  temp<-paste(f,"/*.pp",sep="")
  find_pp<-Sys.glob(temp)#cd pp file
  temp<-paste(f,"/*.stm",sep="")
  find_stm<-Sys.glob(temp)#cd stm file
  if(length(find_pp)!=0 & length(find_stm)!=0)
  {
    cols<-c(1,2)
    stmdata <- read.xlsx2(find_stm,sheetIndex=1,startRow = 9,endRow = 11,colIndex = cols)
    br<-unfactor(stmdata)
    if(nrow(stmdata)==2 & ncol(stmdata)==2 )
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
        mean_5<-(cd_m5-nd_m5)
        cd_phase5<-c(cd_phase5,mean_5)
      }
      
       
        }
    }
}
cd_phase1
cd_phase2
cd_phase3
cd_phase4
cd_phase5