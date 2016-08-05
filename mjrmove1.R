
source("S:/Institutional Research/Chen/R setup/ODBC Connection.R")


library(sqldf)
PAGGPA <- sqlQuery(MSUEDW, "select distinct cohort,Pid,COLLEGE_FIRST_NAME||'-c' as COLLEGE_FIRST_NAME, 
                  MAJOR_NAME_FIRST||'-'||COLLEGE_FIRST_NAME as  MAJOR_NAME_FIRST,
                  (case when COLLEGE_DEGREE_NAME is null then 'Not Graduate' else COLLEGE_DEGREE_NAME end)||'-c' as COLLEGE_DEGREE_NAME ,
                   (case when MAJOR_NAME_DEGREE is null then 'Not Graduate' else MAJOR_NAME_DEGREE end )||'-'|| (case when COLLEGE_DEGREE_NAME is null then 'Not Graduate' else COLLEGE_DEGREE_NAME end)as MAJOR_NAME_DEGREE
                   from OPB_PERS_FALL.PERSISTENCE_V
                   where STUDENT_LEVEL='UN' and LEVEL_ENTRY_STATUS='FRST' and COHORT in (2009,2008,2007,2006,2005)
                   and MAJOR_NAME_FIRST='No Preference'
                    ")


library(dplyr)

Agg1 <- PAGGPA %>% group_by(COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME ) %>% summarise(count=n())
Agg2 <- PAGGPA %>% group_by(COHORT, COLLEGE_FIRST_NAME,COLLEGE_DEGREE_NAME, MAJOR_NAME_FIRST,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
Agg3 <- PAGGPA %>% group_by(COHORT,  MAJOR_NAME_FIRST,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
Agg4 <- PAGGPA %>% group_by(COHORT, COLLEGE_FIRST_NAME,  MAJOR_NAME_DEGREE) %>% summarise(count=n())
Agg5 <- PAGGPA %>% group_by(COHORT, MAJOR_NAME_FIRST,  COLLEGE_DEGREE_NAME) %>% summarise(count=n())

DS1<- sqldf("select distinct COLLEGE_FIRST_NAME as coll, MAJOR_NAME_FIRST as mjr
            from Agg2
            union
            select distinct COLLEGE_DEGREE_NAME ,MAJOR_NAME_DEGREE
            from Agg2")

t<- split(DS1, DS1$coll)
listf <- function(x){ c(unique(x$coll),x$mjr)}
test <- sapply(t, listf)
listall <- as.data.frame(do.call(c,test))
#listall<-as.data.frame(listall[!duplicated(listall), ])
colnames(listall) <- "Org"
listall$merge <-1

#name

name <- as.character(listall$Org)
names <-substr(name,1,regexpr("-", name)-1) 



DS2 <- sqldf("select a.org, b.org as org1
             from listall a, listall b
             on 1=1
            ")


lvl <- unique(DS2$org1)
neworder <- c(lvl[43:44],lvl[1:42] , lvl[45:155])

levels(DS2$Org) <- neworder
DS2$org1 <- as.factor(DS2$org1)
levels(DS2$org1) <- neworder


names(Agg3)<-names(Agg1)
names(Agg4) <- names(Agg1)
names(Agg5) <- names(Agg1)
mainds <- rbind(Agg1, Agg3, Agg4, Agg5)

#2009
mds_2009 <- mainds%>% filter(COHORT==2009)

DS2_2009 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                   from DS2 a
                   left join mds_2009 b
                   on a.org=COLLEGE_FIRST_NAME and a.org1=COLLEGE_DEGREE_NAME ")
#DS2_2009$Org <- as.character(DS2_2009$Org)

xtb_2009 <- xtabs(count ~ Org + org1,data=DS2_2009)


#2008
mds_2008 <- mainds%>% filter(COHORT==2008)

DS2_2008 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                  from DS2 a
                  left join mds_2008 b
                  on a.org=COLLEGE_FIRST_NAME and a.org1=COLLEGE_DEGREE_NAME ")
#DS2_2008$Org <- as.character(DS2_2008$Org)
xtb_2008 <- xtabs(count ~ Org + org1,data=DS2_2008)


#2007
mds_2007 <- mainds%>% filter(COHORT==2007)

DS2_2007 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                  from DS2 a
                  left join mds_2007 b
                  on a.org=COLLEGE_FIRST_NAME and a.org1=COLLEGE_DEGREE_NAME ")
xtb_2007 <- xtabs(count ~ Org + org1,data=DS2_2007)

#2006
mds_2006 <- mainds%>% filter(COHORT==2006)

DS2_2006 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                  from DS2 a
                  left join mds_2006 b
                  on a.org=COLLEGE_FIRST_NAME and a.org1=COLLEGE_DEGREE_NAME ")
xtb_2006 <- xtabs(count ~ Org + org1,data=DS2_2006)

#2005
mds_2005 <- mainds%>% filter(COHORT==2005)

DS2_2005 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                  from DS2 a
                  left join mds_2005 b
                  on a.org=COLLEGE_FIRST_NAME and a.org1=COLLEGE_DEGREE_NAME ")
xtb_2005 <- xtabs(count ~ Org + org1,data=DS2_2005)

#2005-2009


DS2_2005_09 <- sqldf("select  a.org, a.org1, (case when b.count is null then 0 else b.count end) as count
                  from DS2 a
                  left join mainds b
                  on a.org=COLLEGE_FIRST_NAME and a.org1=COLLEGE_DEGREE_NAME ")
xtb_2005_09 <- xtabs(count ~ Org + org1,data=DS2_2005_09)

#2009 new

vec_2009<-vector()
for(i in  seq(nrow(xtb_2009))) {
  x<-c(xtb_2009[nrow(xtb_2009)+1-i,])
  names(x)<-NULL
  vec_2009<-rbind(x,vec_2009)
  
}



#2008 new

vec_2008<-vector()
for(i in  seq(nrow(xtb_2008))) {
  x<-c(xtb_2008[nrow(xtb_2008)+1-i,])
  names(x)<-NULL
  vec_2008<-rbind(x,vec_2008)
  
}

#2007 new

vec_2007<-vector()
for(i in  seq(nrow(xtb_2007))) {
  x<-c(xtb_2007[nrow(xtb_2007)+1-i,])
  names(x)<-NULL
  vec_2007<-rbind(x,vec_2007)
  
}

#2006 new

vec_2006<-vector()
for(i in  seq(nrow(xtb_2006))) {
  x<-c(xtb_2006[nrow(xtb_2006)+1-i,])
  names(x)<-NULL
  vec_2006<-rbind(x,vec_2006)
  
}



#2005 new

vec_2005<-vector()
for(i in  seq(nrow(xtb_2005))) {
  x<-c(xtb_2005[nrow(xtb_2005)+1-i,])
  names(x)<-NULL
  vec_2005<-rbind(x,vec_2005)
  
}

#2005 to 2009 new

vec_2005_09<-vector()
for(i in  seq(nrow(xtb_2005_09))) {
  x<-c(xtb_2005_09[nrow(xtb_2005_09)+1-i,])
  names(x)<-NULL
  vec_2005_09<-rbind(x,vec_2005_09)
  
}


#region
#last 2 character
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

regionnum <- which(substrRight(neworder,2)=='-c')



#listall$id <- as.numeric( row.names(listall)<- 1: nrow(listall))
#reg <-sqldf("select distinct a.Org, a.id-1 as reg
 #            from listall a
  #          inner join DS1 b
   #          on a.Org=b.coll")
#region <- reg %>% group_by(Org) %>% arrange(reg) %>% filter(row_number()==1)
#regionnum <- region$reg

listtest <- list("2009"=vec_2009, "2008"=vec_2008, "2007"=vec_2007, "2006"=vec_2006,"2005"=vec_2005, "All"=vec_2005)
list <- list("names"=neworder, "regions"=regionnum, "matrix"=listtest)

#library(reshape)
#vec<-reshape(DS2_2008,direction = "wide",id="org1", timevar = "Org")


require(RJSONIO)
#test<-paste0(row1x,",",row2x,collapse = "")
#names(test)<-"test1"

jsonOut<-toJSON(list)
#cat(jsonOut)

sink('txt1.json')
cat(jsonOut)

sink()