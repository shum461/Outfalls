


library(leaflet)
library(crosstalk)
library(tidyverse)
library(FedData)
library(magrittr)
library(GISTools)
library(gdistance)
#library(ggmap)
library(sp)
library(rgeos)
library(sp)
library(rgdal)
library(mapview)
library(sf)
library(shiny)
library(raster)
library(DT)
library(shinyjs)
library(dplyr)
#library(spdplyr)
library(htmlTable)
library(htmltools)
library(rhandsontable)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~(1)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# read in the outfall data, fix the blank rows issue so each row has a permit number
# count number of outfalls for each Permit.Number. Then look for duplicate outfalls at each facility based on lon, lat
# add these columns to the data frame and rename them

outfalls=read.csv("BRRO.csv",h=T)%>%
mutate_at(vars(colnames(.)),.funs = funs(ifelse(.=="", NA, as.character(.))))%>% 
fill(Permit.Number,.direction = "down") %>% filter(!is.na(Facility))%>% 
add_count(Permit.Number) %>%
add_count(Permit.Number,Latitude.Decimal,Longitude.Decimal)%>%
rename(Total_Outfalls=n,Outfalls_same_Loc=nn)

# e.g. First Piedmont Corp - Ringgold has 5 outfalls, 4 are in the same location
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~(2)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#read in the wla data, fix the blank rows issue
options(scipen = 999)

wla=read.csv("BRRO_WLA.csv",h=T)%>%
mutate_at(vars(colnames(.)),.funs = funs(ifelse(.=="", NA, as.character(.))))%>% 
fill( everything(),.direction = "down")%>% 
filter(!is.na(Permit_Program_Name))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~(3)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#combine (i.e. "join") outfalls and wla based on permit number, if no match fill with NA. If NA rename to say "No WLA"

outfalls=outfalls %>% left_join(wla,by="Permit.Number") 
outfalls=outfalls %>%
mutate(Current.WLA=ifelse(is.na(Current.WLA),"No WLA",Current.WLA))

#only want outlfalls with WLA? Filter and remove the rows with "No WLA" 
#  outfalls %>% filter(!Current.WLA=="No WLA")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~(4)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#set map projections, read in all shapefiles in directory (i.e. dir) using lapply
prj <- CRS("+proj=utm +zone=18 +datum=WGS84 +units=ft")
WGS84 <-CRS("+proj=longlat +datum=WGS84")

dir="C:/Users/vvt97279/Documents/outfalls/BRO"

Files2=list.files(dir,pattern="shp$", full.names = T)
Files2=Files2[c(-1,-8)]


shapes=lapply(Files2,readOGR) 
shapes2=lapply(shapes,spTransform,CRS("+proj=longlat +datum=WGS84")) 

#convert outfalls .csv to SpatialPointsDataFrame in same cood ref as shapefiles
outfalls$Longitude.Decimal=as.numeric(outfalls$Longitude.Decimal)
outfalls$Latitude.Decimal =as.numeric(outfalls$Latitude.Decimal)
coordinates(outfalls) <- ~ Longitude.Decimal + Latitude.Decimal
proj4string(outfalls) <- proj4string(shapes2[[1]])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~(5)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# for each outfall, find the distance to each shapefile 
# This takes a while to run
  
  out=NULL

  for (i in seq_along(shapes2)) {
  k=geosphere::dist2Line(p=outfalls,line=shapes2[[i]])
  k=data.frame(k)
  
  out=rbind(out,k)
  }

  # possibly more efficient way to apply dist2Line
  
#shapes2 %>%
#map(~geosphere::dist2Line(p=outfalls,line=.)) %>%
#set_names(c("reservoirbacteria","reservoirchla",
#              "reservoirDO","reservoirHg","reservoirpcb","reservoirpH","riverbact","riverbenthics",
#              " riverinebacteria","riverinebenthics",
#              "riverineDO","riverineHg","riverinepcb","riverinepH","riverinetemp")) 
  
  
  
#___________________Output________________________________
  
## 860 outfalls * 15 impairment layers = 12900 rows
## BRO did not have reservoir_2018_temp impairment (i.e.  "no features found") so it was excluded
## less than 50'  218  outfalls
## less than  100' 343 outfalls

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~(6)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
outfalls_df=as.data.frame(outfalls)
outfalls_rep=purrr::map_df(seq_len(15), ~outfalls_df)


#open BRO and TMDL shapefiles separately
TMDL=readOGR(dsn="~/outfalls",layer="TMDL")
TMDL=spTransform(TMDL, WGS84)

BRO=readOGR(dsn="~/outfalls/BRO",layer="BRO")
BRO=spTransform(BRO, WGS84)

#Are outfalls in a local TMDL?
outfalls_in_TMDL=over(outfalls,TMDL) 
outfalls_in_TMDL=outfalls_in_TMDL %>% 
  dplyr::select(PJ_NAME:IMP_NAME) %>%
  mutate(Local_TMDL=ifelse(is.na(PJ_NAME),"NO","YES"))


#combine 
All=cbind(out,outfalls_rep,outfalls_in_TMDL)

#remove file path 
Files3=Files2%>% 
  gsub(dir, '', .) %>% 
  gsub('Clip', '', .)%>% 
  gsub("[[:punct:]]", "", .)%>%
  gsub('[[:digit:]]+', '', .)%>%
  gsub('shp', '', .) 


files_names=Files3 %>% slice(rep(1:n(), each =860 ))
#files_rep=purrr::map_df(seq_len(860), ~Files3)

Allfiles=cbind(files_names, All)
#riverbact and riverbenthics are duplicates of riverinebacteria and riverinebenthics, can delete these rows
All_nodup=Allfiles %>% filter(!Files3 %in% c("riverbact", "riverbenthics"))

write.csv(All_nodup,"BRO_Outfalls_output.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#[7] "C:/Users/vvt97279/Documents/outfalls/BRO/river_bact.shp"                  
#[8] "C:/Users/vvt97279/Documents/outfalls/BRO/river_benthics.shp" 

#create a data.frame of all the impairments shapefiles
shapes3=shapes2[c(-7,-8)]

names(shapes3)=(c("reservoirbacteria","reservoirchla",
            "reservoirDO","reservoirHg",
            "reservoirpcb","reservoirpH","riverinebacteria","riverinebenthics",
            "riverineDO","riverineHg","riverinepcb","riverinepH","riverinetemp"))




all_shapes=map_df(shapes3,as.data.frame,.id="Assessment1") %>%
mutate(Assessment1=factor(Assessment1))%>%
group_by(Assessment1) %>%
mutate(ID1=row_number()) %>%
dplyr::select(ID1,WATER_NAME,IMP_CAUSE,Assessment1,ID305B,LOCATION)

all_shapes=data.frame(all_shapes)



bR=read.csv("BRO_Outfalls_output3.csv",h=T)

Imp_Out_TMDL=bR %>% inner_join(all_shapes,by=c("ASSESSMENT"="Assessment1","ID"="ID1"))
Final_BRO=write.csv(Imp_Out_TMDL,"Imp_Out_TMDL.csv")

water=c("River","Creek")

q2=q %>% 
  gsub("River", '',.WATER_NAME,ignore.case=T) %>% 
  gsub("River", '',.Receiving.Stream.x,ignore.case=T) %>% 
  
  
  gsub("Creek", '',q$WATER_NAME,ignore.case=T) %>% 
  gsub("Creek", '',q$Receiving.Stream.x,ignore.case=T) %>% 
  mutate(is.f=str_detect(q$WATER_NAME, paste(q$Receiving.Stream.x, collapse = '|')))
  
  
  
  gsub('Clip', '', .)%>% 
  gsub("[[:punct:]]", "", .)%>%
  gsub('[[:digit:]]+', '', .)%>%
  gsub('shp', '', .) 
  

mutate(is.f=str_detect(q$WATER_NAME, paste(q$Receiving.Stream.x, collapse = '|')))

Files2%>% 
  








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~(6)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~











