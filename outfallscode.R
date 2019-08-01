
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
#coordinates(data_raw) = ~Longitude + Latitude
#ip2=spTransform(shape(), CRS("+proj=longlat +datum=WGS84"))
#proj4string(data_raw)= CRS("+proj=longlat +datum=WGS84")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read in the outfall data
data=read.csv("troplanning.csv",h=T)

data2=data %>% mutate(Row=row_number()) %>%
dplyr::select(Outfall.No,Permit.Number,LAT,LON,Facility,Receiving.Stream,Row,Outfall.No)%>%
filter(LAT > 0) 

# read in the saved points
points=read.csv("PointDistance.csv",h=T)
points.df=data.frame(points)
wla=read.csv("WLA.csv",h=T)
coordinates(points) <- ~ LON + LAT
proj4string(points) <- proj4string(CB)

#simple features 
data_sf <- st_as_sf(data2, crs = 4326, coords = c("LON", "LAT"))
###################


prj <- CRS("+proj=utm +zone=18 +datum=WGS84 +units=ft")
WGS84 <-CRS("+proj=longlat +datum=WGS84") 
#WGS84 <-CRS("+proj=utm +zone=18 +init=epsg:32147") 

####### sHAPE fILES########################

Tidewater <- readOGR(dsn="~/outfalls",layer="Tidewater")
Tidewater=spTransform(Tidewater, WGS84)

Bacteria <- readOGR(dsn="~/outfalls",layer="Bacteria")
Bacteria=spTransform(Bacteria, WGS84)
Bacteria@data$Type="Bacteria"

Benthic <- readOGR(dsn="~/outfalls",layer="Benthic")
Benthic=spTransform(Benthic, WGS84)
Benthic@data$Type="Benthic"

DO <- readOGR(dsn="~/outfalls",layer="DO")
DO=spTransform(DO, WGS84)
DO@data$Type="Dissolved Oxygen"

CB=readOGR(dsn="~/outfalls",layer="CB_Virginia")
CB=spTransform(CB, WGS84)

TMDL=readOGR(dsn="~/outfalls",layer="TMDL")
TMDL=spTransform(TMDL, WGS84)


#combo=list("bacteria"=Bacteria,"benthic"=Benthic,"dissolved oxygen"=DO)
#dir <- "~/outfalls"
#ff <- list.files(dir, pattern="\\.shp$", full.names=TRUE)



DO2=DO
Benthic2=Benthic
Bacteria2=Bacteria

All <-union(DO2,Benthic2)
Combined=union(All, Bacteria2)



#for (i in 1:nrow(joined_DO)) 
#map<-
#map%>% addPolylines(lat=c(joined_DO[i,]$lat,joined_DO[i,]$LAT),lng=c(joined_DO[i,]$long,joined_DO[i,]$LON))


do.df=data.frame(DO)
bact.df=data.frame(Bacteria)
benthic.df=data.frame(Benthic)

data3=data2 %>% dplyr::select(LON,LAT)
xy <- data3[,c(1,2)]


#Takes a few min to run, now saved as csv called PointDistance
#___________________________________________________________________

#a=geosphere::dist2Line(sp, DO)
#b=geosphere::dist2Line(sp, Bacteria)
#c=geosphere::dist2Line(sp, Benthic)

#____________________________________________________________________

sp <- SpatialPointsDataFrame(data=data2,coords=xy, proj4string=CRS(proj4string(j[[1]])))



dir="C:/Users/vvt97279/Documents/outfalls"

Files=list.files(dir,pattern="shp$", full.names = TRUE)

x <- lapply(Files, shapefile)

x_sf=lapply(Files,
            function(x)
            {y=st_transform(st_read(x),crs=4326)
            y
            }
            )
names(x_sf)= gsub("C:/Users/vvt97279/Documents/outfalls/","",Files)


y=lapply(x,spTransform,CRS("+proj=longlat +datum=WGS84"))



#Remove all shapes other than impairments
y2=y[-c(3,17,18)]

y2[[1]]$Shape_Area =20
y2[[2]]$Shape_Area =20
y2[[3]]$Shape_Area =20

#convert all shape files of impairments to data.frame
library(plyr)
df <- ldply(y2, data.frame)

df$Type = c(
  rep("Bacteria_River", 63),
  rep("Benthic_River", 39),
  rep("DO_River", 33),
  rep("Benthic_Estaurine", 88),
  rep("DO_Estaurine", 269),
  rep("Mercury_Estaurine", 3),
  rep("pcb_Estaurine", 310),
  rep("pH_Estaurine", 2),
  rep("SAV_Estaurine", 1),
  rep("Bacteria_Estaurine", 203),
  rep("Chla_Reservoir", 4),
  rep("DO_Reservoir", 13),
  rep("Mercury_Reservoir", 5),
  rep("pcb_Reservoir", 4),
  rep("pH_Reservoir", 2))


df$OBJECTID=factor(df$OBJECTID)

########################



out=NULL

for (i in seq_along(y)) {
  k=geosphere::dist2Line(p=sp,line=y[[i]])
  k=data.frame(k)
  
  out=rbind(out,k)
  }


#add the permit number to the distance df

l=rep(data2$Permit.Number,18)
h=data.frame(Files)
hh= h %>% slice(rep(1:n(), each = 623))
Type=c("Bacteria_River","Benthic_River","DO_River","Benthic_Estaurine","DO_Estaurine","Mercury_Estaurine","pcb_Estaurine",
        "pH_Estaurine","SAV_Estaurine","Bacteria_Estaurine","Chla_Reservoir","DO_Reservoir","Mercury_Reservoir","pcb_Reservoir","pH_Reservoir")
nn=data.frame(Type) 
names_n=nn%>% slice(rep(1:n(), each = 623))

out2=out %>% 
mutate(Permit.Number=rep(data2$Permit.Number,18),OBJECTID=factor(ID),Files=hh$Files) %>%
filter(!str_detect(Files,"outfalls/Tidewater.shp"))%>%
filter(!str_detect(Files,"outfalls/CB_Virginia.shp"))%>%
filter(!str_detect(Files,"outfalls/TMDL.shp")) %>%
bind_cols(names_n)

names=c("Bacteria_River","Benthic_River","DO_River","Benthic_Estaurine","DO_Estaurine","Mercury_Estaurine","pcb_Estaurine",
"pH_Estaurine","SAV_Estaurine","Bacteria_Estaurine","Chla_Reservoir","DO_Reservoir","Mercury_Reservoir","pcb_Reservoir","pH_Reservoir")




out3=out2 %>%
group_by(Files) %>% 
left_join(df,by=c("OBJECTID","Type"))%>%
ungroup()


###Points in polygons##########################################
data3=data2
coords= data3[,c("LON", "LAT")]
crs=proj4string(CB)
data=data3

d_points=SpatialPointsDataFrame(data=data,coords=coords)
proj4string(d_points) <- proj4string(CB)



df2_in_bay=over(d_points,CB) 
df2_in_TMDL=over(d_points,TMDL) 

df2_in_bay2=cbind(df2_in_bay,Lon=data2$LON,Lat=data2$LAT,Outfall.No=data2$Outfall.No,Permit.Number=data2$Permit.Number,Facility=data2$Facility)

Points_In_Poly=cbind(df2_in_bay,df2_in_TMDL,data2) 

Points_In_Poly2= Points_In_Poly%>%
setNames(make.names(names(.), unique = TRUE)) %>%
#rename(Permit.Number = data2$Permit.Number,Facility= data2$Facility, Outfall.No=data2$Outfall.No) %>%
mutate(Within_ChesBay= ifelse(is.na(cb_land_),"No","Yes"),
Local_TMDL=ifelse(is.na(PROJECT),"No","Yes"),
WLA=ifelse(is.na(match(Permit.Number,wla$Permit.Number)),"No",wla$Current.WLA))


Points_In_Poly3=Points_In_Poly2 %>% 
#dplyr::select(Outfall.No,Permit.Number,Facility,TMDL_Name=PJ_NAME,TMDL_Imp=IMP_NAME,Within_ChesBay,Local_TMDL,WLA,Lon,Lat)%>%
add_count(Permit.Number) %>%
add_count(Permit.Number,LON,LAT)%>%
rename(Total_Outfalls=n,Outfalls_same_Loc=nn)

                                           
##################
#Need to rep data.frame of Points_in_polygon for every Impaired layer (i.e. 15 times for 15 layers)

New=purrr::map_df(seq_len(15), ~Points_In_Poly3)

#all_distance is the saved Dist2line output,623 points*15 layers = 9345 rows
all=read.csv("all_distance.csv",h=T)

#

#bind these together into one data.frame
New_all=cbind(New,all) 

New_All =New_all %>% setNames(make.names(names(.), unique = TRUE))%>% 
mutate(Files = str_replace(Files, "Estaurine","Estuarine")) %>%
mutate(Type = str_replace(Type, "Estaurine","Estuarine")) 
  
New_All=New_All %>% dplyr::select(Outfall.No:WILDLIFE)


New_AllF=cbind(New_All,Outfall.No)




write.csv(New_All,"New_all_combined2.csv")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#In_CB=over(points,CB) %>% mutate(Permit.Number=points$Permit.Number, LAT=points$LAT.1,LON=points$LON.1)
#In_TMDL3=cbind(In_CB,over(points,TMDL))

#In3=In_TMDL3%>% setNames(make.names(names(.), unique = TRUE)) %>% 
#  mutate(Within_ChesBay= ifelse(is.na(cb_land_),"No","Yes"),Local_TMDL=ifelse(is.na(PROJECT),"No","Yes"),WLA=ifelse(is.na(match(Permit.Number,wla$Permit.Number)),"No",wla$Current.WLA))%>%
#  dplyr::select(Permit.Number,LAT,LON,Within_ChesBay,Local_TMDL,cntyname,PJ_NAME,PJ_ID,WLA)


In4=cbind(points.df,In3)
##################################################



#points_in_poly= In4[ ,c("Facility","Permit.Number","LAT","LON","Receiving.Stream","Within_ChesBay","Local_TMDL","WLA")]


#lol=left_join(points_in_poly,out3,by="Permit.Number")
  
#final=lol[!duplicated(lol[c(2,9,13)]),]
num.outfalls=read.csv("Number_of_Outfalls.csv",h=T) %>% group_by(Permit.Number)


all=read.csv("all_distance.csv",h=T)
final=read.csv("Final.csv",h=T)
final500=final %>% filter(distance < 500)
#############################


blue=final500[,c("Permit.Number","LAT","LON","LOCATION",
            "distance","Local_TMDL","Within_ChesBay","WLA","Receiving.Stream","ID305B","Type")]

red=final500[,c("Permit.Number","lat","lon","LOCATION",
           "distance","Local_TMDL","Within_ChesBay","WLA","Receiving.Stream","ID305B","Type","WATER_NAME")]


New_All500=New_All %>% filter(distance < 500)

shared <- SharedData$new(New_All500)


pal <- colorFactor(palette = c("blue", "red", "green"), 
                   levels = c("Dissolved Oxygen", "Bacteria", "Benthic"))

pal2 <- colorFactor(palette = c("green", "red", "red","red"), 
                    levels = c("Benthic-Macroinvertebrate Bioassessments (Streams)", "Total Fecal Coliform", "Escherichia coli","Enterococcus"))

listpal = c("Dissolved Oxygen", "Bacteria", "Benthic")
#~~~

#Estaurine DO is off

bscols(widths = c(3,NA),
       list(       
         filter_checkbox("chesbay", "ChesBay", shared, ~Within_ChesBay, inline = TRUE),
         filter_checkbox("tmdl", "TMDL", shared, ~Local_TMDL, inline = TRUE)
       ),
       list(
         filter_slider("hp", "distance (meters)", shared, ~round(distance,1), width = "80%",step=1),
         leaflet(shared,height="800px")%>%
           addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
           addTiles(options = providerTileOptions(noWrap = TRUE), group="Street") %>%
           #setView(lng = -78.39844, lat = 37.82715, zoom=8) %>%
           addMouseCoordinates(style = "basic") %>%
           addPolygons(fill = FALSE,data=Tidewater,group="TRO",color="yellow") %>%
           addPolygons(fill = TRUE,data=TMDL,group="TMDL",color=~pal2(IMP_NAME)) %>%
           addPolygons(fill = FALSE,data=y2[[3]],group="Estaurine_DO",color="blue") %>%
           addPolygons(fill = TRUE,data=y2[[4]],group="Estaurine_Benthic",color="orange") %>%
           #addPolygons(fill = FALSE,data=CB,group="ChesBay") %>%
           addPolylines(data=Bacteria,group="Bacteria",color= ~pal(Type),label= ~WATER_NAME,popup=~ID305B) %>%
           addPolylines(data=Benthic,group="Benthic",color=~pal(Type),label= ~WATER_NAME,popup=~ID305B) %>%
           addPolylines(data=DO,group="DO",color= ~pal(Type),label= ~WATER_NAME,popup=~ID305B) %>% 
           
           addCircleMarkers(fillColor="blue",weight=2,group="Outfalls",lng=~Lon,lat=~Lat,label=~Permit.Number,
                            popup = paste("Facility",New_All$Facility,"<br>","Meters to nearest impaired:","<b>",
                                          round(New_All$distance,2),"</b>","<br>","Rec Water:",New_All$Receiving.Stream,"<br>",
                                          "Nearest impaired:",New_All$Type,"<br><b>","Impaired Reach Location","</b>","<br>",
                                          New_All$LOCATION,"<br>","<br>","Nearest 305(b) ID",New_All$ID305B))%>%
           
           #addCircleMarkers(color="red",data=shared,weight=1.5,group="Snapped_Outfalls",lng=~lon,lat=~lat,label=~Permit.Number,popup = paste("Facility",New_All$Facility,"<br>","meters",round(New_All$distance,2),"<br>",New_All$Type,"<br>",New_All$WATER_NAME))%>%
           addLayersControl(overlayGroups=c("TMDL","TRO","Estaurine_DO","Estaurine_Benthic","Bacteria","Benthic","DO","Snapped_Outfalls","Outfalls"),baseGroups = c("Imagery","Street"),
                            options = layersControlOptions(collapsed = TRUE))%>%
           leaflet::addLegend(pal = pal, 
                              values = listpal, 
                              position = "topright", 
                              title = "Impairment") %>% hideGroup("TMDL")
       )
)



datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%",


















df = bind_rows(
  c(
    y2[[1]] = "Bacteria_River",
    y2[[2]] = "Benthic_River",
    y2[[3]] = "DO_River",
    y2[[4]] = "Benthic_Estaurine",
    y2[[5]] = "DO_Estaurine",
    y2[[6]] = "Mercury_Estaurine",
    y2[[7]] = "pcb_Estaurine",
    y2[[8]] = "pH_Estaurine",
    y2[[9]] = "SAV_Estaurine",
    y2[[10]] = "Bacteria_Estaurine",
    y2[[11]] = "Chla_Reservoir",
    y2[[12]] = "DO_Reservoir",
    y2[[13]] = "Mercury_Reservoir",
    y2[[14]] = "pcb_Reservoir",
    y2[[15]] = "pH_Reservoir"
  ),
  .id = "source"
)



rep("Bacteria_River",63),rep("Benthic_River",38),rep("DO_River",32),rep("Benthic_Estaurine",)
  
,64:102,
103:135,
136:223,
224:492,
493:495,
496:805,
806:807,
808:808,
809:1011,
1012:1015,
1016:1028,
1029:1033,
1034:1037,
1038:1038






Bacteria_River=y[[1]] %>%()







do.call()


G=623

for (i in seq_along(y)) {
  
  D=data.frame(i) %>%
  
  
}

x2=do.call(x,list(data.frame))

shapefile1=1:623
shapefile2=624:1246

rep("Bacteria_River",623),rep("DO_River",623)

listOfDataFrames=NULL

for (i in seq_along(y2)) {
  listOfDataFrames[[i]]= data.frame(i)
}



apply(geosphere::dist2Line(sp,j))

apply(geosphere::dist2Line(sp, j),2,min)


do.call(j)



  kk=geosphere::dist2Line(sp,i)
 
  }


> apply(gDistance(spts, columbus,byid=TRUE),2,min)

print(count)

for (i in seq_along(x)
 

))




  
do.call(rbind,lapply(j,geosphere::dist2Line,sp))

geosphere::dist2Line,sp
  
lines(data[[i]]$time, data[[i]]$temp, col=cols[i])

n=

do.call(j,geosphere::dist2Line,list(sp))


#create empty list
lst <- vector("list", length(Files))

setwd("~/outfalls/")

shp_objects <- lapply(Files, function(x) {readOGR(dsn=x, 
                                                     layer=x)
  })



shapes <- lapply(Files, 
              function(x) { readOGR(dsn=x, layer="tracks") }
)




for(i in 1:length(Files)) {
  lst[[i]] <- readOGR(dsn="C:/Users/vvt97279/Documents/outfalls",layer=Files[i])
  
}



O = lapply(Files, function(x) {
  
  
  
  
  WGS84 <-CRS("+proj=longlat +datum=WGS84") 
  
  
  DF$Date <- as.character(CAN$Date)
  DF$Date <- as.Date(CAN$Date, format ="%m/%d/%y")
  DF_Merge <- merge(all.dates.frame, CAN, all = T)
  DF_Merge$Bid.Yield.To.Maturity <- NULL
  return(DF_Merge)
  })




sapply(lapply(list.files(pattern="*.shp"), dist2Line), sp)



named_shape=list(DO,Bacteria,Benthic)

getDis=function()

lapply(named_shape,dist2Line,p=sp)



a.df=data.frame(a)
a.df=a.df %>% mutate(Permit.Number=data2$Permit.Number,OBJECTID=factor(ID)) %>% left_join(data.frame(DO), by="OBJECTID")

b.df=data.frame(b)
b.df=b.df %>% mutate(Permit.Number=data2$Permit.Number,OBJECTID=factor(ID)) %>% left_join(data.frame(Bacteria), by="OBJECTID")

c.df=data.frame(c)
c.df=c.df %>% mutate(Permit.Number=data2$Permit.Number,OBJECTID=factor(ID)) %>% left_join(data.frame(Benthic), by="OBJECTID")


all=rbind(a.df,b.df,c.df)







################  map ################################################


pal <- colorFactor(palette = c("blue", "red", "green"), 
                   levels = c("Dissolved Oxygen", "Bacteria", "Benthic"))

pal2 <- colorFactor(palette = c("green", "red", "red","red"), 
                    levels = c("Benthic-Macroinvertebrate Bioassessments (Streams)", "Total Fecal Coliform", "Escherichia coli","Enterococcus"))

listpal = c("Dissolved Oxygen", "Bacteria", "Benthic")

map=leaflet(shared, height="800px")%>%
  addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="Street") %>%
  #setView(lng = -78.39844, lat = 37.82715, zoom=8) %>%
  addMouseCoordinates(style = "basic") %>%
  addPolygons(fill = FALSE,data=Tidewater,group="TRO",color="yellow") %>%
  addPolygons(fill = TRUE,data=TMDL,group="TMDL",color=~pal2(IMP_NAME)) %>%
  #addPolygons(fill = FALSE,data=CB,group="ChesBay") %>%
  addPolylines(data=Bacteria,group="Bacteria",color= ~pal(Type),label= ~WATER_NAME,popup=~ID305B) %>%
  addPolylines(data=Benthic,group="Benthic",color=~pal(Type),label= ~WATER_NAME,popup=~ID305B) %>%
  addPolylines(data=DO,group="DO",color= ~pal(Type),label= ~WATER_NAME,popup=~ID305B) %>% 
  #addPolylines(data=Combined,group="Combined",color="black",label=~WATER_NAME,popup = ~IMP_CAUSE) %>% 
  addCircleMarkers(data=shared,weight=2,group="Outfalls",~LON,~LAT,label=~Permit.Number,popup = paste("<em>","Facility",FULL$Facility,"</em>","<br>","Meters to nearest impaired:","<b>",round(FULL$distance,2),"</b>","<br>","Rec Water:",FULL$Receiving.Stream,"<br>","Nearest impaired:",OriginalPoints$Type,"<br><b>","Impaired Reach Location","</b>","<br>",FULL$LOCATION))%>%
  addCircleMarkers(color="red",data=shared,weight=1,group="Outfalls2",~lon,~lat,label=~Permit.Number,popup = paste("Facility",FULL$Facility,"<br>","meters",round(FULL$distance,2),"<br>",FULL$Type,"<br>",FULL$WATER_NAME))%>%
  addLayersControl(overlayGroups=c("TMDL","TRO","Bacteria","Benthic","DO","Outfalls2","Outfalls"),baseGroups = c("Imagery","Street"),
                   options = layersControlOptions(collapsed = TRUE))%>%
  leaflet::addLegend(pal = pal, 
                     values = listpal, 
                     position = "bottomleft", 
                     title = "Impairment") %>% hideGroup("TMDL")

















########################################################















FULL <- SpatialPointsDataFrame(data=shared,coords=SharedData[,c(3,4)], proj4string=CRS(proj4string(DO)))

















icons1 <- reactive ({
  awesomeIcons(
    icon = ifelse(raw()$In_IP=="YES" & raw()$Approved =="YES",'ion-checkmark-circle','ion-close-circle'),
    iconColor = 'black',
    library = 'ion',
    markerColor = ifelse(raw()$In_IP=="YES" & raw()$Approved =="YES",'green','red'))
})



#factpal <- colorFactor(brewer.pal(6,"BrBG"), IP$REGION) 
impairments.pal <- colorFactor(palette = c("blue", "red", "green"), 
              levels = c("DO", "Bacteria", "Benthic"))


leaflet::addLegend(pal = impairments.pal, 
                   values = FULL$Type, 
                   position = "bottomleft", 
                   title = "Impairments") %>%
  
leaflet::addLegend(colors=c('red','green'),values = unique(raw()$In_IP),labels = unique(raw()$In_IP), 
                     position = "bottomleft", 
                     title = "Acceptable Site?")

#~~Distance to DO~~~~~~~~~~~~~~~~~~~~~~~~~
#   points.distance=data.frame(points) %>% mutate(Type="DO",PointID=row_number(),OBJECTID=ID)%>%
#   inner_join(do.df, by="OBJECTID") 
#   points.distance.do=bind_cols(data.frame(sp),points.distance)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~Distance to Bacteria~~~~~~~~~~~~~~~~~~~
#   points.distance.bact=data.frame(b) %>% mutate(Type="Bacteria",PointID=row_number(),OBJECTID=ID)%>%
#   inner_join(bact.df, by="OBJECTID") 
#   points.distance.bact=bind_cols(data.frame(sp),points.distance.bact)
#~~~Distance to Benthic~~~~~~~~~~~~~~~~~~~
#   points.distance.benthic=data.frame(c) %>% mutate(Type="Benthic",PointID=row_number(),OBJECTID=ID)%>%
#   inner_join(benthic.df, by="OBJECTID") 
#   points.distance.benthic=bind_cols(data.frame(sp),points.distance.benthic)

##~~Combined points~~~~~~~~~~~~~~~~~~~~~~~~~ 
#   PointDistance=rbind(points.distance.do,points.distance.benthic,points.distance.bact)



PointDistance1000=points %>% filter(distance < 1000)



#Points<- SpatialPointsDataFrame(data=points,coords=points[,c(3,4)], proj4string=CRS(proj4string(DO)))

OriginalPoints<- SpatialPointsDataFrame(data=PointDistance1000,coords=PointDistance1000[,c(3,4)], proj4string=CRS(proj4string(DO)))

RefPoints<- SpatialPointsDataFrame(data=PointDistance1000,coords=PointDistance1000[,c(12,13)], proj4string=CRS(proj4string(DO)))

###Points in Poly













points.distance3=points.distance2 %>% filter(distance < 1000)
joined_DO_red <- SpatialPointsDataFrame(data=points.distance3,coords=points.distance3[,c(11,12)], proj4string=CRS(proj4string(DO)))


ff=data.frame(joined_DO)

fred=data.frame(joined_DO_red)

    
p2=as(Points, 'SpatialPoints') 


pal <- colorFactor(c("green","yellow", "red"), domain = c("ship", "pirate"))



           
sf <- st_transform(data2, crs = WGS84)
lineWgs84.sf <- st_transform(Combined, crs = WGS84)

dist <- geosphere::dist2Line(p = st_coordinates(pointsWgs84.sf), line = st_coordinates(lineWgs84.sf)[,1:2])





###############################################


sp2 <- spTransform(sp,prj)
DO2 <- spTransform(DO,prj)


gd <- gDistance(sp2,DO2,byid=TRUE)

DO.dist <- apply(gd, 2,min)

bact <- gDistance(sp,Bacteria,byid=TRUE)
Bact.dist <- apply(bact, 2,min)

benthic<- gDistance(sp,Benthic,byid=TRUE)
Benthic.dist <- apply(benthic,2,min)



Distance=data.frame(DO.dist,Bact.dist,Benthic.dist) %>% mutate(Row=row_number())



b <- apply(gd, 1,which.min)

b5 <- apply(gd, 2,which.min)



data4=cbind(data2,b2%>%filter(OBJECTID< 15))
data5=data4 %>% filter(b < 100 & !LON %in% LON[duplicated(LON)])

newa=data.frame(a) %>% mutate(row=row_number())
newa5=newa %>% filter(distance < 500 & !lon %in% lon[duplicated(lon)])
newa500=newa %>% filter(distance < 500)
newa1000=newa %>% filter(distance < 1000)
#################################################
plot( makeLine(Combined), type='l')
points(Combined)
points(a, col='blue', pch=20)
points(a[,2], a[,3], col='red', pch='x')
for (i in 1:nrow(a)) lines(geosphere::gcIntermediate(data3[i,], a[i,2:3], 10), lwd=2)



# Get the NHD (USA ONLY)
NHD <- get_nhd(template = Tidewater, 
               label = "Waters")

# Plot the NED again
raster::plot(NED)
# Plot the NHD data
NHD %>%
  lapply(sp::plot,
         col = 'black',
         add = TRUE)