#Ecometrics, KZOO BSA data
#***********start over with better parcel data w/ CT/Neighb***********
#PARCELS
#assessing parcel data (incomplete - taxable parcels only)
parcels <- read.csv(file = "2016Parcels.csv", head = T, sep = ",")
names(parcels)
#parcel data, all parcels in city
parcels2 <- read.csv(file = "ParcelList_1.csv", head = T, sep = ",")

parcels2 <- read.csv(file = "parcelsCTNeighb.csv", head = T, sep = ",")
names(parcels2)
dim(parcels2)
View(parcels2)




#keep pnum, total acres, ownercity, taxpayercity, propclass, mborsev, mbortax, zoning
#resbyrbuilt and cibyearbuilt
keepcols <- c(1, 2, 8, 9, 15, 16, 18, 19, 20, 21, 22:25, 29:30)
parcels <- parcels[,keepcols]




#complaints
complaints <- read.csv(file = "ComplaintTrackingnt2.csv", head = T, sep = ",")
Joinp <- read.csv(file = "EnforcementListREPORT.csv", head = T, sep = ",")
names(complaints)
dim(complaints)
names(Joinp)
dim(Joinp)

complaints <- merge(complaints, Joinp, by.x = "Case..", by.y = "Enforcement..", all.x = T)
names(complaints)
keepcols <- c(1, 3:10, 14)
complaints <- complaints[,keepcols]
View(complaints)

#join in parcel data (both assessing - incomplete and all parcel)
#to keep all records
compparcall <- merge(complaints, parcels2, by.x = "Address.y", by.y = "SITE_ADDRE", all.x = T, na.rm = T)

#to keep only complaints /enforcement where there is an address match (43,000 out of 73,000)
compparc <- merge(complaints, parcels2, by.x = "Address.y", by.y = "SITE_ADDRE", all.x.y = T, na.rm = T)
dim(compparc)
names(compparc)
compparc2 <- merge(compparc, parcels, by.x = "PIN", by.y = "Parcels.pnum", all.x = T)
names(compparc2)
dim(compparc2)
View(compparc2)

#remove NAs
compparc2 <- compparc2[!(is.na(compparc2$Date.Closed) | compparc2$Date.Closed == ""), ]
dim(compparc2)




#permits
permits2 <- read.csv(file = "PermitList.csv", head = T, sep = ",")
names(permits2)
dim(permits2)

#merge all parcels first
#to keep all cases
permparcall <- merge(permits2, parcels2, by.x = "Address", by.y = "SITE_ADDRE", all.x = T, na.rm = T)

#to keep only cases where address match is possible (32,000 out of 58,000)
permparc2 <- merge(permits2, parcels2, by.x = "Address", by.y = "SITE_ADDRE", all.x.y = T, na.rm = T)
names(permparc2)

#merge additional parcel data
permparc3 <- merge(permparc2, parcels, by.x = "PIN", by.y = "Parcels.pnum", all.x = T)
dim(permparc3)
names(permparc3)

#Remove NAs for date issued
permparc3 <- permparc3[!(is.na(permparc3$Date.Issued) | permparc3$Date.Issued == ""), ]
dim(permparc3)
View(permparc3)





#ANALYSIS
#complaints
#Develop variables for analysis
#Opened date --> Year, Month, Days open (closed - open)
#Ownership --> in Kalamazoo = 1, not = 0
#Ownership --> in MI = 1, not = 0

#ownership
compparc2$OwnerKzoo <- ifelse(compparc2$ParcelMaster.ownercity == "KALAMAZOO", 1, 0)
compparc2$OwnerMI <- ifelse(compparc2$ParcelMaster.ownerstate == "MI", 1, 0)

#Open date
require(reshape2)
require(stringr)
opendate <- data.frame(do.call('rbind', strsplit(as.character(compparc2$Date.Filed), '/', fixed = T)))

names(opendate)[1] <- "openM"
names(opendate)[2] <- "openD"
names(opendate)[3] <- "openY"

opendate$openM <- as.integer(opendate$openM)
opendate$openD <- as.integer(opendate$openD)


compparc2 <- cbind(compparc2, opendate)
names(compparc2)

#now close date
closedate <- data.frame(do.call('rbind', strsplit(as.character(compparc2$Date.Closed), '/', fixed = T)))
dim(closedate)
names(closedate)[1] <- "closeM"
names(closedate)[2] <- "closeD"
names(closedate)[3] <- "closeY"

closedate$closeM <- as.integer(closedate$closeM)
closedate$closeD <- as.integer(closedate$closeD)

compparc2 <- cbind(compparc2, closedate)
names(compparc2)
dim(compparc2)

#number of days open
compparc2$daysopen <- as.Date(as.character(compparc2$Date.Closed), format = "%m/%d/%y") - as.Date(as.character(compparc2$Date.Filed), format = "%m/%d/%y")

#remove any with days open < 0 (i.e., this would be an error)
compparc2 <- subset(compparc2, compparc2$daysopen >=0)
dim(compparc2)
View(compparc2)


#recategorize
aggbytype <- aggregate(openY ~ Category, data = compparc2, FUN = length)
View(aggbytype)


compparc2$newcat <- ifelse(compparc2$Category == "CURB LAWN TRASH", "Curb Lawn Trash",
                           ifelse(compparc2$Category == "WEED", "Weeds",
                                  ifelse(compparc2$Category == "PRIVATE PROPERTY", "Misc Priv Property",
                                         ifelse(compparc2$Category == "CHP 22", "Private Nuisance",
                                                ifelse(compparc2$Category == "CHP 22 - IMMED REF", "Private Nuisance",
                                                       ifelse(compparc2$Category == "CHP 22 - Immed. Ref", "Private Nuisance",
                                                              ifelse(compparc2$Category == "HOUSING", "Housing",
                                                                     ifelse(compparc2$Category == "Service Request NH", "Service Request",
                                                                            ifelse(compparc2$Category == "ZONING", "Zoning",
                                                                                   ifelse(compparc2$Category == "ABANDON STRUCTURE", "Vacant Structure",
                                                                                          ifelse(compparc2$Category == "Abandon Structures", "Vacant Structure",
                                                                                                 ifelse(compparc2$Category == "VACANT STRUCTURE", "Vacant Structure",
                                                                                                        ifelse(compparc2$Category == "ANTI-BLIGHT", "Anti Blight",
                                                                                                               ifelse(compparc2$Category == "Chapter 15a", "Fire Hazard",
                                                                                                                      ifelse(compparc2$Category == "OCCUPIED", "Occupied",
                                                                                                                             ifelse(compparc2$Category == "CHP 33", "Public Nuisance",
                                                                                                                                    ifelse(compparc2$Category == "CHP 9", "Bldg Regulation", 
                                                                                                                                           ifelse(compparc2$Category == "Curb Lawn Trash", "Curb Lawn Trash",
                                                                                                                                                  ifelse(compparc2$Category == "Zoning", "Zoning",
                                                                                                                                                         ifelse(compparc2$Category == "Service Request", "Service Request",
                                                                                                                                                                ifelse(compparc2$Category == "CHP 9 COMM-IND", "Bldg Regulation",
                                                                                                                                                                       ifelse(compparc2$Category == "CHP 9 STOP WORK", "Bldg Regulation",
                                                                                                                                                                              "Other"))))))))))))))))))))))
View(compparc2)

#NEW
aggbytype2 <- aggregate(openY ~ newcat, data = compparc2, FUN = length)
View(aggbytype2)

#aggregate complaints per parcel
aggbyparcel <- aggregate(openY ~ PIN, data = compparc2, FUN = length)
View(aggbyparcel)

#New variable External Nuisance (Minor) : Weed, Lawn/Curb Trash, Public Nuisance
#External Nuisance (Major): Vacant Structure, Anti-Blight
#Internal Nuisance : Private Nuisance, Fire Hazard, Bldg Regulation Complaint

compparc2$ExtNuisMin <- ifelse(compparc2$newcat == "Curb Lawn Trash" | compparc2$newcat == "Weeds" | compparc2$newcat == "Public Nuisance", 1, 0)
compparc2$ExtNuisMaj <- ifelse(compparc2$newcat == "Vacant Structure" | compparc2$newcat == "Anti Blight", 1, 0)
compparc2$IntNuis <- ifelse(compparc2$newcat == "Private Nuisance" | compparc2$newcat == "Fire Hazard" | compparc2$newcat == "Bldg Regulation", 1, 0)


View(compparc2)




#graphing
require(ggplot2)
names(compparc2)

#plot number of complaints by year and month
ggplot(compparc2, aes(openY, fill = openM)) + geom_bar() + labs(x = "Year", y = "# of Complaints", title = "Number of Complaints by Year and Month") + scale_fill_discrete(name = "Month") + theme(plot.title = element_text(hjust = .5))

#plot number of complaints 
#for complaints w > 0 days open
ggplot(data = subset(compparc2, daysopen !=0), aes(x = daysopen)) + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  xlim(0,100)+ labs(x = "Days Open", y = "# of Complaints", title = "Number of Complaints by Days Open") + theme(plot.title = element_text(hjust = .5))
#for all complaints
ggplot(compparc2, aes(x = daysopen)) + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  xlim(-1,100)

#summary stats test
library(plyr)
cdat <- ddply(subset(compparc2, daysopen != 0), "openM", summarise, rating.mean = mean(daysopen))
cdat

#for all complaints by month days open
ggplot(data = subset(compparc2, daysopen !=0), aes(x = daysopen)) + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  xlim(0,50) + facet_grid(openM ~ .) 

ggplot(data = subset(compparc2, daysopen != 0), aes(x = openM, y = daysopen)) + 
  geom_boxplot() + coord_flip() + ylim(0, 60) + labs(x = "Month", y = "Days Open", title = "Distribution of Days Open By Month") + theme(plot.title = element_text(hjust = .5))


#by complaint type
ggplot(compparc2, aes(openY, fill = newcat)) + geom_bar() + 
  labs(x = "Year", y = "# of Complaints", title = "Number of Complaints by Type") + theme(plot.title = element_text(hjust = .5))


ggplot(data = subset(compparc2, daysopen !=0), aes(x = daysopen)) + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  xlim(0,50) + facet_grid(openM ~ .) 


ggplot(data = subset(compparc2, daysopen !=0), aes(x = daysopen)) + geom_histogram(binwidth = 1, colour = "black", fill = "white") +
  xlim(0,50) + facet_grid(newcat ~ .) 


ggplot(data = subset(compparc2, daysopen != 0), aes(x = newcat, y = daysopen)) + 
  geom_boxplot() + coord_flip() + ylim(0, 200) + labs(x = "Days Open", y = "Categories", title = "Days Open by Category") + theme(plot.title = element_text(hjust = .5))

#by neighb
ggplot(data = subset(compparc2, NAME != 'NA'), aes(openY, fill = NAME)) + geom_bar() + labs(x = "Year Issued", y = "# of Permits")

#wrapped by neighb
ggplot(data = subset(compparc2, NAME != 'NA'), aes(x = openY)) + geom_bar() + facet_wrap(~ NAME, ncol = 4)

#wrapped by zoning
ggplot(data = subset(compparc2, ParcelMaster.zoning != 'NA'), aes(x = openY)) + geom_bar() + facet_wrap(~ ParcelMaster.zoning, ncol = 4)

ggplot(data = subset(compparc2, ParcelMaster.zoning != 'NA'), aes(openY, fill = ParcelMaster.zoning)) + geom_bar() + labs(x = "Year Issued", y = "# of Permits")





#PERMITS
names(permparc3)
View(permparc3)

#ownership kzoo & MI
permparc3$OwnerKzoo <- ifelse(permparc3$ParcelMaster.ownercity == "KALAMAZOO", 1, 0)
permparc3$OwnerMI <- ifelse(permparc3$ParcelMaster.ownerstate == "MI", 1, 0)

#aggregate by category
aggbycat <- aggregate(PIN ~ Category, data = permparc3, FUN = length)
View(aggbycat)


#newcategories

permparc3$newcat <- ifelse(permparc3$Category == "Electrical", "Electrical", ifelse(permparc3$Category == "Mechanical", "Mechanical", ifelse(permparc3$Category == "Plumbing", "Plumbing", ifelse(permparc3$Category == "Residential Roof", "Ext Renovation Res", ifelse(permparc3$Category == "Sign", "Sign", ifelse(permparc3$Category == "Residential, Miscellaneous", "Misc Res", ifelse(permparc3$Category == "Paving", "Site Work", ifelse(permparc3$Category == "Commercial,Interior Alteration", "Int Renovation Comm", ifelse(permparc3$Category == "Technical Code Inspection", "Code Inspection", ifelse(permparc3$Category == "Demo Residential Dwelling", "Demo Res", ifelse(permparc3$Category == "Residential, Interior Remodel", "Int Renovation Res", ifelse(permparc3$Category == "MECHANICAL", "Mechanical", ifelse(permparc3$Category == "Residential, Siding", "Ext Renovation Res", ifelse(permparc3$Category == "Residential, Addition", "Addition Res", ifelse(permparc3$Category == "Commercial, Miscellaneous", "Misc Comm", ifelse(permparc3$Category == "PLANNING/ZONING", "Prep Work", ifelse(permparc3$Category == "Temporary Structure", "Temporary Structure", ifelse(permparc3$Category == "Demo Commercial Structure", "Demo Comm", ifelse(permparc3$Category == "Residential, New Home", "New Res", ifelse(permparc3$Category == "Res, Alteration", "Misc Res", ifelse(permparc3$Category == "Commercial, New Structure", "New Comm",  ifelse(permparc3$Category == "Residential, Garage (detached)", "Addition Res", ifelse(permparc3$Category == "Demo Residential Accessory", "Demo Misc", ifelse(permparc3$Category == "Environmental", "Site Work", ifelse(permparc3$Category == "Commercial, Addition", "Addition Comm", ifelse(permparc3$Category == "Site Plan Prints", "Prep Work", ifelse(permparc3$Category == "Commercial, Roofing", "Ext Renovation Comm", ifelse(permparc3$Category == "Residential, Reroof & Residing", "Ext Renovation Res", ifelse(permparc3$Category == "Residential, Deck", "Ext Renovation Res", ifelse(permparc3$Category == "Demolition", "Demo Misc", ifelse(permparc3$Category == "Demo Residential Accessory", "Demo Misc", ifelse(permparc3$Category == "Com, Alteration", "Misc Comm", ifelse(permparc3$Category == "Residential, Garage", "Addition Res", ifelse(permparc3$Category == "Soil Erosion", "Site Work", ifelse(permparc3$Category == "Res, Miscellaneous", "Misc Res", ifelse(permparc3$Category == "Comm, Multi-Family New Bld.", "New Comm", ifelse(permparc3$Category == "Res, Swimming Pool", "Ext Renovation Res", ifelse(permparc3$Category == "Residential, Mobile Home Set", "New Res", ifelse(permparc3$Category == "Residential,Interior Remodel", "Int Renovation Res", ifelse(permparc3$Category == "Residential, Garade (attached)", "Addition Res", ifelse(permparc3$Category == "Comm, Multi-Family Alteration", "Misc Comm", ifelse(permparc3$Category == "Communications Tower or Equip", "Site Work", ifelse(permparc3$Category == "Residential, Pole Barn", "Addition Res", ifelse(permparc3$Category == "Res, Garage", "Addition Res", ifelse(permparc3$Category == "Commercial foundation only", "Site Work", ifelse(permparc3$Category == "Demo Res. Other Than a Bld.", "Demo Misc", ifelse(permparc3$Category == "Res, New Home", "New Res", ifelse(permparc3$Category == "Residential, New modular home", "New Res", ifelse(permparc3$Category == "Com, New Structure", "New Comm", ifelse(permparc3$Category == "Residential, Multi-family new", "New Res", "Other"))))))))))))))))))))))))))))))))))))))))))))))))))

#view new categories
aggbycat2 <- aggregate(PIN ~ newcat, data = permparc3, FUN = length)
View(aggbycat2)

#date issued

dateissued <- data.frame(do.call('rbind', strsplit(as.character(permparc3$Date.Issued), '/', fixed = T)))

names(dateissued)[1] <- "issuedM"
names(dateissued)[2] <- "issuedD"
names(dateissued)[3] <- "issuedY"

dateissued$issuedM <- as.integer(dateissued$issuedM)
dateissued$issuedD <- as.integer(dateissued$issuedD)

permparc3 <- cbind(permparc3, dateissued)
names(permparc3)
View(permparc3)

#remove where year issued is 1996 or 2113 (almost no permits for 96, obvious error for 2113)
permparc3 <- subset(permparc3, permparc3$issuedY != 1996 & permparc3$issuedY != 2113)

#aggregate permits per parcel
aggpermbyparcel <- aggregate(issuedY ~ PIN, data = permparc3, FUN = length)
View(aggpermbyparcel)


#NEW VARIABLES CATEGORICAL
#Investment Major = New construction and demolition (non -misc)
#Investment Minor = Addition / Renovation, elec, mech, plumbing
#Local Investment = Permit sought for structure that is owned by kzoo resident/ MI resident
#for local investment, will aggregate as % by neighb / ct boundaries

permparc3$InvestMajor <- ifelse(permparc3$newcat == "Demo Comm" | permparc3$newcat == "Demo Res" | permparc3$newcat == "New Comm" | permparc3$newcat == "New Res", 1, 0)
permparc3$InvestMinor <- ifelse(permparc3$newcat == "Addition Comm" | permparc3$newcat == "Addition Res" | permparc3$newcat == "Ext Renovation Res" | permparc3$newcat == "Ext Renovation Comm" | permparc3$newcat == "Int Renovation Res" | permparc3$newcat == "Int Renovation Comm" | permparc3$newcat == "Mechanical" | permparc3$newcat == "Electrical" | permparc3$newcat == "Plumbing", 1, 0)

View(permparc3)




#GRAPHING
permparc3$issuedM <- as.factor(permparc3$issuedM)
#number of permits by year and month
ggplot(permparc3, aes(issuedY, fill = issuedM)) + geom_bar() + 
  labs(x = "Year Issued", y = "# of Permits", title = "Number of Permits Issued by Year and Month") + theme(plot.title = element_text(hjust = .5)) + 
  scale_fill_discrete(name = "Month")

ggplot(data = subset(permparc3, issuedY != 1997), aes(issuedM)) + geom_bar() + facet_wrap(~ issuedY) +
  labs(x = "Month Issued", y = "# of Permits", title = "Number of Permits Issued by Year and Month") + theme(plot.title = element_text(hjust = .5))



#by newcat
ggplot(permparc3, aes(issuedY, fill = newcat)) + geom_bar() + 
  labs(x = "Year Issued", y = "# of Permits", title = "Number of Permits Issued by Year and Type") + theme(plot.title = element_text(hjust = .5)) +
  scale_fill_discrete(name = "Category")

#wrapped by newcat by year
ggplot(permparc3, aes(x = issuedY)) + geom_bar() + facet_wrap(~ newcat, ncol = 5)
#same by month
ggplot(permparc3, aes(x = issuedM)) + geom_bar() + facet_wrap(~ newcat, ncol = 5)

ggplot(permparc3, aes(issuedM, fill = newcat)) + geom_bar() + 
  labs(x = "Month Issued", y = "# of Permits", title = "Number of Permits Issued by Month and Type") + theme(plot.title = element_text(hjust = .5)) +
  scale_fill_discrete(name = "Category")




#by neighb
ggplot(data = subset(permparc3, NAME != 'NA'), aes(issuedY, fill = NAME)) + geom_bar() + labs(x = "Year Issued", y = "# of Permits")

#wrapped by neighb
ggplot(data = subset(permparc3, NAME != 'NA'), aes(x = issuedY)) + geom_bar() + facet_wrap(~ NAME, ncol = 4) + 
  labs(x = "Year Issued", y = "# of Permits", title = "# of Permits by Neighborhood") + theme(plot.title = element_text(hjust = .5)) + scale_x_discrete(breaks = levels(permparc3$issuedY)[c(T, rep(F, 3))])

#wrapped by zoning
ggplot(data = subset(permparc3, ParcelMaster.zoning != 'NA'), aes(x = issuedY)) + geom_bar() + facet_wrap(~ ParcelMaster.zoning, ncol = 4)







names(permparc3)
View(permparc3)
#AGGREGATE ANALYSIS
require(sqldf)

#NEIGHB
#pull permit data by neighb
permneighb <- sqldf("select NAME, count(Address) as NumPermits,
                    sum(InvestMajor) as MajorProjects,
                    sum(InvestMinor) as MinorProjects
                   from permparc3 where NAME != 'NA' group by NAME")

#pull parcel data by neighb, such as num parcels, 


#join


keeprows <- c(3:23)
permneighb <- permneighb[3:23,]
View(permneighb)

#TRACT
permtract <- sqldf("select TRACT, count(Address) as NumPermits,
                    sum(InvestMajor) as MajorProjects,
                    sum(InvestMinor) as MinorProjects
                   from permparc3 where TRACT != 'NA' group by TRACT")


View(permtract)

View(compparc2)

comptract <- sqldf("select TRACT, count(newcat) as NumComplaints,
                   sum(daysopen) / count(daysopen) as AvgDaysOpen,
                   sum(ExtNuisMaj) as ExtMajorNuisance,
                   sum(ExtNuisMin) as ExtMinorNuisance,
                   sum(IntNuis) as InteriorNuisance from compparc2 where TRACT != 'NA' group by TRACT")


View(comptract)


tractdata <- merge(permtract, comptract, by = "TRACT", all.x.y. = T)



#GRAPHING AGGREGATE DATA
ggplot(tractdata, aes(x = ))







#MAPPING
require(rgdal)
require(sp)
require(ggplot2)
require(ggmap)
tracts_kzoo <- readOGR(dsn = ".", "Tracts")
hoods_kzoo <- readOGR(dsn = ".", "City_Neighborhoods")

proj4string(tracts_kzoo)
tracts_kzoo = spTransform(tracts_kzoo, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

names(tracts_kzoo)
names(hoods_kzoo)

tracts_kzoo <- fortify(tracts_kzoo, region = "TRCT_KEY")
hoods_kzoo <- fortify(hoods_kzoo, region = "NAME")


hoods_kzoo <- merge(hoods_kzoo, permneighb, by.x = 'id', by.y = 'NAME', all.x = T)
hoods_kzoo <- hoods_kzoo[order(hoods_kzoo$order),]
View(hoods_kzoo)

tracts_kzoo <- merge(tracts_kzoo, tractdata, by.x = 'id', by.y = 'TRACT', all.x = T)
tracts_kzoo <- tracts_kzoo[order(tracts_kzoo$order),]
View(tracts_kzoo)

#KZOO BASE
kzoo<-get_map(location=c(left = -85.71350, bottom = 42.22343, right = -85.47947, top = 42.34078))
kzoo<-ggmap(kzoo)
kzoo

#PERMITS
numperm <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = NumPermits), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Permits (1998-2016)')
numperm

majorinvest <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = MajorProjects), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Major Investment Permits (1998-2016)\n -- based on # of New Construction and \n Demolition permits issued')
majorinvest

minorinvest <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = MinorProjects), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Minor Investment Permits (1998-2016)\n -- based on # of electrical, mechanical, \n plumbing, renovation, and addition \n permits issued')
minorinvest


#COMPLAINTS
numcomp <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = NumComplaints), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Complaints (1998-2016)')
numcomp

avgdaysopencomp <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = AvgDaysOpen), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "green", high = "red")+labs(fill='Avg. Days Open for Complaints (1998-2016)')
avgdaysopencomp

compextmin <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = ExtMinorNuisance), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "green", high = "red")+labs(fill='# Complaints, Minor Exterior Nuisance (1998-2016)')
compextmin

compextmaj <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = ExtMajorNuisance), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "green", high = "red")+labs(fill='# Complaints, Major Exterior Nuisance (1998-2016)')
compextmaj

compintnuis <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = InteriorNuisance), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "green", high = "red")+labs(fill='# Complaints, Private (Interior) Nuisance (1998-2016)')
compintnuis



