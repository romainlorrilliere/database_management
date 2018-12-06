### import huge file in posgresql database


vecPackage=c("RODBC","reshape2","data.table","lubridate","doBy","devtools","stringr","dplyr","ggplot2","RPostgreSQL")
ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)

library(RODBC)
library(reshape2)
library(data.table)
library(lubridate)
library(doBy)
library(devtools)
library(stringr)
library(dplyr)
library(RPostgreSQL)


myshell <- function(mycmd,myinvisible=TRUE) {
    is.windows <- Sys.info()["sysname"] == "Windows"

    cat("System command: ",mycmd,"\n",paste="")
    if(is.windows){
        cat("     - OS: Windows \n")
        shell(cmd=mycmd,invisible=myinvisible)
    }else{
        cat("     - OS: Linux alike \n")
        system2(mycmd)
    }
}




import_psql <- function(file="testAmphi2.csv",vecSep=c(",",";","\t"),pathData="rawData",pathSQL= "sql",changeColNames=c("collaborative_australian_protected_areas_database__capad__marine_2010"="capad__marine_2010","collaborative_australian_protected_areas_database__capad__marine_2010_1"="capad__marine_2010_1"), excludedColumn=NULL,#"taxon_identification_issue",
                        fileSQL= "_postgres_import_data.sql",
                        db.name="harmony",db.user="postgres",
                        tableName="amphibians",pkField="record_id",
                        realField=c("decimallatitude","decimallongitude"),intField=NULL,booleanField=NULL,
                        toDoLiteTable=FALSE,litetableName="alalite",
                        keepedCol=NULL,litetableColType=c("f"="varchar(20)","e"="real"),
                        vecIndex=NULL,
                        createGeom=FALSE,geomName="geom",epsgRaw="",epsgGeom=NULL,geom.lonlat=c("lon","lat")) {


    ## ---- initializing parameters for debugging ----
    ## file="aaa.csv"
    ## pathSQL= "sql/"; fileSQL= "_postgres_import_data.sql";   tableName= "ala"
    ## pkField="a";realField="d";intField="e";booleanField=NULL
    ## tableName;toDoLiteTable=FALSE;litetableName="alalite";keepedCol=c("a","d","e","f");colType=NULL;
    ## geomName="geom";epsgRaw=NULL;epsgGeom=NULL;geom.lonlat=c("lon","lat")
    ## dbname="bullshit";dbusername="annechristine"
    ## -------------------------------------------------------


    fileSQL.path <- paste(pathSQL,"/",fileSQL,sep="")

    if(is.null(pathData)) pathDataFull <- paste(getwd()) else pathDataFull <- paste(getwd(),"/",pathData,"/",sep="")

    pathFile <- paste(pathDataFull,file,sep="")

    flag <- TRUE
    i <- 0
    while(flag){
        i <- i+1
        theSeparator <- vecSep[i]
        d <- read.delim(pathFile, nrows = 2,sep=theSeparator,header=TRUE)
        flag <- ncol(d)<2
    }
    h <- colnames(d)
    h <- tolower(h)
    #browser()
    h <- gsub(".","_",h,fixed =TRUE)
    h[h=="class"] <- "classe"
    h[h=="order"] <- "ordre"

    if(!is.null(excludedColumn))
        h <- h[!(h%in%excludedColumn)]

    for(i in 1:length(changeColNames))
                            h[h==names(changeColNames)[1]] <- changeColNames[1]


    cat("\n",length(h)," columns founded with the separator:",theSeparator,"\n")

    head <- paste("DROP table if exists ",tableName,";\nCREATE TABLE ",tableName,"\n(",sep="")

    end <- ");\n"

#browser()


    field <- paste(paste(h,ifelse(h %in% realField," real",
                     ifelse(h %in% intField," int",
                     ifelse(h %in% booleanField," boolean"," varchar(250)"))),
                    ifelse(h %in% pkField," primary key",""),",\n",sep=""),
                   collapse="")
    field <- substr(field,1,nchar(field)-2)

    create <- paste(head,field,end,collapse="")


    if(fileSQL%in% dir(pathSQL)) file.remove(fileSQL.path)

    cat(create,file=fileSQL.path,append=TRUE)

    cat("\n\n\ \\copy ",tableName,ifelse(!is.null(excludedColumn),paste("(",paste(h,collapse=","),")",sep=""),"")," FROM '",pathFile,"' with (format csv, header, delimiter '",theSeparator,"',ESCAPE '\"', null '',ENCODING 'UTF-8')\n",sep="",file=fileSQL.path,append=TRUE)

    if(toDoLiteTable) {

        query <- paste("\n\nDROP TABLE IF EXISTS ",litetableName,
                       ";\nCREATE TABLE ",litetableName," as (\nSELECT ",
                       paste(keepedCol,collapse=",")," from ",tableName,");\n")
        cat(query,file=fileSQL.path,append=TRUE)
        if(!is.null(litetableColType)) {
            query <- paste("\n",paste("ALTER TABLE ",litetableName," ALTER COLUMN ",names(litetableColType)," TYPE ",litetableColType,";\n"),collapse="")
            cat(query,file=fileSQL.path,append=TRUE)
        }
    }

    if(createGeom) {
        if(is.null(epsgRaw)) epsgRaw <- "4326"
        if(is.null(epsgRaw)) epsgGeom <- epsgRaw

        query <- paste("ALTER TABLE ",ifelse(toDoLiteTable,litetableName,tableName)," ADD COLUMN ",geomName," geometry(Point,",epsgGeom,");\nUPDATE ",ifelse(toDoLiteTable,litetableName,tableName),"\nSET ",geomName," = ",ifelse(epsgRaw!=epsgGeom,"ST_Transform(",""),"ST_SetSRID(ST_MakePoint(", geom.lonlat[1],",", geom.lonlat[2],"),",epsgRaw,")",ifelse(epsgRaw!=epsgGeom,",2154)",""),";\n",sep="")
        cat(query,file=fileSQL.path,append=TRUE)
    }


    if(!is.null(vecIndex)){
        query <- paste(paste("CREATE INDEX index_",vecIndex," ON ",ifelse(toDoLiteTable,litetableName,tableName),"(vecIndex);\n",sep=""),collapse="")
        cat("\n",query,file=fileSQL.path,append=TRUE)
    }

    pathSQLfull <- paste(getwd(),"/",pathSQL,"/",sep="")
    cmd_import <- paste("psql -U ",db.user," ",db.name," < ",pathSQLfull,"_postgres_import_data.sql",sep="")
    myshell(cmd_import)


}


