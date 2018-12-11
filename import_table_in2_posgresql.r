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




psql_import_table <- function(file="testAmphi2.csv",
                              vecSep=c("\t",";",","),rawEncoding="UTF-8",
                              pathData="rawData",pathSQL= "sql",
                              doChangeChar=TRUE,
                              changeCharInRaw=list(c("\\\\\\\\\"\"",""),c("\\\",\\\"","\\\";\\\""),c(","," "),c("\"\"","")),#c(","," "),c("\"\"",""),,c("\'\'"," ")
                              changeColNames=c("collaborative_australian_protected_areas_database__capad__marine_2010"="capad__marine_2010",
                                               "collaborative_australian_protected_areas_database__capad__marine_2010_1"="capad__marine_2010_1"),
                              excludedColumn=NULL,#"taxon_identification_issue",
                        fileSQL= "_postgres_import_data.sql",
                        db.name="harmony",db.user="postgres",
                        tableName="amphibians",pkField="record_id",
                        realField=c("decimallatitude","decimallongitude"),intField=NULL,booleanField=NULL,
                        toDoLiteTable=FALSE,litetableName="alalite",
                        keepedCol=NULL,litetableColType=c("f"="varchar(20)","e"="real"),
                        vecIndex=NULL,
                        createGeom=FALSE,geomName=NULL,epsgRaw="4326",epsgGeom=NULL,geom.lonlat=c("decimallongitude","decimallatitude")) {


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

    cat("\n Table importation in psql database\n======================================\n\n")

   if(doChangeChar) {
        cat("\nChange some character directly into raw file:\n--------------------------------------------\n\n")
        is.windows <- Sys.info()["sysname"] == "Windows"
        if(is.windows) {
    for(i in 1:length(changeCharInRaw)) {
        vchange <- changeCharInRaw[[i]]

        cat("   change:",vchange[1],"->",vchange[2],"\n")

        cmd <- paste("powershell -Command \"(gc ",pathFile,") -replace '",vchange[1],"', '",vchange[2],"' | Out-File ",pathFile," -encoding 'ASCII'\"",sep="")
        myshell(cmd)
    }
        } else {
            stop("!!! This procedure has not yet done for UNIX OS :'-( !!!\n")
        }



    }


        cat("\nSummary:\n-------------\n\n")

    cat(" psql   [",tableName,"] <- ",pathFile,"\n",sep="")

    flag <- TRUE
    i <- 0
    while(flag){
        i <- i+1
        theSeparator <- vecSep[i]
        d <- read.delim(pathFile, nrows = 2,sep=theSeparator,header=TRUE,encoding=rawEncoding,skipNul=FALSE)
        flag <- ncol(d)<2
    }

    h <- colnames(d)
    h <- tolower(h)




    cat("\n",length(h),"columns detected with the separator:",theSeparator,"\n\n")




    cat("\nFixing some pb in data header:\n----------------------\n\n")

    whereRsep <- grep(theSeparator,h,fixed=TRUE)
    if(length(whereRsep)>0) {
        cat("",length(whereRsep)," separator character detected in colnames have to be changed \n ",whereRsep," -> _ \n\n",sep="")
        h <- gsub(theSeparator,"_",h,fixed =TRUE)
        }

    whereRdot <- grep(".",h,fixed=TRUE)
    if(length(whereRdot)>0) {
        cat("",length(whereRdot)," dot(s) detected have to be changed \n . -> _ \n\n",sep="")
        h <- gsub(".","_",h,fixed =TRUE)
        }


    whereRcomma <- grep(",",h,fixed=TRUE)
    if(length(whereRcomma)>0) {
        cat("",length(whereRcomma)," comma(s) detected have to be changed \n , -> _ \n\n",sep="")
        h <- gsub(",","_",h,fixed =TRUE)
        }


    whereRsemicolon <- grep(";",h,fixed=TRUE)
    if(length(whereRsemicolon)>0) {
        cat("",length(whereRsemicolon)," semicolon(s) detected have to be changed \n ; -> _ \n\n",sep="")
        h <- gsub(";","_",h,fixed =TRUE)
        }


    if(!is.null(excludedColumn)) {
        cat("Excluding",length(excludedColumn),"column(s)\n")
        h <- h[!(h%in%excludedColumn)]
        }
    if(!is.null(changeColNames)) {
        cat("Changing",length(changeColNames),"column names\n")
        for(i in 1:length(changeColNames))
            h[h==names(changeColNames)[1]] <- changeColNames[1]

    }



    if("class" %in% h) {
        cat("changing colname class to classe\n")
        h[h=="class"] <- "classe"
    }
    if("order" %in% h) {
        cat("changing colname order to ordre\n")
        h[h=="order"] <- "ordre"
    }




    head <- paste("DROP table if exists ",tableName,";\nCREATE TABLE ",tableName,"\n(",sep="")

    end <- ");\n"



    field <- paste(paste(h,ifelse(h %in% realField," real",
                     ifelse(h %in% intField," int",
                     ifelse(h %in% booleanField," boolean"," varchar(250)"))),
                    ifelse(h %in% pkField," primary key",""),",\n",sep=""),
                   collapse="")
    field <- substr(field,1,nchar(field)-2)

    create <- paste(head,field,end,collapse="")



    if(fileSQL%in% dir(pathSQL)) file.remove(fileSQL.path)


    cat("\nThe queries:\n-------------\n\n")
    cat("   SQL file:",fileSQL.path,"\n\n")


    if(nchar(create)>1000) cat(substr(create,1,500),"\n[...CREATE query to long to be enterely showed...]\n",substr(create,nchar(create)-50,nchar(create)),"\n") else cat(create,"\n")
    cat(create,file=fileSQL.path,append=TRUE)

    copyQuery <- paste("\n\n\ \\copy ",tableName,ifelse(!is.null(excludedColumn),paste("(",paste(h,collapse=","),")",sep=""),"")," FROM '",pathFile,"' with (format csv, header, delimiter '",theSeparator,"', null '')\n",sep="")#,ESCAPE '\"',ENCODING '",rawEncoding," ,QUOTE E'\\b'
    cat(copyQuery)
    cat(copyQuery,file=fileSQL.path,append=TRUE)

    if(toDoLiteTable) {

        query <- paste("\n\nDROP TABLE IF EXISTS ",litetableName,
                       ";\nCREATE TABLE ",litetableName," as (\nSELECT ",
                       paste(keepedCol,collapse=",")," from ",tableName,");\n")
        cat(query)
        cat(query,file=fileSQL.path,append=TRUE)
        if(!is.null(litetableColType)) {
            query <- paste("\n",paste("ALTER TABLE ",litetableName," ALTER COLUMN ",names(litetableColType)," TYPE ",litetableColType,";\n"),collapse="")
            cat(query,file=fileSQL.path,append=TRUE)
        }
    }

    if(createGeom) {
        if(is.null(epsgGeom)) epsgGeom <- epsgRaw
        if(is.null(geomName)) geomName <- paste("geom",epsgGeom,sep="")

        query <- paste("\nALTER TABLE ",ifelse(toDoLiteTable,litetableName,tableName)," ADD COLUMN ",geomName," geometry(Point,",epsgGeom,");\nUPDATE ",ifelse(toDoLiteTable,litetableName,tableName),"\nSET ",geomName," = ",ifelse(epsgRaw!=epsgGeom,"ST_Transform(",""),"ST_SetSRID(ST_MakePoint(", geom.lonlat[1],",", geom.lonlat[2],"),",epsgRaw,")",ifelse(epsgRaw!=epsgGeom,",2154)",""),";\n",sep="")
        cat(query)
        cat(query,file=fileSQL.path,append=TRUE)
    }


    if(!is.null(vecIndex)){
        query <- paste("\n",paste("CREATE INDEX index_",vecIndex," ON ",ifelse(toDoLiteTable,litetableName,tableName),"(vecIndex);\n",sep=""),collapse="")
        cat(query)
        cat("\n",query,file=fileSQL.path,append=TRUE)
    }


    cat("\n==============================\n\n")
    pathSQLfull <- paste(getwd(),"/",pathSQL,"/",sep="")
    cmd_import <- paste("psql -U ",db.user," ",db.name," < ",pathSQLfull,"_postgres_import_data.sql",sep="")
    myshell(cmd_import)


}


