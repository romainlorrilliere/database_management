### import huge file in posgresql database


vecPackage=c("RODBC","reshape2","data.table","lubridate","doBy","devtools","stringr","dplyr","ggplot2","RPostgreSQL","sf")
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
library(sf)

#' myshell function : to send a command to the shell whatever the OS
#'
#' @param mycmd the system command to be invoked, as a string
#' @param myinvisible TRUE/FALSE to force silence mode
#'
#' @return NA
#' @export NA
#'
#' @examples myshell("dir")
#'
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




#' Title
#'
#' @param file the raw data file name
#' @param vecSep a vector of probable separator. In defaut the script test successively \t ; ,
#' @param rawEncoding encoding of raw data file, default value : UTF-8
#' @param pathData directory of data
#' @param pathSQL directory of sql file that will maked by the script (this directory need to be exist)
#' @param doChangeChar bolean value if you need to change some character directly in the raw file before the importation. This process could during a long time, so use it only if necessary, default FALSE
#' @param changeCharInRaw a list of pairs of character change in the raw file
#' For exemple:
#'      list(c("\\\\\\\\\"\"",""),c("\\\",\\\"","\\\";\\\""),c(","," "),c("\"\"",""),c(","," "),c("\"\"",""),c("\'\'"," "))
#' @param changeColNames colnames changes formated like that a vector each name of elements of the changeColNames vector is change to the value of the elements.
#' For exemple:
#'    c("nameInRawData1"="newName1","nameInRawData2"="newName2"...),
#' @param excludedColumn TRUE or FALSE maybe this option do not works
#' @param fileSQL name of the of the built SQL file
#' @param dbname  for the name of the database on the host
#' @param user for the user name (default: current user), in windows OS you probably have to define the user
#' @param tableName for the table name that will be created
#' @param pkField vector of column name for the field that define the primary key
#' @param realField vector of column name for the field that are real
#' @param intField  vector of column name for the field that are integer
#' @param booleanField  vector of column name for the field that are real
#' @param toDoLiteTable boolean, if you want built a liter table with a subset of column
#' @param litetableName the name of lite table, defaut value = paste(tableName,"_lite",sep="")
#' @param keepedCol vector of column keeped
#' @param litetableColType vector of type of column with the name to define the column name
#' Fore exemple:
#'     c("city"="varchar(20)","area"="real")
#' @param vecIndex vector to define the index you want built
#' @param createGeom boolean to add a geom field
#' @param geomName the geom name by defaut paste("geom",epsgGeom,sep=""
#' @param epsgRaw the epsg of input data
#' @param epsgGeom the epsg of geom by defaut epsgRaw
#' @param geom.lonlat vector of the name of field to construct the geom default c("longitude","latitude")
#'
#' @return NULL
#' @export
#'
#' @examples psql_import_table(file="amphi.csv",doChangeChar=TRUE,changeCharInRaw=c(c(",","_"),changeColNames=c("collaborative_australian_protected_areas_database__capad__marine_2010"="capad__marine_2010","collaborative_australian_protected_areas_database__capad__marine_2010_1"="capad__marine_2010_1"),dbname="harmony",tableName="amphibians",pkField="record_id",realField=c("decimallongitude","decimallatitude"),createGeom=TRUE,geom.lonlat=c("decimallongitude","decimallatitude"))
psql_import_table <- function(file,
                              vecSep=c("\t",";",","),rawEncoding="UTF-8",
                              pathData="rawData",pathSQL= "sql",
                              doChangeChar=FALSE,
                              changeCharInRaw=NULL,
                              changeColNames=NULL,
                              excludedColumn=NULL,
                              fileSQL= "_postgres_import_data.sql",
                              dbname=NULL,user="postgres",
                              tableName=NULL,pkField=NULL,
                              realField=c("decimallatitude","decimallongitude"),
                              intField=NULL,booleanField=NULL,
                              toDoLiteTable=FALSE,litetableName=paste(tableName,"_lite",sep=""),
                              keepedCol=NULL,litetableColType=NULL,
                              vecIndex=NULL,
                              createGeom=FALSE,geomName=paste("geom",epsgGeom,sep=""),
                              epsgRaw="4326",epsgGeom=epsgRaw,
                              geom.lonlat=c("longitude","latitude")) {


    ## ---- initializing of an example set of parameters for debugging ----
    ## file="aaa.csv"
    ## pathSQL= "sql/"; fileSQL= "_postgres_import_data.sql";   tableName= "ala"
    ## pkField="a";realField="d";intField="e";booleanField=NULL
    ## tableName;toDoLiteTable=FALSE;litetableName="alalite";keepedCol=c("a","d","e","f");colType=NULL;
    ## geomName="geom";epsgRaw=NULL;epsgGeom=NULL;geom.lonlat=c("lon","lat")
    ## dbname="bullshit"
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

                cmd <- paste("powershell -Command \"(gc ",pathFile,") -replace '",
                             vchange[1],"', '",vchange[2],"' | Out-File ",
                             pathFile," -encoding 'ASCII'\"",sep="")
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
        d <- read.delim(pathFile, nrows = 2,sep=theSeparator,
                        header=TRUE,encoding=rawEncoding,skipNul=FALSE)
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
                           ifelse(h %in% booleanField," boolean"," varchar"))),
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
        if(is.null(litetableName)) litetableName <- paste(tableName,"_lite",sep="")
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
    cmd_import <- paste("psql -U ",user," ",dbname," < ",pathSQLfull,"_postgres_import_data.sql",sep="")
    myshell(cmd_import)


}




#' Open a database connexion with a postgresql database in linux case the user and the password could be blank.
#'
#' @param DBname  for the name of the database on the host
#' @param user for the user name (default: current user), in windows OS you probably have to define the user
#' @param password for the password
#'
#' @return a database connexion
#'
#' @author R. Lorrilliere
#' @examples con <- openDB.PSQL(dbname,user,password)
#'
openDB.PSQL <- function(DBname=NULL,user=NULL,password=NULL){
    ## --- initializing parameters for debugging ----
                                        #DBname=birdlab;
                                        #user="romain" # windows
                                        #user = NULL # linux
                                        #  password=NULL
    ## ---

    library(RPostgreSQL)
    drv <- dbDriver("PostgreSQL")

     cat("DB psql connexion:",DBname,user,ifelse(is.null(password),"","****"),"\n")
                                        # In windows OS you probably have to define the user
    if(is.null(user)) {
        con <- dbConnect(drv, dbname=DBname)
    } else {
        con <- dbConnect(drv, dbname=DBname,user=user, password=password)
    }

    return(con)
}





#' Posgis extension intialisation,
#' this function have to do only one time by database
#'
#' @param con A DBIConnection object, as returned by openDB.PSQL() or more generaly by dbConnect().
#' @param dbname  for the name of the database on the host
#' @param user for the user name (default: current user), in windows OS you probably have to define the user
#' @param password for the password
#'
#' @return NULL
#'
#' @author R. Lorrilliere
#' @examples psql_posgis_init(dbname,user,password)
#'

psql_posgis_init <- function(con=NULL,dbname,user=NULL,password=NULL) {
    library(RPostgreSQL)

    if(is.null(con)) con <- openDB.PSQL(DBname=dbname,user=user,password=password)

    query <- paste("CREATE EXTENSION postgis;\n",
                   "CREATE EXTENSION postgis_topology;\n",
                   "CREATE SCHEMA IF NOT EXISTS spatial;\n",sep="")
    dbSendQuery(con,query)


}



#' Import ogr file in postgres posgis database.
#' This function manage the importation with ogr2ogr in terminal command and could be doesn't work if the Porsgres driver is not found.
#'
#' @param pathFile the path file you want to import
#' @param epsg the epsg defaut value 4326 correspond to WGS84
#' @param host for the host name (default: local connection)
#' @param dbname  for the name of the database on the host
#' @param user for the user name (default: current user), in windows OS you probably have to define the user
#' @param password for the password
#'
#' @return NULL
#'
#' @examples psql_import_ogr(dbname,user,password)
#'

psql_import_ogr <- function(pathFile,epsg="4326",
                            dbname="",user="",password="",
                            pgis.tableName="",
                            host="localhost") {

    cmd <- paste("ogr2ogr -f PostgreSQL -t_srs EPSG:",epsg," PG:\"host=",host," port=5432 dbname=",dbname," user=",user," password=",password,"\" ",pathFile," -nln public.",dbname," -nlt MULTIPOLYGON -overwrite -progress -unsetFid --config PG_USE_COPY YES",sep="")
    myshell(cmd)

}



#' Import shape file in postgres posgis database.
#' This function manage the importation with the sf library and start by charg the shape file in the R interface
#'
#' @param pathFile the path file you want to import
#' @param host for the host name (default: local connection)
#' @param dbname  for the name of the database on the host
#' @param user for the user name (default: current user), in windows OS you probably have to define the user
#' @param password for the password
#'
#' @return NULL
#'
#' @examples psql_posgis_init(dbname,user,password)
#'
psql_import_shp <- function(pathFile,
                            dbname="",user="",password="",
                            pgis.tableName="",
                            host="localhost",overwrite=TRUE) {
    library(sf)
    library(dplyr)
    st_read(pathFile) %>%
        st_write(paste("PG: host= ",host," dbname=",dbname," user=",user," password=",password," ",sep=""),pgis.tableName, layer_options=paste("OVERWRITE=",ifelse(overwrite,"true","false"),sep=""))

}





psql_import_shp_by_sql <- function(pathFile,
                                   host="localhost",dbname="",user="",
                                   pgis.tableName="",overwrite=TRUE) {

  path.shape<- paste(getwd(),"/shape/theia/",sep="")
    vecF <- dir(path.shape)                          #
    vecF <- vecF[grep("shp",vecF)]
    pathSQL <- paste(getwd(),"/sql/",sep="")
    vec.tableName <- NULL
    tabCol <- NULL

    if(toSQL){
        cat("\n\n --- Shape to SQL ---\n\n")
        for(f in vecF) {
            cat("\n\n - file: ",f,"\n")
            fsql <- paste(path.shape,"theia2016_",substr(f,13,14),".sql",sep="")
            cmd <- paste("shp2pgsql -I -s 2154  ",path.shape,f," >  ",fsql," \n",sep="")
            myshell(cmd,invisible=TRUE)
        }

    }
    if(import) {
        cat("\n\n --- Importation SQL ---\n\n")
        vec.tableName <- rep(NA,length(vecF))
        if(is.null(con)) con <- openDB.PSQL(user=nameUser,pw=pw)
        i <- 0
        for(f in vecF) {
            i <- i+1
            fsql <- paste(path.shape,"theia2016_",substr(f,13,14),".sql",sep="")
            name_table <- tail(readLines(fsql, n=4),1)
            name_table <- gsub("CREATE TABLE \"","",name_table)
            name_table <- gsub("\" (gid serial,","",name_table,fixed=TRUE)
            vec.tableName[i] <- name_table
            query <- paste("DROP TABLE IF EXISTS ",name_table,";")
            cat("drop query:", query,"\n")
            dbSendQuery(con, query)
            cat("\n\n - file: ",fsql,"\n")
            cmd <- paste("psql -U ",nameUser," -d ",db_name," -f ",fsql,sep="")
            myshell(cmd,invisible=TRUE)
        }
    }



}


