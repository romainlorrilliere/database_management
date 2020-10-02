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




#' Bind importation of a huge file in postgres
#'
#' @param file the raw data file name
#' @param vecSep a vector of probable separator. In defaut the script test successively \t ; ,
#' @param rawEncoding encoding of raw data file, default value : UTF-8
#' @param pathData directory of data
#' @param pathSQL directory of sql file that will maked by the script (this directory need to be exist)
#' @param doChangeChar bolean value if you need to change some character directly in the raw file before the importation. This process could be long, so use it only if necessary, default FALSE
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
#'
#'
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
    ## file="FR_species.txt"
    ## pathSQL= "sql/"; fileSQL= "_postgres_import_data.sql";   tableName= "fr_species"
    ## pkField="pk_fr";realField=NULL;intField=NULL;booleanField=NULL;vecIndex="esp"
    ## toDoLiteTable=FALSE;litetableName="alalite";keepedCol=NULL;colType=NULL;
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

    copyQuery <- paste0("\n\n\ \\copy ",tableName,ifelse(!is.null(excludedColumn),paste0("(",paste(h,collapse=","),")"),"")," FROM '",pathFile,"' with (format csv, header, delimiter '",theSeparator,"',",ifelse(!is.null(rawEncoding),paste0(" ENCODING '",rawEncoding,"' "),""),", null '')\n")#,ESCAPE '\"',ENCODING '",rawEncoding," ,QUOTE E'\\b'
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

        query <- paste("\nALTER TABLE ",ifelse(toDoLiteTable,litetableName,tableName)," ADD COLUMN ",geomName," geometry(Point,",epsgGeom,");\nUPDATE ",ifelse(toDoLiteTable,litetableName,tableName),"\nSET ",geomName," = ",ifelse(epsgRaw!=epsgGeom,"ST_Transform(",""),"ST_SetSRID(ST_MakePoint(", geom.lonlat[1],",", geom.lonlat[2],"),",epsgRaw,")",ifelse(epsgRaw!=epsgGeom,paste(",",epsgGeom,")",sep=""),""),";\n",sep="")
        cat(query)
        cat(query,file=fileSQL.path,append=TRUE)
    }


    if(!is.null(vecIndex)){
        query <- paste("\n",paste("CREATE INDEX index_",vecIndex," ON ",ifelse(toDoLiteTable,litetableName,tableName),"(",vecIndex,");\n",sep=""),collapse="")
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
#' @param ogrinfo option of ogr2ogr function defaut value FALSE
#'
#' @return NULL
#'
#' @examples psql_import_ogr(dbname,user,password)
#'

psql_import_ogr <- function(pathFile,epsg="4326",
                            dbname="",user="",password="",
                            pgis.tableName="",
                            host="localhost",ogrinfo=FALSE) {

    cmd <- paste("ogr2ogr -f PostgreSQL -t_srs EPSG:",epsg," PG:\"host=",host," port=5432 dbname=",dbname," user=",user," password=",password,"\" ",pathFile," -nln public.",dbname," -nlt MULTIPOLYGON -overwrite -progress -unsetFid --config PG_USE_COPY YES ",ifelse(ogrinfo,"ogrinfo --formats",""),"",sep="")
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







#' Add geometry on table of point
#'
#' @param dbname  for the name of the database on the host
#' @param user for the user name (default: current user), in windows OS you probably have to define the user
#' @param tableName for the table name that will be created
#' @param geomName the geom name by defaut paste("geom",epsgGeom,sep=""
#' @param epsgRaw the epsg of input data
#' @param epsgGeom the epsg of geom by defaut epsgRaw
#' @param geom.lonlat vector of the name of field to construct the geom default c("longitude","latitude")
#'
#' @return NULL
#' @export
#'
#' @examples psql_add_geom(dbname="harmony",tableName="amphibians",geom.lonlat=c("decimallongitude","decimallatitude"))
psql_add_geom <- function(    dbname=NULL,user="postgres",tableName,
                              geomName=paste("geom",epsgGeom,sep=""),
                              epsgRaw="4326",epsgGeom=epsgRaw,
                              geom.lonlat=c("longitude","latitude")) {


    ## ---- initializing of an example set of parameters for debugging ----
    ## geomName=NULL;epsgRaw="4326";epsgGeom=NULL;geom.lonlat=c("lon","lat")
    ## tableName="foo";dbname="bullshit"
    ## -------------------------------------------------------


        if(is.null(epsgGeom)) epsgGeom <- epsgRaw
        if(is.null(geomName)) geomName <- paste("geom",epsgGeom,sep="")

        query <- paste("\nALTER TABLE ",tableName," ADD COLUMN ",geomName," geometry(Point,",epsgGeom,");\nUPDATE ",tableName,"\nSET ",geomName," = ",ifelse(epsgRaw!=epsgGeom,"ST_Transform(",""),"ST_SetSRID(ST_MakePoint(", geom.lonlat[1],",", geom.lonlat[2],"),",epsgRaw,")",ifelse(epsgRaw!=epsgGeom,paste(",",epsgGeom,")",sep=""),""),";\n",sep="")
        cat(query)
        cat(query,file=fileSQL.path,append=TRUE)
    }





get_oso_data <- function(rep = NULL,vec_dep=NULL,year=2017) {

    ## ---- initializing parameters for debugging ----
    ## rep=NULL;vec_dep=NULL
    ## ---


    if(is.null(rep)) rep <- paste(getwd(),"/data/oso",year,sep="")

    if(!(rep %in% dir("data/")))
        dir.create(paste(rep,"/",sep=""),showWarnings=FALSE)


    if(is.null(vec_dep)) {
        tabDep <- read.csv2("data/departement.csv",encoding = "latin1")
        vec_dep <- tabDep[,1]
    }

    for(dep in vec_dep) {
        cat("departement:",dep,"\n")
        url <- paste("http://osr-cesbio.ups-tlse.fr/echangeswww/TheiaOSO/vecteurs_",year,"/departement_",dep,".zip", sep="")
        cat("   ",url)
        dest <- paste(rep,"/",dep,".zip",sep="")
        tt <- try(download.file(url,destfile = dest),silent = TRUE)
        if(class(tt)[1]=="try-error") {
            cat("Error for departement:", dep,"\n")
        } else {
            cat("  --> DONE !\n Extract ...")

            unzip(dest,exdir=rep)
            cat("  --> DONE !\n\n")
        }

    }

}



import_multiShape <- function(toSQL=FALSE,import=FALSE,doUnion=TRUE,shape_name="oso_france_2017",
                              nameIdGroupColumn="departement",typeIdGroupColumn="varchar(2)",
                              doColumnChecking=FALSE,doOriginalTableDroping=TRUE,importTableClasses=TRUE,
                              vecOriginalTableKeeped=paste("departement",c("75","91","92","94","78","44")),
                              con=NULL,
                              db_name="birdlab",nameUser="romain",pw=NULL) {



    ## --- initializing parameters for debugging ----
    ## toSQL=FALSE;import=FALSE;doUnion=TRUE;
    ## shape_name="theia_france_2016"
    ## nameIdGroupColumn="departement";typeIdGroupColumn="varchar(2)"
    ## doColunmChecking=TRUE;doOriginalTableDroping=TRUE;vecOriginalTableKeeped=paste("departement",c("75","91","92","94","78","44"),sep="_")
    ## con=NULL
    ## db_name="birdlab";nameUser="romain"; pw=NULL
    ## ---

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



    if(doColumnChecking) {
        if(is.null(con)) con <- openDB.PSQL(user=nameUser,pw=pw)
        if(is.null(vec.tableName)) {
            vec.tableName <- rep(NA,length(vecF))
            i <- 0
            for(f in vecF) {
                i <- i+1
                fsql <- paste(path.shape,"theia2016_",substr(f,13,14),".sql",sep="")
                name_table <- tail(readLines(fsql, n=4),1)
                name_table <- gsub("CREATE TABLE \"","",name_table)
                name_table <- gsub("\" (gid serial,","",name_table,fixed=TRUE)
                vec.tableName[i] <- name_table
            }
        }


        ## check column of departement tables
        tabCol <- NULL
        for(tt in vec.tableName) {
            query<- paste("SELECT column_name , data_type FROM information_schema.columns WHERE table_name   = '",tt,"';",sep="")
            cat("Colonne names query:\n", query,"\n")
            cnames <- dbGetQuery(con, query)
            tabCol <- unique(rbind(tabCol,cnames))
        }

        dupli <- duplicated(tabCol$column_name)
        if(any(dupli)) {

            cat("Warning !!   -> column_name with sevaral data_type\n")

            duplicol <- tabCol$column_name[dupli]
            for(col in duplicol) {
                print(subset(tabCol,column_name == col))
                theLines <- rownames(subset(tabCol,column_name == col))
                cat("Write the line number that you want to keep (with the most generalist data type)\n",paste(theLines,collapse=" or "),"\n")
                i <- readline()
                while(!(i %in% theLines)) {
                    cat("Error, the line number is not among the possibilities\nplease write again\n",paste(theLines,collapse=" or "),"\n")
                    i <- readline()

                } #end while

                lines_deleted <- setdiff(theLines,i)
                tabCol <- tabCol[!(rownames(tabCol) %in% lines_deleted),]

            } #end for(col in duplicol)
        }#  if(length(dupli)>0)

        for(tt in vec.tableName) {
            query <- paste("SELECT column_name , data_type FROM information_schema.columns WHERE table_name   = '",tt,"';",sep="")
            cat("Colonne names query:\n", query,"\n")
            tabCol.table <- dbGetQuery(con, query)
            if(any(dupli)) {
                for(col in duplicol) {
                    if(tabCol.table$data_type[tabCol.table$column_name==col] != tabCol$data_type[tabCol$column_name==col]) {
                        query <- paste("ALTER TABLE  ",tt,"\n",
                                       "ALTER COLUMN",col," type ",tabCol$data_type[tabCol$column_name==col],";")
                        cat("Changing column type query:\n", query,"\n",sep="")
                        dbSendQuery(con, query)
                    } #end if

                }#end for(col in duplicol)
            } #end if(any(dupli))

            if(any(!(tabCol$column_name%in%tabCol.table$column_name))) {
                newcol <- tabCol$column_name[!(tabCol$column_name%in%tabCol.table$column_name)]
                for(nc in newcol) {
                    query <- paste("ALTER TABLE  ",tt," ADD COLUMN ",nc," ",tabCol$data_type[tabCol$column_name==nc],";",sep="")
                    cat("ADD COLUMN query:\n", query,"\n",sep="")
                    dbSendQuery(con, query)
                } #end for(nc in newcol)
            }#end if(any!...
            if(!(is.null(nameIdGroupColumn))) {
                query <- paste("ALTER TABLE  ",tt," DROP COLUMN IF EXISTS ",nameIdGroupColumn,";\n",
                               "ALTER TABLE  ",tt," ADD COLUMN ",nameIdGroupColumn," ",typeIdGroupColumn,";\n",#" default ",substr(tt,nchar(tt)-1,nchar(tt)),";\n",
                               "UPDATE ",tt," SET ",nameIdGroupColumn," = '",substr(tt,nchar(tt)-1,nchar(tt)),"';",sep="")
                cat("ADD COLUMN for group identification '",nameIdGroupColumn,"':\n",query,"\n",sep="")
                dbSendQuery(con, query)
            }#end if(!(is.null(nameIdGroupColumn))
            nameIdGroupColumn="departement";typeIdGroupColumn="varchar(2)"
        }#end for(tt in vec.tableName) end of ALTER TABLE  to fit column between table


    }#end if(doColumnChecking)

    if(doUnion) {
        if(is.null(vec.tableName)) {
            vec.tableName <- rep(NA,length(vecF))
            i <- 0
            for(f in vecF) {
                i <- i+1
                fsql <- paste(path.shape,"theia2016_",substr(f,13,14),".sql",sep="")
                name_table <- tail(readLines(fsql, n=4),1)
                name_table <- gsub("CREATE TABLE \"","",name_table)
                name_table <- gsub("\" (gid serial,","",name_table,fixed=TRUE)
                vec.tableName[i] <- name_table
            }#end if(is.null(vec.tableName))
        }#end

        if(is.null(tabCol)) {
            for(tt in vec.tableName) {
                query<- paste("SELECT column_name , data_type FROM information_schema.columns WHERE table_name   = '",tt,"';",sep="")
                cat("Colonne names query:\n", query,"\n")
                cnames <- dbGetQuery(con, query)
                tabCol <- unique(rbind(tabCol,cnames))
            }

        }

        query.head <- paste("DROP TABLE IF EXISTS theia_france_2016;\n",
                            "CREATE TABLE theia_france_2016 as(\n")
        subquery.column <- paste(c(ifelse(is.null(nameIdGroupColumn),"",nameIdGroupColumn),
                                   setdiff(tabCol$column_name,nameIdGroupColumn)),collapse=",")
        query.body <- paste(paste("select ",subquery.column," \n from ", vec.tableName),collapse="\n union ")
        query.tail <- "\n);"
        query <- paste(query.head,query.body,query.tail,sep="")
        cat("Union query:\n",query,"\n",sep="")

        dbSendQuery(con, query)

        pathSQL <- paste(getwd(),"/sql/",sep="")
        cmd <- paste("psql -U ",nameUser," ",db_name," < ",pathSQL,"postgres_index_theia.sql",sep="")
        myshell(cmd)



    }#end if(doUnion)

    if(doOriginalTableDroping) {
        if(is.null(vec.tableName)) {
            vec.tableName <- rep(NA,length(vecF))
            i <- 0
            for(f in vecF) {
                i <- i+1
                fsql <- paste(path.shape,"theia2016_",substr(f,13,14),".sql",sep="")
                name_table <- tail(readLines(fsql, n=4),1)
                name_table <- gsub("CREATE TABLE \"","",name_table)
                name_table <- gsub("\" (gid serial,","",name_table,fixed=TRUE)
                vec.tableName[i] <- name_table
            }#end if(is.null(vec.tableName))
        }#end
        if(!is.null(vecOriginalTableKeeped)) {
            vec.tableName <- setdiff(vec.tableName,vecOriginalTableKeeped)
        }
        query <- paste(paste(paste("DROP TABLE IF EXISTS ", vec.tableName,sep=""),collapse=";\n"),";",sep="")
        cat("Drop query:\n",query,"\n")

        dbSendQuery(con, query)

    }

    if(importTableClasses){
        if(is.null(con)) con <- openDB.PSQL(user=nameUser,pw=pw)
        pathSQL <- paste(getwd(),"/sql/",sep="")
        cmd_init <- paste("psql -U ",nameUser," ",db_name," < ",pathSQL,"postgres_init_theia_classes.sql",sep="")
        myshell(cmd_init)

        path.data <- paste(getwd(),"/generic_data/theia_classes.csv",sep="")

        fileImportTable.theia_classes <- paste(pathSQL,"_postgres_import_theia_classes.sql",sep="")
        cat("\\copy theia_classes FROM ",path.data," with (format csv, header, delimiter ';')\n",
            sep="",file=fileImportTable.theia_classes)

        cmd_import <- paste("psql -U ",nameUser," ",db_name," < ",fileImportTable.theia_classes,sep="")
        myshell(cmd_import)

    }



}

