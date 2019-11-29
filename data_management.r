##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Generic function to read separatd column file
##' @param file path file
##' @param sep vector of separator tested default c("\t",";",",")
##' @param dec vector of decimal tested  default c(".",",")
##' @param as.tibble logical to get a tibble format, default TRUE
##' @param print_head logical to print to screen the head of data
##' @param max_col_head the maximal number of column if the head of data is printed
##' @param print_summary logical to print to screen the summary of the data
##' @return data.frame or tibble
##' @author Romain Lorrilliere
my_read_delim <- function(file,sep=c("\t",";",","),dec=c(".",","),as.tibble=TRUE,print_head=TRUE,max_col_head=10,print_summary=FALSE) {

    ## file="data/DataRP_SpTron_90.csv";sep=c("\t",";",",");dec=c(".",",") ##

    nextSep <- TRUE
    nbSep <- length(sep)
    i <- 0

    cat("\nOpening:",file,"\n    with with decimal '",dec[1],"' and try separators",sep="")

    while(nextSep & i < nbSep) {
        i <- i + 1
        cat("\n '",sep[i],"'")
                                        #browser()
        d <- try(read.delim(file,header=TRUE,stringsAsFactor=FALSE,sep=sep[i],dec=dec[1]),silent=TRUE)

        if(class(d)[1]=="try-error") {
            nextSep <- TRUE
        } else {
            nextSep <- ncol(d) ==  1
        }
    }

    if(ncol(d)>1) {
        vecdec <- dec[-1]
        vecdec <- vecdec[which(vecdec != sep[i])]
        if(length(vecdec>0)) {
            thecolclass <- sapply(d,class)
            numericNotFound <- !("numeric" %in% thecolclass)
            if(numericNotFound) {
                cat("\n Test for other decimal character\n",sep="")
                colchar <- which(thecolclass=="character")
                j <- 0
                while(numericNotFound & j < length(colchar)) {
                    j <- j+1
                    b <- 0
                    while(numericNotFound & b < length(dec)) {
                        b <- b + 1
                        veccol <- d[,i]
                        veccol <- ifelse(is.na(veccol) | veccol == "" | veccol== " ",-99999,veccol)
                        veccol <- sub(dec[1],vecdec[b],veccol)
                        veccol <- as.numeric(veccol)
                        numericNotFound <- any(is.na(veccol))

                    }

                }
                if(!numericNotFound) {
                    cat("Decimal character founded: '",vecdec[b],"'\n",sep="")
                    cat("Opening:",file,"\n    with with decimal '",vecdec[b],"' and separator '",sep[i],"'\n",sep="")
                    d <- read.table(file,header=TRUE,stringsAsFactor=FALSE,sep=sep[i],dec=vecdec[b])

                }
            }
        }
    }else{
        cat("Only 1 column founded!!!\n    ---> Check that the separator is in the proposed separators!\n")
    }

                                        #browser()
    pbEncoding_columns <- grep("Ã",d)
    if(length(pbEncoding_columns)>0) {
        cat("\nCharacter do not recognised in ",length(pbEncoding_columns)," columns\n",sep="")
        cat("  --> Convert encoding to UTF-8\n")
        for(j in pbEncoding_columns) Encoding(d[,j]) <- "UTF-8"
    }



    cat("\n")
    cat("Dimension:\n")
    print(dim(d))
    if(print_head) {
        cat("Head:\n")
        print(d[1:5,1:(min(max_col_head,ncol(d)))])
    }

    if(print_summary) {
        cat("Summary:\n")
        print(summary(d))
    }

    if(as.tibble) {
        library(dplyr)
        dd <- try(as_tibble(d))
        if(class(dd)[1]=="try-error") cat("The conversion to table format did not work.... \n Output at dataframe format!! \n") else d <- dd
    }

    cat("\n   DONE!\n\n")
    return(d)
}




##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Add abscence in data set of observation
##' @param d data,
##' @param col_gr character vector of the column names that will be uses to construct the sample unit example : date, site. The species column could be add in this vector, default NULL
##' @param col_sp name of the species name column default NULL
##' @param col_value vectors of colmuns that will be update with abscence, defaut NULL, if null all columns not present in col_gr and col_sp will be update with abscence
##' @param dall if there are not all sample in d you can import a table with all sample with the column of col_gr, default NULL
##' @param as.tibble  logical to get a tibble format, default TRUE
##' @return update of d (data.frame or tibble) with 0 in the col_value(s) when species are absente
##' @author
add_absc <- function(d,col_gr=NULL,col_sp=NULL,col_value=NULL,dall=NULL,as.tibble=TRUE) {
    ##     col_value = c("nb_contacts","temps_enr"); col_sp = "col_sp"; col_gr = c("col_sample","expansion_direct")

    cat("\nAjout des absences\n")

    if(class(d)=="data.frame") d <- tibble(d)

    if(is.null(dall)) {
        l_gr <- list()
        col_exp <- c(col_sp,col_gr)
        for(i in 1:length(col_exp))
            l_gr[[i]] <- unique(pull(d,col_exp[i]))

        dall <-(expand.grid(l_gr,stringsAsFactors=FALSE))
        colnames(dall) <- col_exp
    }

    d <- full_join(d,dall)
    if(is.null(col_value)) {
        d[is.na(d)] <- 0
    } else {
        for(j in col_value)
            d[is.na(d[,j]),j] <- 0
    }

    if(as.tibble) {
        library(dplyr)
        dd <- try(as_tibble(d))
        if(class(dd)[1]=="try-error") {
            cat("The conversion to table format did not work.... \n Output at dataframe format!! \n")
           d <- dd
            d <- d[order(d),]
        }else {
             d <- d %>% arrange()

        }
    } else {
            d <- dd
            d <- d[order(d),]

        }

    cat("\n   DONE!\n\n")

    return(d)
}



sameFloat <- function(x,y,digits=NULL) {
  if(is.null(digits)) #recherche du nombre de décimal max necessaire
      digits <- max(c(nchar(as.character(x%%1))-2,nchar(as.character(y%%1))-2,0))
  deg <- 10^(-digits)
  return(abs(x-y) <= deg)

}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title get weather anomalie form sample data
##' @param dsample data or data path or NULL
##' @param first_year if NULL first year of data sample
##' @param last_year  if NULL last year of data sample
##' @param nc_local TRUE to extract value in local file
##' @param nc_extract TRUE if you want use the extract_nc_value function
##' @param nc_data  path of the Rdata of the weather data
##' @param var weather variables
##' @param dsample_colnames names of important columns in sample table defaut c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitudeWGS84")
##' @param output boolean export table in console
##' @param save boolean save or not the table
##' @param fileouput name of file to save the table
##' @return la table sample avec les anomalies des variables météo désiré à 1,3,9,27 et 81 jours
##' @author Romain Lorrilliere à partir d'un code de Yves Bas
get_sample_weather <- function(dsample=NULL,first_year=NULL,last_year=NULL,nc_local=TRUE,nc_extract=FALSE,nc_data=NULL,var=c("precipitation","mean_temp"),dsample_colnames=c("site_id"="site_id","date"="date","longitude"="longitude","latitude"="latitudeWGS84"),output=TRUE,save=FALSE,fileouput=NULL) {

    library(data.table)
    library(RNetCDF)
    library(climateExtract) #https://github.com/RetoSchmucki/climateExtract
    library(ncdf4)

##    dsample=NULL;first_year=NULL;last_year=NULL;nc_local=TRUE;nc_extract=FALSE;nc_data=NULL;var=c("precipitation","mean_temp");dsample_colnames=c("site_id"="id_site","date"="date","longitude"="longitude_wgs84","latitude"="latitudeWGS84");output=TRUE;save=FALSE;fileoutput=NULL

    if (is.null(dsample)) {
        print("select your sample file")
        dsample <- fread(file.choose())
    } else {
        if(class(dsample)[1]=="character") d <- fread(dsample)
    }
    if(class(dsample)[1]=="character") d <- fread(dsample)


    dsample_colnames2 <- setdiff(names(dsample_colnames),colnames(dsample))
    if(length(dsample_colnames2)>0) for(n in dsample_colnames2) dsample[,n] <-  dsample[,dsample_colnames[names(dsample_colnames)==n],with=FALSE]

    dsample$year <- format(as.Date(dsample$date),"%Y")

    if(is.null(first_year)) first_year <- min(dsample$year)
    if(is.null(last_year)) last_year <- max(dsample$year)

    lnc <- list()
    if(nc_local) {
        if(nc_extract) {
            for(v in var){
                cat(" - Variable:",v,"\n-----------------------------\n\n")
                lnc[[v]] <- extract_nc_value(first_year,last_year,local_file = TRUE)
            } # END for(v in var){
        } else { # ELSE  if(nc_extract)
            if(is.null(nc_data)){
                print("select your nc file at Rdata format")
                ## provient de :
                ## precipitation <- extract_nc_value(2014,2019) avec le fichier rr_ens_mean_0.25deg_reg_v20.0e.nc
                ## mean_temp <- extract_nc_value(2014,2019) avec le fichier tg_ens_mean_0.25deg_reg_v20.0e.nc
                ## save(list=c("precipitation","mean_temp"),file="XXX.Rdata")
                load(file.choose())
            } else { # ELSE  if(is.null(lnc))
                load(nc_data)
            }# END ELSE  if(is.null(lnc))
            for(v in var)
                    lnc[[v]] <-  get(v)
        } # END  ELSE  if(nc_extract)
    } else { # ELSE if(dnc_local)
        for(v in var){
            cat(" - Variable:",v,"\n-----------------------------\n\n")
            lnc[[v]] <- extract_nc_value(first_year,last_year,local_file = FALSE, clim_variable = v, grid_size = 0.25)
        } # END for(v in var){
    }# END ELSE if(dnc_local)

    dsample$longitudeRAW <- dsample$longitude
    dsample$latitudeRAW <-  dsample$latitude
    dsample$longitude <- (floor(dsample$longitude*4)/4)+0.125
    dsample$latitude <-  (floor(dsample$latitude*4)/4)+0.125

    dsite <- unique(dsample[,c("site_id","longitude","latitude")])

    for(l in 1:length(lnc)) {

        laVar <- names(lnc)[l]
        cat("Variable météo:",laVar,"\n--------------------------------\n\n")

        point.TM <- point_grid_extract(lnc[[l]],dsite)

        Jour=yday(point.TM$date_extract)

        weather <- data.frame(matrix(NA,nrow(dsample),5))
        colnames(weather) <- paste(laVar,c("A1","A3","A9","A27","A81"),sep="_")

        for (i in 1:nrow(dsample)) {
                                        # cat(i,"")
            if (i%%100==1){print(paste(i,Sys.time()))}
            MatchLongLat=match(paste(dsample$longitude[i],dsample$latitude[i]),paste(dsite$longitude,dsite$latitude))
            MatchDate=match(dsample$DATE_POSIX[i],as.character(point.TM$date_extract))
            T1=point.TM[MatchDate,(MatchLongLat+1)]
            D1=point.TM$date_extract[MatchDate]
            J1=yday(D1)
            J1_30=match(Jour,J1)
            N1=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J1_30)))
            weather$AT1[i]=T1-N1

            T3=mean(point.TM[(MatchDate-2):MatchDate,(MatchLongLat+1)])
            J3=c((J1-2):J1) #last 3 days
            J3=J3-floor((J3-1)/365)*365 #to keep in 1:365 domain
            J3_30=match(Jour,J3)
            N3=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J3_30)))
            weather$AT3[i]=T3-N3

            T9=mean(point.TM[(MatchDate-8):MatchDate,(MatchLongLat+1)])
            J9=c((J1-8):J1) #last 9 days
            J9=J9-floor((J9-1)/365)*365 #to keep in 1:365 domain
            J9_30=match(Jour,J9)
            N9=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J9_30)))
            weather$AT9[i]=T9-N9

            T27=mean(point.TM[(MatchDate-26):MatchDate,(MatchLongLat+1)])
            J27=c((J1-26):J1) #last 27 days
            J27=J27-floor((J27-1)/365)*365 #to keep in 1:365 domain
            J27_30=match(Jour,J27)
            N27=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J27_30)))
            weather$AT27[i]=T27-N27

            T81=mean(point.TM[(MatchDate-80):MatchDate,(MatchLongLat+1)])
            J81=c((J1-80):J1) #last 81 days
            J81=J81-floor((J81-1)/365)*365 #to keep in 1:365 domain
            J81_30=match(Jour,J81)
            N81=mean(subset(point.TM[,(MatchLongLat+1)],!is.na(J81_30)))
            weather$AT81[i]=T81-N81
        }
             dsample <- cbind(dsample,weather)


    }

    if(save) fwrite(dsample,fileouput)


    if(output) return(dsample)

}

