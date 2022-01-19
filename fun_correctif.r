

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title correction des noms de communes
##' @param data data table or path of the data table
##' @param com_cor correcting table of the path of the table
##' @return data.table of updating data
##' @author Romain Lorrilliere
corrections <- function(data=NULL,com_cor="library/correctifs.csv",
                        output=FALSE,save=saveStep,fileoutput=NULL,
                        repOut="output_import/",
                        field_2_upper=c("SEXE","AGE","GESTATION","MAMELLES","TEST","EPID","TV","EPIPH","CHIN_SPOT","USURE_DENTS","STATUT_REPRO","COMMUNE","DEPARTEMENT"),
                        field_2_correctif=c("SEXE","AGE","TAXON","GESTATION","MAMELLES","TEST","EPID","TV","EPIPH","CHIN_SPOT","USURE_DENTS","STATUT_REPRO","COMMUNE","DEPARTEMENT"),
                        sex_values=c("FEMELLE","MALE",""),
                        age_values=c("ADULTE","IMMATURE","JUVENILE"),
                        dsp=NULL) {
    ## Pour deboguage ------
##    data=NULL;com_cor=NULL
##    output=FALSE;save=TRUE;fileoutput=NULL
##    field_2_upper=c("COMMUNE","SEXE","DEPARTEMENT","AGE")
##    field_2_correctif=c("COMMUNE","SEXE","DEPARTEMENT","AGE","TAXON")
##    sex_values=c("FEMELLE","MALE","")
##    age_values=c("ADULTE","IMMATURE","JUVENILE")
##    dsp=NULL
##    ## ----------------------
    library(data.table)


    data <- my_import_fread(data,"ex: fichier_capt_mai2018_analysesbis_clean.csv")
    com_cor <- fread("library/correctifs.csv")

    data <- data.table(data)
    for(f in field_2_corretif) {
        data[[f]] <- iconv(data[[f]], from = 'UTF-8', to = 'ASCII//TRANSLIT')
        f_raw <- paste(f,"raw",sep="_")
        vec_f <- data[[f]]
        data <- data[,(f_raw) := vec_f]
    }

    for (f in field_2_upper) data[[f]] <- toupper(as.character(data[[f]]))

    cat("\n\nPour les COMMUNES deplacement des articles en début de nom\n\n",sep="")
    vecArticle <- c("LA","LE","LES","L'")
    for( a in vecArticle) {
        from <- paste("[-,\\s]*\\(",a,"\\)[-,\\s]*\\(?([0-9]{1,2}|2[A,B])?\\)?[-,\\s]*$",sep="")
        ifrom <- grep(from,data$COMMUNE,perl=TRUE)
        cat("(",a,"): ",gsub("\\","\\\\",from,fixed=TRUE),"\n\n","    -> ",length(ifrom),"\n\n",sep="")
        data$COMMUNE[ifrom] <- paste(a,ifelse(a=="L'",""," "),data$COMMUNE[ifrom],sep="")
        data$COMMUNE[ifrom] <- gsub(from,"",data$COMMUNE[ifrom],perl=TRUE)

    }


    cat("\n\nCorrectifs: ",nrow(com_cor)," correctifs\n\n",sep="")

    lesfieldcorrectif <- paste("les",length(field_2_correctif),"colonnes")

    for(i in 1:nrow(com_cor)) {
        ##  i=1
        from <- gsub("\\\\","\\",com_cor[i,search],fixed=TRUE)
        ##  print(from)
        the_field <- com_cor[i,field]
        if (the_field == "") the_field <- field_2_correctif
        to <- com_cor[i,replaceBy]
        sub <- com_cor[i,restrict]!=""
        strict <- com_cor[i,strictly]
        multi <-  com_cor[i,multiple]
        for(field in the_field) {
            cat(i,": colonne:",field," | action:",gsub("\\","\\\\",as.vector(from),fixed=TRUE)," -> ", to," | subset:",sub,"  | nom complet: ",strict,"\n\n")

            if(sub) { # on remplace un sous ensemble des noms
                col <- com_cor[i,fieldRestrict]
                val <- com_cor[i,restrict]
                cat("     ",col," %in% ",val," \n\n")
                if(strict) { # on remplace tout le nom
                    if (multi) from <- strsplit(from,",")
                    if(field == "TAXON") browser()
                    nbcor <- nrow(data[data[[col]] %in% val & data[[field]] %in% from])
                    if(nbcor>0)
                        set(data,i=which(data[[col]] %in% val & data[[field]] %in% from),j=field,value=to)
                 } else { # ELSE strict   remplace qu'un pattern
                    nbcor <- length(grep(from,data[data[[col]] %in% val,COMMUNE]))
                    if(nbcor>0)
                        data[[field]] <- ifelse(data[[col]] %in% val, gsub(from,to,data[[field]]),data[[field]])
                } # END ELSE strict
            } else { # ELSE sub on remplace tous les noms
                if(strict) { # on remplace tout le nom
                    if (multi) from <- strsplit(from,",")
                    nbcor <- nrow(data[data[[field]] %in% from])
                    if(nbcor>0)
                        set(data,i=which(data[[field]] %in% from),j=field,value=to)
                } else { # ELSE strict   remplace qu'un pattern
                    nbcor <- length(grep(from,data[[field]],perl=TRUE))
                    if(nbcor>0)
                        data[[field]] <- gsub(from,to,data[[field]],perl=TRUE)
                } # END ELSE strict
            } #END ELSE sub
            cat("    -> ",nbcor," remplacement(s)\n\n")
        } #END for(field in the_field) {
    } #END for(i in 1:nrow(com_cor))


    if(save) {
        if(is.null(fileoutput))fileoutput<- paste0(format(as.Date(Sys.time()),"%Y%m%d"),"_clean.csv")
        file_data <- paste0(repOut,fileoutput)
        cat("\n\n")
        cat("  -->", file_data)
        write.csv(data,file_data,row.names=FALSE)
    }

    if(output) return(data)
} # END commune_corrections

