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
        if(class(dd)[1]=="try-error") cat("The conversion to table format did not work.... \n Output at dataframe format!! \n") else d <- dd
    }

    cat("\n   DONE!\n\n")
    d <- d[order(d),]

    return(d)
}

