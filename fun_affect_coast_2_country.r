################################################################################
##
## Improve the precision of the TDWG map
##
################################################################################

                                        # For each additional polygon from monde, find the closest TDWG unit

library(sf)
library(ggplot2)
library(tidyverse)


exemple <- FALSE

##load("data/workspace_260220.Rdata")

##load("data/tmp_diffprecision.RData")


##monde_simple <- st_simplify(monde,preserveTopology=TRUE,dTolerance=1)

##saveRDS(monde_simple,"data/monde_simple.rds")

##monde_simple <- readRDS("data/monde_simple.rds")

##code_cntry <- "FRA"

Encoding_utf8 <- function(x) {
    Encoding(x) <- "UTF-8"
    return(x)
}


#d_diff<- coast2country(monde_simple=monde_simple,monde=monde,diffprecision=diffprecision,tdwg=tdwg,vec_cntry = c(vec_cntry[1:2], "FRA"))

coast2country <- function(monde_simple=NULL,monde,diffprecision,tdwg,vec_cntry=NULL) {

   start <- Sys.time()

    cat("\n\n START !  ",format(start, "%Y-%m-%d %X"),"\n\n")
    if(!dir.exists("fig"))
    {
        cat("\nCréation du repertoire de sortie des figures: fig\n")
        dir.create("fig")
    }

    if(is.null(monde_simple)) {
        cat("\nSimplification des polygones monde:")
        monde_simple <- st_simplify(monde,preserveTopology=TRUE,dTolerance=1)
        cat(" DONE!\nSauvegarde: --> data/monde_simple.rds")
        saveRDS(monde_simple,"data/monde_simple.rds")
        cat(" DONE!\n")
    }

    diff <- NULL

    diffprecision <- diffprecision %>% mutate_if(is.character, Encoding_utf8)
    tdwg <- tdwg %>% mutate_if(is.character, Encoding_utf8)


    if(is.null(vec_cntry)) vec_cntry <- unique(diffprecision$cntry_code)

    tab_cntry <- unique(st_drop_geometry(diffprecision)[,c("cntry_code","cntry_name")])

    tab_cntry <- subset(tab_cntry, cntry_code %in% vec_cntry)

    nb_cntry <- length(vec_cntry)
    cat("\n",nb_cntry,"pays:\n")
    cat(vec_cntry)
    cat("\n")

    vecCol <- c("main"="#4daf4a","other"="#ff7f00","out"="#377eb8")

    for(i in 1:length(vec_cntry)) {
                                        #browser()
        code_cntry <- tab_cntry$cntry_code[i]
        name_cntry <- tab_cntry$cntry_name[i]

        cat("\n\n---------------------\n [",i,"/",nb_cntry,"] ",code_cntry," ",name_cntry,"\n---------------------\n\n",sep="")
        monde_simple_cntry_strict <- subset(monde_simple,cntry_code == code_cntry)

       ## gg <- ggplot() + geom_sf(data=monde_simple,colour="black",alpha=0.5)+ geom_sf(data=monde_simple_cntry_strict,colour="blue",size=2,fill="blue",alpha=0.75)
       ## gg

        cat("* high resolution polygon for ",code_cntry," (diffprecision_cntry) that have to be affect to their true country\n")
        diffprecision_cntry <- diffprecision[which(diffprecision$cntry_code %in% code_cntry),]
        diffprecision_cntry <- st_buffer(diffprecision_cntry ,0.001)

        rad <- 2.5
        diff_cntry <- NULL
        flag_while_buff <- TRUE

        while(flag_while_buff) {
            cat("\n\n == Buffer: ",rad,"° ==\n\n",sep="")

            cat("(1/11) Selection of neighbouring countries\n")
            monde_simple_cntry_strict_buff <- st_buffer(monde_simple_cntry_strict,rad)

            inter <- st_intersects(tdwg,monde_simple_cntry_strict_buff,sparse=FALSE)
            tdwg_cntry <-tdwg[inter,]
            cat("   DONE!\n")

            cat("(2/11) Border buffer\n\n")
            buff_rad <- rad/10
            tdwg_cntry_buff0 <- st_buffer(tdwg_cntry, buff_rad)
            cat("   DONE!\n")
            cat("(3/11) Out of border buffer polygon\n")
            tdwg_cntry_buff0_out <- st_difference(st_buffer( monde_simple_cntry_strict_buff,rad*2),st_union(tdwg_cntry_buff0))
            tdwg_cntry_buff0_out$ogc_fid <- tdwg_cntry_buff0_out$ogc_fid*-1
            tdwg_cntry_buff0_out$level3_nam <- ""
            tdwg_cntry_buff0_out$level3_cod <- ""
            tdwg_cntry_buff0_out$level2_cod <- ""
            tdwg_cntry_buff0_out$level1_cod <- ""
            tdwg_cntry_buff0_out <- tdwg_cntry_buff0_out[,colnames(tdwg_cntry_buff0)]
            cat("   DONE!\n")
            tdwg_cntry_buff0 <- rbind(tdwg_cntry_buff0 ,tdwg_cntry_buff0_out)
            ##browser()

            cat("(4/11) Border buffer polygons simplification\n")
            i_cntry0 <- which(tdwg_cntry_buff0$level3_cod == code_cntry)
#browser()

      ##  gg <- ggplot() +geom_sf(data=tdwg_cntry_buff0_out,fill="blue",colour="black",alpha=0.5)+ geom_sf(data=tdwg_cntry)+geom_sf(data=tdwg_cntry_buff0,fill=NA,colour="black",alpha=0.5)#+ geom_sf(data=diffprecision_cntry,size=5,colour="red",fill="red")
       ##   gg

           ## gg <- ggplot()+ geom_sf(data=diffprecision_cntry,size=5,colour="red")
           ## gg


            tdwg_cntry_buff <- st_intersection(st_buffer(tdwg_cntry_buff0,0.01))
            cat("   DONE!\n")

            cat("(5/11) ",code_cntry," code assignment for ",code_cntry," country boundary polygons\n",sep="")
            if(length(i_cntry0)) {
                i_cntry <- which(sapply(tdwg_cntry_buff$origins,FUN=function(X) i_cntry0 %in% X))
                tdwg_cntry_buff$level3_cod[i_cntry] <- code_cntry

                cat("   DONE!\n")
            } else {
                cat("   SKIP!\n")}

      # gg <- ggplot() + geom_sf(data=  tdwg_cntry_buff0)#+geom_sf(data=  tdwg_cntry_buff,fill=NA,colour="black",alpha=0.5)+ geom_sf(data=st_centroid(diffprecision_cntry),size=5,colour="red",fill="red")
       #    gg

            cat("(6/11) Intersection between country boundary polygons and diffprecision_cntry\n")

            diff_poly0 <- st_intersection(diffprecision_cntry,tdwg_cntry_buff)
            diff_poly <- st_intersection(diff_poly0)

            #gg <- ggplot() + geom_sf(data=tdwg.simple_cntry_strict_buff)+geom_sf(data=tdwg_cntry,fill=NA,colour="black",alpha=0.5)+ geom_sf(data=st_centroid(diffprecision_cntry),size=5,colour="red",fill="red") + +geom_sf(data=diff_poly,fill=NA,colour="blue",size=3)
            #gg
            cat("   DONE!\n")


            cat("(7/11) Select the polygon those interact with ",code_cntry," border or with only one border\n")

            i_good <- which(diff_poly$level3_cod == code_cntry | diff_poly$n.overlaps == 1)
            diff_good <- diff_poly[i_good,]

            diff_out <- subset(diff_good,level3_cod == "")
            diff_good <- subset(diff_good,level3_cod != "")

            cat("(8/11) Manage the polygons those interesct with several borders\n")

            diff_conflit <- diff_poly[setdiff(1:nrow(diff_poly),i_good),]

            if(rad >= 10) {
                cat(" ! Radius wider that 10°, we search closer country polygon also for polygon those do not intersect with the border buffers !! \n" )
                diff_conflit <- rbind(diff_conflit,diff_out)
                diff_out <- subset(diff_out,level3_cod != "")
            }


            if(nrow(diff_conflit) > 0) {
                cat("nb of conflicts:",nrow(diff_conflit),"")
                near <- st_nearest_feature(diff_conflit,tdwg_cntry)
                diff_conflit$level3_cod <- tdwg_cntry$level3_cod[near]
                diff_conflit$level3_name <- tdwg_cntry$level3_name[near]
                diff_conflit$level2_cod <- tdwg_cntry$level2_cod[near]
                diff_conflit$level1_cod <- tdwg_cntry$level1_cod[near]

                diff_good <- rbind(diff_good,diff_conflit)
                cat("   DONE!\n")
            } else {
                cat("   SKIP!\n")
            }

              cat("(9/11) Calcul du nombre de polygones et de leur centroid\n")

            ## obliger de faire un tout petit buffer car certain elements sont des polystrings
            diff_poly_cast <- st_centroid(st_cast(st_buffer(diff_poly,0.001),to="POLYGON"))
            diff_poly_cast_df <- st_drop_geometry(diff_poly_cast)
            diff_poly_cast_df$nom <- paste0(diff_poly_cast_df$level3_nam," (",diff_poly_cast_df$level3_cod,")")
            tt_diff <- data.frame(table( diff_poly_cast_df$nom),stringsAsFactors=FALSE)
            colnames(tt_diff) <- c("nom","nb")
            tt_diff$nom <- as.character(tt_diff$nom)
            tt_diff$nom[tt_diff$nom==" ()"] <- "ZZZout"

            tt_diff <- tt_diff[order(tt_diff$nom),]
             tt_diff$nom[tt_diff$nom=="ZZZout"] <- "out"
            tt_diff$txt <- paste0(tt_diff$nom,": ",tt_diff$nb)
            cat("   DONE!\n")


            cat("(10/11) Figure\n")

            txt <- paste0("conflit: ",nrow(diff_conflit),", good: ",nrow(diff_good),", out: ",nrow(diff_out),"\n")
            attrib <- paste(unique(tt_diff$txt),collapse=" | ")
            txt <- paste0(txt,attrib)

            ## if(rad ==  5) browser()

            vecCol_cntry <- vecCol
            names(vecCol_cntry)[1] <- code_cntry

            gg <- ggplot() + geom_sf(data=monde_simple_cntry_strict_buff)+geom_sf(data=tdwg_cntry,fill=NA,colour="black",alpha=0.5)+ geom_sf(data=tdwg_cntry_buff,aes(fill=ifelse(level3_cod == code_cntry,code_cntry,ifelse(level3_cod == "","out","other"))),colour=NA,alpha=0.25) + geom_sf(data=diff_poly_cast,aes(colour=ifelse(level3_cod == code_cntry,code_cntry,ifelse(level3_cod == "","out","other"))),size=1) + geom_sf(data=diff_poly,aes(colour=ifelse(level3_cod == code_cntry,code_cntry,ifelse(level3_cod == "","out","other"))),size=2.5)
            gg <- gg + scale_colour_manual(values=vecCol_cntry)
            gg <- gg + scale_fill_manual(values=vecCol_cntry)

            gg <- gg + labs(fill="",colour="",title=paste0(name_cntry," (",code_cntry,") et pays proches | Zone: ",rad,"| Buffer:",round(buff_rad,2)),subtitle=txt)

            print(gg)

            ggfile <- paste0("fig/",code_cntry,"_",rad*10,".png")

            cat(" --> ",ggfile)
            ggsave(ggfile,gg)#,width=7,height=7)
            cat("   DONE!\n")


            cat("(11/11) End of Buffer:",rad,"   Preparing next radius\n")
            flag_while_buff <- nrow(diff_out) > 0

            if(flag_while_buff) {
                rad <- rad * 2
                diff_out <- diff_out[,colnames(diffprecision_cntry)]
                diffprecision_cntry <- diff_out
                 cat("   DONE!\n")
            } else {
                cat("   SKIP!\n")
            }

            diff_cntry <- rbind(diff_cntry,diff_good)
        }
        cat("\n** End of country:",code_cntry,"\n")
        diff <- rbind(diff,diff_cntry)
        cat("** Save to diff.rds")
        saveRDS(diff,"diff.rds")
        cat("   DONE!\n")
        current <- Sys.time()

        duration <- current-start

        reste <- duration/i * (nb_cntry-i)

        cat("\n[",i,"/",nb_cntry,"]  |--> Elapsed time: ",round(duration,1)," ",attr(duration,"units"),"   -->|  Estimated time: ",round(reste,1)," ",attr(reste,"units"),"\n",sep="")

    }
    cat("\n\n** Save to diff.rds")
    saveRDS(diff,"diff.rds")
    cat("   DONE!\n")

    end <- Sys.time()
    duration <- current-start

    cat("\n\nEND !  ",format(end, "%Y-%m-%d %X"),"  -- Total duration: ",round(duration,1)," ",attr(duration,"units"),"\n",sep="")

    return(diff)
}




if (exemple) {
load("data/workspace_260220.Rdata")
load("data/tmp_diffprecision.RData")
monde_simple <- readRDS("data/monde_simple.rds")
d_diff<- coast2country(monde_simple=monde_simple,monde=monde,diffprecision=diffprecision,tdwg=tdwg,vec_cntry=c("FRA","ARG"))

}

