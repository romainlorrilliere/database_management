################################################################################
##
## Improve the precision of the TDWG map
##
################################################################################

## For each additional polygon from monde, find the closest TDWG unit

library(sf)
library(ggplot2)
library(tidyverse)
library(lwgeom)

library(rcartocolor)

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

coast2country <- function(id=NULL,monde_simple=NULL,monde,diffprecision,tdwg,vec_cntry=NULL) {

    start <- Sys.time()
    if(is.null(id)) id <- format(start, "%Y-%m-%d")

    cat("Session id:",id,"\n")

    cat("\n\n START !  ",format(start, "%Y-%m-%d %X"),"\n\n")

    rep_fig <- paste0("fig_",id)
    if(!dir.exists(rep_fig))
    {
        cat("\nCréation du repertoire de sortie des figures: ",rep_fig,"\n")
        dir.create(rep_fig)
    }


 rep_out <- paste0("output")
    if(!dir.exists(rep_out))
    {
        cat("\nCréation du repertoire de sortie des figures: output\n")
        dir.create(rep_out)
    }

    fileRDS <- paste0("output/",id,"_diff.rds")


    if(is.null(monde_simple)) {
        cat("\nSimplification des polygones monde:")
        monde_simple <- st_simplify(monde,preserveTopology=TRUE,dTolerance=1)
        cat(" DONE!\nSauvegarde: --> data/monde_simple.rds")
        saveRDS(monde_simple,"data/monde_simple.rds")
        cat(" DONE!\n")
    }
    flush.console()


    diff <- NULL

    diffprecision <- diffprecision %>% mutate_if(is.character, Encoding_utf8)
    tdwg <- tdwg %>% mutate_if(is.character, Encoding_utf8)

    tab_cod_nam <- st_drop_geometry(tdwg) %>% subset(select=c("level3_cod","level3_nam")) %>% unique()
    rownames(tab_cod_nam) <- tab_cod_nam$level3_cod

    if(is.null(vec_cntry)) vec_cntry <- unique(diffprecision$cntry_code)

    tab_cntry <- unique(st_drop_geometry(diffprecision)[,c("cntry_code","cntry_name")])

    tab_cntry <- subset(tab_cntry, cntry_code %in% vec_cntry)

    nb_cntry <- length(vec_cntry)
    cat("\n",nb_cntry,"pays:\n")
    cat(vec_cntry)
    cat("\n")
    flush.console()

    vecCol_tot <- rcartocolor::carto_pal(12, "Bold")#7F3C8D,#11A579,#3969AC,#F2B701,#E73F74,#80BA5A,#E68310,#008695,#CF1C90,#f97b72,#4b4b8f,#A5AA99)

    for(i in 1:length(vec_cntry)) {
                                        #browser()
        code_cntry <- tab_cntry$cntry_code[i]
        name_cntry <- tab_cntry$cntry_name[i]

        cat("\n\n---------------------\n [",i,"/",nb_cntry,"] ",code_cntry," ",name_cntry,"\n---------------------\n\n",sep="")
        flush.console()
        monde_simple_cntry_strict <- subset(monde_simple,cntry_code == code_cntry)

        ## gg <- ggplot() + geom_sf(data=monde_simple,colour="black",alpha=0.5)+ geom_sf(data=monde_simple_cntry_strict,colour="blue",size=2,fill="blue",alpha=0.75)
        ## gg

        cat("* high resolution polygon for ",code_cntry," (diffprecision_cntry) that have to be affect to their true country\n")
        flush.console()
        diffprecision_cntry <- diffprecision[which(diffprecision$cntry_code %in% code_cntry),]
        diffprecision_cntry <- st_buffer(diffprecision_cntry ,0.001)

        rad <- 2.5
        diff_cntry <- NULL
        flag_while_buff <- TRUE

        while(flag_while_buff) {
            cat("\n\n == Buffer: ",rad,"° ==\n\n",sep="")

            cat("(1/11) Selection of neighbouring countries\n")
            flush.console()
            monde_simple_cntry_strict_buff <- st_buffer(monde_simple_cntry_strict,rad)

            inter <- st_intersects(tdwg,monde_simple_cntry_strict_buff,sparse=FALSE)
            tdwg_cntry <-tdwg[inter,]
            cat("   DONE!\n")
            flush.console()
            cat("(2/11) Border buffer\n\n")
            flush.console()
            buff_rad <- rad/5
            tdwg_cntry_buff0 <- st_buffer(tdwg_cntry, buff_rad)
            ##browser()



            ## gg <- ggplot() + geom_sf(data=tdwg_cntry_buff0,aes(fill=level3_cod),colour=NA,alpha=0.5)
            ##  gg

            cat("   DONE!\n")
            cat("(3/11) Out of border buffer polygon\n")
            flush.console()
            tdwg_cntry_buff0_out <- st_difference(st_buffer( monde_simple_cntry_strict_buff,rad*2),st_union(tdwg_cntry_buff0))
            tdwg_cntry_buff0_out$ogc_fid <- tdwg_cntry_buff0_out$ogc_fid * -1
            tdwg_cntry_buff0_out$level3_nam <- ""
            tdwg_cntry_buff0_out$level3_cod <- ""
            tdwg_cntry_buff0_out$level2_cod <- ""
            tdwg_cntry_buff0_out$level1_cod <- ""
            tdwg_cntry_buff0_out <- tdwg_cntry_buff0_out[,colnames(tdwg_cntry_buff0)]
            cat("   DONE!\n")
                                        #  tdwg_cntry_buff0 <- rbind(tdwg_cntry_buff0 ,tdwg_cntry_buff0_out)
            ##browser()

            cat("(4/11) Border buffer polygons simplification\n")
            flush.console()
            i_cntry0 <- which(tdwg_cntry_buff0$level3_cod == code_cntry)


            gg <- ggplot() +geom_sf(data=monde_simple_cntry_strict_buff,fill="blue",colour=NA,alpha=0.5)+geom_sf(data=tdwg_cntry_buff0,aes(fill=level3_cod),colour=NA,alpha=0.5)+ geom_sf(data=tdwg_cntry,aes(fill=level3_cod),alpha=.5)+ geom_sf(data=diffprecision_cntry,size=5,colour="red",fill="red")
            print(gg)


            ## gg <- gg + geom_point(aes(x=-62.686545435252611,y=-41.293004651442693),colour="green",size=3,alpha=.3)
            ## gg <- gg + coord_sf(xlim = c(-66,-60), ylim = c(-44,-39), expand = TRUE,default = FALSE, clip = "on")
            ## gg

            tdwg_cntry_buff <-   tdwg_cntry_buff0 %>% st_buffer(0.01) %>% st_set_precision(1e5)  %>% st_intersection
            cat("   DONE!\n")

            cat("(5/11) ",code_cntry," code assignment for ",code_cntry," country boundary polygons\n",sep="")
            flush.console()
            if(length(i_cntry0)>0) {
                i_cntry <- which(sapply(tdwg_cntry_buff$origins,FUN=function(X) i_cntry0 %in% X))
                tdwg_cntry_buff$level3_cod[i_cntry] <- code_cntry

                cat("   DONE!\n")
            } else {
                cat("   SKIP!\n")}


            cat("(6/11) Intersection between country boundary polygons and diffprecision_cntry\n")
            flush.console()
            ##  gg <- ggplot() +geom_sf(data=monde_simple_cntry_strict_buff,fill="blue",colour=NA,alpha=0.5)+ geom_sf(data=tdwg_cntry_buff0,aes(fill=level3_cod),colour=NA,alpha=0.5)+ geom_sf(data=tdwg_cntry,aes(fill=level3_cod),alpha=.5)+ geom_sf(data=diffprecision_cntry,size=5,colour="red",fill="red")# + geom_sf(data=diff_poly,fill=NA,colour="blue",size=3)
            ## print(gg)

            diff_poly0 <- st_intersection(diffprecision_cntry,tdwg_cntry_buff)
            if(nrow(diff_poly0)>0) {
                diff_poly <- st_intersection(diff_poly0)



                cat("   DONE!\n")


                cat("(7/11) Select the polygon those interact with ",code_cntry," border or with only one border\n")
                flush.console()
                i_good <- which(diff_poly$level3_cod == code_cntry | diff_poly$n.overlaps == 1)
                diff_good <- diff_poly[i_good,]

                                        #browser()
                diff_good <- subset(diff_good,level3_cod != "")

                cat("(8/11) Manage the polygons those interesct with several borders\n")
                flush.console()
                diff_conflit <- diff_poly[setdiff(1:nrow(diff_poly),i_good),]

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


                cat("(9/12) Manage the polygons those are outsite border buffers\n")
                flush.console()
                diff_out <- st_intersection(diffprecision_cntry,tdwg_cntry_buff0_out)

                if(nrow(diff_out) > 0){

                    diff_out_cast <- st_buffer(diff_out,0.001) %>% st_cast('MULTIPOLYGON')  %>% st_cast('POLYGON', warn=F)
                    diff_out_cast$n.overlaps <- NA
                    diff_out_cast$origins <- NA
                    diff_out_cast <- diff_out_cast[,colnames(diff_good)]

                    cat("nb of polygons:",nrow(diff_out_cast),"\n")

                    if(rad >= 10) {
                        cat(" ! Radius wider that 10°, we search closer country polygon also for polygon those do not intersect with the border buffers !! \n" )
                        flush.console()
                        near <- st_nearest_feature(diff_out_cast,tdwg_cntry)
                        diff_out_cast$level3_cod <- tdwg_cntry$level3_cod[near]
                        diff_out_cast$level3_name <- tdwg_cntry$level3_name[near]
                        diff_out_cast$level2_cod <- tdwg_cntry$level2_cod[near]
                        diff_out_cast$level1_cod <- tdwg_cntry$level1_cod[near]

                        diff_good <- rbind(diff_good,diff_out_cast)
                        cat("   DONE!\n")

                        diff_out <- subset(diff_out,level3_cod != "")
                        diff_out_cast <- subset(diff_out,level3_cod != "")
                    }
                } else {
                    cat("No polygon outside border buffers\n")
                    flush.console()
                }



                cat("(10/12) Calcul du nombre de polygones et de leur centroid\n")
                flush.console()
                ## obliger de faire un tout petit buffer car certain elements sont des polystrings
                diff_good_buff <- st_buffer(diff_good,0.001)
                diff_gg_cast <- diff_good_buff %>% st_cast('MULTIPOLYGON')  %>% st_cast('POLYGON', warn=F)
                ## browser()
                ##  gg <- ggplot() +geom_sf(data=monde_simple_cntry_strict_buff,fill="blue",colour=NA,alpha=0.5)+ geom_sf(data=tdwg_cntry_buff0,aes(fill=level3_cod),colour=NA,alpha=0.5)+ geom_sf(data=tdwg_cntry,aes(fill=level3_cod),alpha=.5)+ geom_sf(data=diffprecision_cntry,size=5,colour="red",fill="red") + geom_sf(data=diff_poly,fill=NA,colour="blue",size=3) +  geom_sf(data=diff_good,fill=NA,colour="white",size=2)+  geom_sf(data=diff_gg_cast,fill=NA,colour="black",size=1)
                ## gg

                if(nrow(diff_out)>0) diff_gg_cast <- rbind(diff_gg_cast,diff_out_cast)
                diff_gg_cast <- st_centroid(diff_gg_cast)
                diff_gg_cast_df <- st_drop_geometry(diff_gg_cast)

                tt_diff <- data.frame(table( diff_gg_cast_df$level3_cod),stringsAsFactors=FALSE)


                colnames(tt_diff) <- c("level3_cod","nb")
                tt_diff <- left_join(tt_diff,tab_cod_nam)
                tt_diff$level3_nam[is.na(tt_diff$level3_nam)] <- ""

                tt_diff$nom <- paste0(tt_diff$level3_nam," (",tt_diff$level3_cod,")")
                tt_diff$nom[tt_diff$nom==" ()"] <- "out"
                tt_diff$nb[tt_diff$nom=="out"] <- tt_diff$nb[tt_diff$nom=="out"] * -1
                tt_diff <- tt_diff[order(tt_diff$nb,decreasing=TRUE),]
                tt_diff$nb[tt_diff$nom=="out"] <- tt_diff$nb[tt_diff$nom=="out"] * -1

                tt_diff$txt <- paste0(tt_diff$nom,": ",tt_diff$nb)


#########------------------------###################

                txt <- paste0("conflit: ",nrow(diff_conflit),", good: ",nrow(diff_good),", out: ",nrow(diff_out),"\n")
                if(nrow(tt_diff)<4) {
                    attrib <- paste(tt_diff$txt,collapse=" | ")
                } else {
                    txt_attrib <-  tt_diff$txt
                    nb_attrib <- length(txt_attrib)
                    txt_attrib <- c(txt_attrib, rep("",3-(nb_attrib %% 3)))
                    tab_txt <- matrix(txt_attrib,ncol=3,byrow=TRUE)
                    attrib <- paste(apply(tab_txt,1,paste,collapse=" | "),collapse="\n")
                }

                txt <- paste0(txt,attrib)

                cat(txt)
                cat("\n")

                cat("   DONE!\n")
                flush.console()


                cat("(11/12) Figure\n")
                flush.console()


                ## if(rad ==  5) browser()


                level3_cod <- tt_diff$level3_cod
                nb_col <- length(level3_cod)
                vecCol_name<- c(level3_cod,"other","out")
                vecCol_values <- c(vecCol_tot[1:nb_col],"#969696","#000000")
                names(vecCol_values) <- vecCol_name

                tdwg_cntry_buff$gg_cod <- ifelse(tdwg_cntry_buff$level3_cod %in% level3_cod,tdwg_cntry_buff$level3_cod,"other")
                tdwg_cntry$gg_cod <- ifelse(tdwg_cntry$level3_cod %in% level3_cod,tdwg_cntry$level3_cod,"other")
                diff_gg_cast$gg_cod <- ifelse(diff_gg_cast$level3_cod =="","out", diff_gg_cast$level3_cod)


                gg <- ggplot()
                gg <- gg + geom_sf(data=monde_simple_cntry_strict_buff,colour="black",size=1,fill=NA)
                gg <- gg + geom_sf(data=tdwg_cntry,aes(fill=gg_cod),colour="black",alpha=0.2)
                gg <- gg + geom_sf(data=tdwg_cntry_buff,aes(fill=gg_cod),colour=NA,alpha=0.2)
                gg <- gg + geom_sf(data= tdwg_cntry_buff0_out,colour="white",size=1,fill="white",alpha=0.5)
                gg <- gg + geom_sf(data=diff_gg_cast,aes(colour=gg_cod),size=1.2)
                gg <- gg + scale_colour_manual(values=vecCol_values)
                gg <- gg + scale_fill_manual(values=vecCol_values,guide=FALSE)

                gg <- gg + labs(fill="",colour="",title=paste0(name_cntry," (",code_cntry,") et pays proches | Zone: ",rad,"| Buffer:",round(buff_rad,2)),subtitle=txt)

                print(gg)
                ##

                ggfile <- paste0(rep_fig,"/",id,"_",code_cntry,"_",rad*10,".png")

                cat(" --> ",ggfile)

                ggsave(ggfile,gg,width=10,height=8)
                cat("   DONE!\n")
                flush.console()

                cat("(12/12) End of Buffer:",rad,"   Preparing next radius\n")
                flush.console()
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
            } else {

                cat("\n !! all polygons are outside border buffer !!\n")

                cat("\n section from 6 to 12 skipped !! \n")
                flag_while_buff <- TRUE
                rad <- rad * 2
            }
            flush.console()
        }
        cat("\n** End of country:",code_cntry,"\n")
        diff <- rbind(diff,diff_cntry)
        cat("** Save to", fileRDS)
         saveRDS(diff,fileRDS)
        cat("   DONE!\n")
        current <- Sys.time()

        duration <- current-start

        reste <- duration/i * (nb_cntry-i)

        cat("\n[",i,"/",nb_cntry,"]  |--> Elapsed time: ",round(duration,1)," ",attr(duration,"units"),"   -->|  Estimated time: ",round(reste,1)," ",attr(reste,"units"),"\n",sep="")
        flush.console()
    }
    cat("\n\n** Save to",fileRDS)
    saveRDS(diff,fileRDS)
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
  #  d_diff<- coast2country(monde_simple=monde_simple,monde=monde,diffprecision=diffprecision,tdwg=tdwg,vec_cntry=c("FRA","ARG"))

    d_diff<- coast2country(monde_simple=monde_simple,monde=monde,diffprecision=diffprecision,tdwg=tdwg,vec_cntry=NULL)

}

