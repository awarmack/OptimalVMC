#functions for calculating optimal performace



matchLength <- function(v1, v2){
  
  if(length(v1)==length(v2)){
    return(list(v1,v2))
  }
  
  if(length(v1)>length(v2)){
    v2 <- rep_len(v2, length.out = length(v1))
  } else {
    v1 <- rep_len(v1, length.out = length(v2))
  }
  return(list(v1,v2))
}

#get optimal boat speed at a given TWA (=btw)
getOptV <- function(btw, vtw, pol.model){
  
  if(any(is.na(btw)) | any(is.na(vtw)) ){
    return(NA)
  }
  
  #returns Optimal Velocity at a given Bearing to Wind (BTW) and Wind Speed (VTW)
  btw <- normbear(btw)
  
  btw <- fromNorth(btw)
  
  btw <- abs(btw)
  
  #ensure that the length of both btw and vtw are the same...
  inputvectors <- matchLength(btw, vtw)
  btw <- inputvectors[[1]]
  vtw <- inputvectors[[2]]
  
  #output the optimum Velocity for a given BTW and wind speed. 
  v <- bilinear(x=pol.model$twa, y=pol.model$tws, z=pol.model$pol.matrix, x0=btw, y0=vtw)
  
  return(v$z)
  
}


#convert bearing to +/- 0 (north) from 0~360
fromNorth <- function(bearing){
  bearing <- normbear(bearing)  #first convert to a normal bearing
  
  fromNorth <- ifelse(bearing > 180, bearing-360, bearing)
  fromNorth <- ifelse(fromNorth < -180, 360+fromNorth, fromNorth)
  return(fromNorth)
}

normbear <- function(x){
  #converts back to a normal bearing
  x %% 360
}


getTWA <- function(btm, twd){
  #twd <- fromNorth(twd)
  #btm <- fromNorth(btm)
  
  fromNorth(twd-btm)
  
}

getTWD <- function(twa, hdg){
  #twa = TRUE WIND ANGLE
  #hdg = Heading
  hdg <- fromNorth(hdg)
  twa <- fromNorth(twa)
  
  normbear(hdg + twa)
  
}




#figuring out max VMC Angle....
# We'll check the VMC of both tacks seperately and decide if we're on the wrong tack elsewhere. 
#note that this function is not vectorized.

# optvmc <- function(btm, twd, tws, pol.model){
#   #given a bearing to the mark from wind  and vtw
#   
#   #twd = True Wind Direction (between 0 - 360)
# 
#   #create a vector of angles +/- from the btm
#   twa <- seq(-179, 179, by=1)
#   
#   #find all bsp on the polar
#   v <- getOptV(twa, tws, pol.model)  
#   
#   #ensure TWD is in +/- 180 from 0. 
#   twd <- convBearing(twd)
#   
#   #Add true wind direction to true wind angle to get actual bearings of each point of the polar
#   bearings <- normbear(twd + twa)
#   
#   #find difference between each bearing and the mark
#   btm <- convBearing(btm)  #convert to bearing +/- 180 degrees from north (rather than 0~360)
#   offmark <- bearings - btm   
#   
#   vmc <- v*cos(offmark*(pi/180))
#   
#   #return bearing to 0-360 formant
#   bearings <- convBearingBack(bearings)
#   
#   opt <- data.frame(btm, twd, twa, bearings, offmark, bsp=v, vmc)
#   names(opt) <- c("btm", "twd", "opt_twa", "opt_bear", "DegOffMark", "opt_bsp", "vmc")
#   
#   vmcopt <- max(vmc)  ### Could be multiple points (how to handle if on opp tacks?)
#   
#   opt$vmc_gain <- vmcopt - getOptV(abs(btm-twd), vtw=tws, pol.model)
#   
#   #optimal angle to the mark
#   return_opt <- opt[which(vmcopt==vmc), ]
#   
#   return(return_opt)
#   
# }

getVMC <- function(btm, cog, bsp){
  #given a bearing to the mark (BTM) and course over ground(COG)
  # what is the vmc being made towards the mark
  
  #get angle between btm and cog
  angle <- fromNorth(cog) - fromNorth(btm)
  
  vmc <- bsp * cos(angle*(pi/180))
  
  return(vmc)
  
}


optvmc <- function(btm, twd, tws, pol.model){
  #BTM = Bearing to Mark (0-360)
  #TWD = True Wind Direction (0-360)
  #
  
  col_names <-  c("btm", "twd", "tws", "opt_twa", "opt_bear", "degoffmark", "opt_bsp", "opt_vmc", "vmc_gain")
  
  if(any(is.na(c(btm, twd, tws)))){ 
    
    na_row <- as.data.frame(matrix(NA, nrow=1, ncol=9))
    names(na_row) <- col_names
    
    return(na_row)
  }
  
  
  btm <- normbear(btm)
  twd <- normbear(twd)
  
  #nominal TWA (pointing at mark) where 0 at the wind
  ntwa <- getTWA(btm, twd)
  
  #create vector of angles off the mark (0 at the wind)
  off_mark <- seq(-50,50, by=1)         #degrees away from the mark + = Stbd
  twa_s <- fromNorth(ntwa + off_mark)   #twa at each degree off the mark
  
  #bearings of each test position (actual direction of optimal VMC)
  opt_bear <- normbear(btm - off_mark)
  
  #get BSP at each twa
  bsp <- getOptV(twa_s, vtw = tws, pol.model)
  
  #get vmc at each angle off mark
  vmc <- bsp * cos(off_mark*(pi/180))
  
  #find max VMC
  vmcopt <- max(vmc)
  
  #find gain
  if(abs(ntwa) < 40){  ntwa <- 40 }   #if we're on a beat, assume close hulled
  
  vmc_gain <- vmcopt - getOptV(ntwa, vtw=tws, pol.model)
  
  opt <- data.frame(btm, twd, tws, twa_s, opt_bear, off_mark, bsp, vmc, vmc_gain)
  names(opt) <- col_names
  
  return_opt <- head(opt[which(vmcopt==vmc), ], 1)   #only return one value if there are more than 1
  
  return(return_opt)
  
}