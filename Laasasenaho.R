calulate_wood_product_volumes <- function(step, saw_prop, pulp_prop) {
  return( function(stemcurve) {    

    # Calculate overall volume of the stemcurve
    vstem <- with(stemcurve, 1/3 * pi * step *  (
                                                (dl/2/100)^2 + (dl/2/100) * (dl_next/2/100) + (dl_next/2/100)^2
                                                )
                            
                  )
    vstem_tot <- sum(vstem)

    # Solve proportions for wood products
    saw_proportion <- stemcurve$dl > saw_prop
    pulp_proportion <- (stemcurve$dl < saw_prop & stemcurve$dl > pulp_prop)
    waste_proportion <- stemcurve$dl < pulp_prop
      
    vsaw <- sum(vstem[saw_proportion])
    vpulp <- sum(vstem[pulp_proportion])
    vwaste <- vstem_tot - (vsaw + vpulp)
      
    return( c(vstem_tot, vsaw, vpulp, vwaste) )
  })
}

init_stemcurve_df_generator <- function(treeh, step)
{
  return( function(dl) {
    h <- seq(step, treeh, step) # heigth vector for different diameters
    dl_next <- c(dl[2:length(dl)], .0)
    stemcurve_df <- data.frame(h=h, dl=dl, dl_next=dl_next)
    return(stemcurve_df)
  })
}



Laasasenaho_stemcurve_model <- function(step=0.1, saw, pulp, c1, c2, c3, c4, c5, c6, c7, c8)
{
  return(function(treed, treeh)
  {

    l <- 1.3 # cross-section heigth from ground
    x <- 1-(l/treeh) # relative distance from top
    t13 <- (
            c1*x
            + c2*(x^2)
            + c3*(x^3)
            + c4*(x^5)
            + c5*(x^8)
            + c6*(x^13)
            + c7*(x^21)
            + c8*(x^34)
            )
    d20 <- treed/t13 # basic diameter at 20% heigth
    
    h <- seq(step, treeh, step) # heigth vector for different diameters
    x <- 1-(h/treeh)
    dl <- d20 * (
                c1*x
                + c2*(x^2)
                + c3*(x^3)
                + c4*(x^5)
                + c5*(x^8)
                + c6*(x^13)
                + c7*(x^21)
                + c8*(x^34)
                )
    return(dl)
  })
}
    
stemcurve_vols <- function(step, saw, pulp, c1, c2, c3, c4, c5, c6, c7, c8)
{
  laasis_pine <- Laasasenaho_stemcurve_model(step, saw, pulp, c1, c2, c3, c4, c5, c6, c7, c8)
  cal_vols <- calulate_wood_product_volumes(step, saw, pulp)
  return(function(treed, treeh) {
    generate_stemcurve_df <- init_stemcurve_df_generator(treeh, step) 
    dl <- laasis_pine(treed, treeh)
    stemcurve_df <- generate_stemcurve_df(dl)
    return( cal_vols(stemcurve_df) )
  })
}

stemcurve_pine <- stemcurve_vols(0.1, 15, 7, 2.1288, -0.63157, -1.6082, 2.4886, -2.4147, 2.3619, -1.7539, 1.0817)
stemcurve_pine(24, 21) # >> 0.431646396 0.364797984 0.062068874 0.004779538

stemcurve_spruce <- stemcurve_vols(0.1, 15, 8, 2.3366, -3.2684, 3.6513, -2.2608, 0.0, 2.1501, -2.7412, 1.8876)
stemcurve_spruce(24, 21) # >> 0.443982098 0.376306778 0.059670332 0.008004988

stemcurve_birch <- stemcurve_vols(0.1, 15, 8, 0.93838, 4.1060, -7.8517, 7.8993, -7.5018, 6.3863, -4.3918, 2.1604)
stemcurve_birch(24, 21) # >> 0.427840282 0.367908611 0.052293887 0.007637784

