Laasasenaho_stemcurve_pine <- function(treed, treeh, step=0.1, saw=15, pulp=7, 
  c1=2.1288, c2=-0.63157, c3=-1.6082, c4=2.4886, c5=-2.4147, c6=2.3619, c7=-1.7539, c8=1.0817)
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
  
  h <- seq(step, treeh, step) # heigth vector
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
  
  # Generate dataframe for volume calculations
  dl_shift <- c(dl[2:length(dl)], .0)
  stemcurve <- data.frame(h=h, dl=dl, dl_shift=dl_shift)
  
  # Calculate overall volume
  vstem <- with(stemcurve, pi * step *((dl/2/100)^2 + (dl/2/100) * (dl_shift/2/100) + (dl_shift/2/100)^2)/3)
  vstem_tot <- sum(vstem)

  saw_proportion <- stemcurve$dl > saw
  pulp_proportion <- (stemcurve$dl < saw & stemcurve$dl > pulp)
  waste_proportion <- stemcurve$dl < pulp
  
  vsaw <- sum(vstem[saw_proportion])
  vpulp <- sum(vstem[pulp_proportion])
  vwaste <- vstem_tot - (vsaw + vpulp)
  
  return( c(vstem_tot, vsaw, vpulp, vwaste) )
  
}

Laasasenaho_stemcurve_spruce <- function(treed, treeh, step=0.1, saw=15, pulp=8,
  c1=2.3366, c2=-3.2684, c3=3.6513, c4=-2.2608, c5=0.0, c6=2.1501, c7=-2.7412, c8=1.8876)
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
  
  h <- seq(step, treeh, step) # heigth vector
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
  
  # Generate dataframe for volume calculations
  dl_shift <- c(dl[2:length(dl)], .0)
  stemcurve <- data.frame(h=h, dl=dl, dl_shift=dl_shift)
  
  # Calculate overall volume
  vstem <- with(stemcurve, pi * step *((dl/2/100)^2 + (dl/2/100) * (dl_shift/2/100) + (dl_shift/2/100)^2)/3)
  vstem_tot <- sum(vstem)

  saw_proportion <- stemcurve$dl > saw
  pulp_proportion <- (stemcurve$dl < saw & stemcurve$dl > pulp)
  waste_proportion <- stemcurve$dl < pulp
  
  vsaw <- sum(vstem[saw_proportion])
  vpulp <- sum(vstem[pulp_proportion])
  vwaste <- vstem_tot - (vsaw + vpulp)
  
  return( c(vstem_tot, vsaw, vpulp, vwaste) )
  
}

Laasasenaho_stemcurve_birch <- function(treed, treeh, step=0.1, saw=15, pulp=8,
  c1=0.93838, c2=4.1060, c3=-7.8517, c4=7.8993, c5=-7.5018, c6=6.3863, c7=-4.3918, c8=2.1604)
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
  
  h <- seq(step, treeh, step) # heigth vector
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
  
  # Generate dataframe for volume calculations
  dl_shift <- c(dl[2:length(dl)], .0)
  stemcurve <- data.frame(h=h, dl=dl, dl_shift=dl_shift)
  
  # Calculate overall volume
  vstem <- with(stemcurve, pi * step *((dl/2/100)^2 + (dl/2/100) * (dl_shift/2/100) + (dl_shift/2/100)^2)/3)
  vstem_tot <- sum(vstem)

  saw_proportion <- stemcurve$dl > saw
  pulp_proportion <- (stemcurve$dl < saw & stemcurve$dl > pulp)
  waste_proportion <- stemcurve$dl < pulp
  
  vsaw <- sum(vstem[saw_proportion])
  vpulp <- sum(vstem[pulp_proportion])
  vwaste <- vstem_tot - (vsaw + vpulp)
  
  return( c(vstem_tot, vsaw, vpulp, vwaste) )
  
}

treed <- 24
treeh <- 21
step <- .1
saw <- 15
pulp <- 7
c1=2.128800
c2=-0.63157
c3=-1.6082
c4=2.488600
c5=-2.4147
c6=2.361900
c7=-1.7539
c8=1.081700
Laasasenaho_stemcurve_pine(treed, treeh) # >> 0.431646396 0.364797984 0.062068874 0.004779538
Laasasenaho_stemcurve_spruce(treed, treeh) # >> 0.443982098 0.376306778 0.062707912 0.004967409
Laasasenaho_stemcurve_birch(treed, treeh) # >> 0.427840282 0.367908611 0.054863938 0.005067733
