#' @title Creates a vector of Gabelhouse lengths for each species in an entire data frame.
#'
#' @description Creates a vector of the Gabelhouse lengths specific to a species for all individuals in an entire data frame.
#' 
#' @param len A numeric vector that contains lengths measurements or a formula of the form \code{len~spec} where \dQuote{len} generically represents the length variable and \dQuote{spec} generically represents the species variable. Note that this formula can only contain two variables and must have the length variable on the left-hand-side and the species variable on the right-hand-side.
#' @param species A character or factor vector that contains the species names. Ignored if \code{len} is a formula.
#' @param group A named list that provides specific choices for \code{group} for species for which more than one set of Gabelhouse lengths exists in \code{\link{PSDlit}}.
#' @param data A data.frame that minimally contains the length measurements and species names if \code{len} is a formula.
#' @param thesaurus A named list for providing alternative species names (the values in the list) that correspond to specific names in \code{PSDlit} (the names in the list). See details and examples.
#' @param units A string that indicates the type of units used for the lengths. Choices are \code{mm} for millimeters (DEFAULT), \code{cm} for centimeters, and \code{in} for inches.
#' @param use.names A logical that indicates whether the vector returned is numeric (\code{=FALSE}) or string (\code{=TRUE}; default) representations of the Gabelhouse lengths. See details.
#' @param as.fact A logical that indicates that the new variable should be returned as a factor (\code{=TRUE}) or not (\code{=FALSE}). Defaults to same as \code{use.names} unless \code{addLens} is not \code{NULL}, in which case it will default to \code{FALSE}. See details.
#' @param addLens A named list with (possibly named) numeric vectors of lengths that should be used in addition to the Gabelhouse lengths for the species that form the names in the list. See examples.
#' @param verbose A logical that indicates whether detailed messages about species without Gabelhouse lengths or with no recorded values should be printed or not.
#' @param \dots Not used.
#'
#' @details This computes a vector that contains the Gabelhouse lengths specific to each species for all individuals in an entire data frame. The vector can be appended to an existing data.frame to create a variable that contains the Gabelhouse lengths for each individual. The Gabelhouse length value will be \code{NA} for each individual for which Gabelhouse length definitions do not exist in \code{\link{PSDlit}}. Species names in the data.frame must be the same as those used in \code{\link{PSDlit}} (i.e., same spelling and capitalization; use \code{psdVal()} to see the list of species).
#' 
#' Some species have Gabelhouse lengths for sub-groups (e.g., \dQuote{lentic} vs \dQuote{lotic}). For these species, choose which sub-group to use with \code{group}.
#' 
#' Individuals shorter than \dQuote{stock} length will be listed as \code{substock} if \code{use.names=TRUE} or \code{0} if \code{use.names=FALSE}.
#' 
#' Additional lengths to be used for a species may be included by giving a named list with vectors of additional lengths in \code{addLens}. Note, however, that \code{as.fact} will be reset to \code{FALSE} if \code{addLens} are specified, as there is no way to order the names (i.e., factor levels) for all species when additional lengths are used.
#' 
#' See examples and \href{https://fishr-core-team.github.io/FSA/articles/Computing_PSDs.html}{this article} for a demonstration.
#'
#' @return A numeric or factor vector that contains the Gabelhouse length categories.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @section IFAR Chapter: 6-Size Structure.
#' 
#' @seealso \code{\link{psdVal}}, \code{\link{psdCalc}}, \code{\link{psdPlot}}, \code{\link{PSDlit}}, and \code{\link{wrAdd}} for related functions. See \code{\link[plyr]{mapvalues}} for help in changing species names to match those in \code{\link{PSDlit}}.
#' 
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Guy, C.S., R.M. Neumann, and D.W. Willis. 2006. New terminology for proportional stock density (PSD) and relative stock density (RSD): proportional size structure (PSS). Fisheries 31:86-87. [Was (is?) from http://pubstorage.sdstate.edu/wfs/415-F.pdf.]
#'
#' Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson. 2006. Proportional size distribution (PSD): A further refinement of population size structure index terminology. Fisheries 32:348. [Was (is?) from http://pubstorage.sdstate.edu/wfs/450-F.pdf.]
#'
#' Willis, D.W., B.R. Murphy, and C.S. Guy. 1993. Stock density indices: development, use, and limitations. Reviews in Fisheries Science 1:203-222. [Was (is?) from http://web1.cnre.vt.edu/murphybr/web/Readings/Willis\%20et\%20al.pdf.]
#'
#' @keywords manip
#' 
#' @examples
#' #===== Simple examples -- 2 species, no groups, names as in PSDlit
#' #----- Isolate simple data from PSDWRtest
#' tmp <- subset(PSDWRtest,
#'               species %in% c("Yellow Perch","Largemouth Bass"),
#'               select=c("species","len"))
#' peek(tmp,n=6)
#' 
#' #----- Add variable using category names -- non-formula notation
#' tmp$PSD <- psdAdd(tmp$len,tmp$species)
#' peek(tmp,n=6)
#' 
#' #----- Add variable using category names -- formula notation
#' tmp$PSD1 <- psdAdd(len~species,data=tmp)
#' peek(tmp,n=6)
#' 
#' #----- Add variable using length values as names
#' tmp$PSD2 <- psdAdd(len~species,data=tmp,use.names=FALSE)
#' peek(tmp,n=6)
#' 
#' #----- Same as above but using dplyr
#' if (require(dplyr)) {
#'   tmp <- tmp %>%
#'     mutate(PSD1A=psdAdd(len,species),
#'            PSD2A=psdAdd(len,species,use.names=FALSE))
#'   peek(tmp,n=6)
#' }
#' 
#' #===== Add lengths besides Gabelhouse lengths (start over with same simple data)
#' tmp <- subset(PSDWRtest,
#'               species %in% c("Yellow Perch","Largemouth Bass"),
#'               select=c("species","len"))
#' 
#' #----- Add a "minimum length" for one species
#' tmp$PSD3 <- psdAdd(len~species,data=tmp,
#'                    addLens=list("Yellow Perch"=c("minLen"=225)))
#' tmp$PSD3A <- psdAdd(len~species,data=tmp,
#'                     addLens=list("Yellow Perch"=225))
#' tmp$PSD3B <- psdAdd(len~species,data=tmp,
#'                     addLens=list("Yellow Perch"=c("minLen"=225)),use.names=FALSE)
#' head(tmp,n=6)
#' 
#' #----- Add add'l lengths and names for multiple species
#' tmp$psd4 <- psdAdd(len~species,data=tmp,
#'                    addLens=list("Yellow Perch"=175,
#'                                 "Largemouth Bass"=c(254,306)))
#' peek(tmp,n=20)
#' 
#' #===== Handle additional species in PSDlit but named differently
#' #----- Isolate different species data from PSDWRtest
#' tmp <- subset(PSDWRtest,
#'               species %in% c("Bluegill Sunfish","Lean Lake Trout"),
#'               select=c("species","len"))
#' 
#' #----- No "Bluegill Sunfish" in PSDlit, use thesaurus to note this is "Bluegill"
#' #        Note: "Lean Lake Trout" not processed as not in PSDlit
#' tmp$psd5 <- psdAdd(len~species,data=tmp,
#'                    thesaurus=c("Bluegill"="Bluegill Sunfish"))
#' peek(tmp,n=6)
#' 
#' #----- Process multiple species in PSDlit with different names
#' #        Note: Can still use addLens=, but with original name
#' thes <- c("Bluegill"="Bluegill Sunfish","Lake Trout"="Lean Lake Trout")
#' tmp$psd6 <- psdAdd(len~species,data=tmp,thesaurus=thes)
#' tmp$psd7 <- psdAdd(len~species,data=tmp,thesaurus=thes,
#'                    addLens=list("Bluegill Sunfish"=c("minLen"=175)))
#' peek(tmp,n=20)
#' 
#' #===== Example for a species with sub-groups but only one sub-group in data
#' #----- Isolate species data from PSDWRtest ... only Brook Trout has sub-group
#' tmp <- subset(PSDWRtest,
#'               species %in% c("Yellow Perch","Brook Trout"),
#'               select=c("species","len"))
#' 
#' #----- This will err as Brook Trout has sub-groups in PSDlit (as message notes)
#' # tmp$psd8 <- psdAdd(len~species,data=tmp)
#' 
#' #----- Can choose "overall" sub-group with group=
#' tmp$psd8 <- psdAdd(len~species,data=tmp,
#'                    group=list("Brook Trout"="overall"))
#' peek(tmp,n=10)
#' 
#' #----- Or can create species name with sub-group name in parentheses
#' #        Note: this is more useful in next examples
#' tmp$species2 <- ifelse(tmp$species=="Brook Trout","Brook Trout (overall)",
#'                        tmp$species)
#' tmp$psd8A <- psdAdd(len~species2,data=tmp) # note use of species2
#' peek(tmp,n=10)
#' 
#' #===== Example for species with more than one sub-group in data
#' #----- Isolate species data from PSDWRtest ... Brown Trout has two sub-groups
#' tmp <- subset(PSDWRtest,
#'               species %in% c("Yellow Perch","Largemouth Bass","Brown Trout"),
#'               select=c("species","len","location"))
#' peek(tmp,n=10)
#' 
#' #----- Must create a species name variable with sub-groups in parentheses
#' #        Note: there are likely many ways to do this specific to each use-case
#' tmp$species2 <- tmp$species
#' tmp$species2[tmp$species=="Brown Trout" & 
#'              tmp$location=="Trout Lake"] <- "Brown Trout (lotic)"
#' tmp$species2[tmp$species=="Brown Trout" & 
#'              tmp$location=="Brushy Creek"] <- "Brown Trout (lentic)"
#' peek(tmp,n=10)
#' 
#' tmp$psd9 <- psdAdd(len~species2,data=tmp)
#' peek(tmp,n=10)
#' 
#' @rdname psdAdd
#' @export
psdAdd <- function (len,...) {
  UseMethod("psdAdd") 
}

#' @rdname psdAdd
#' @export
psdAdd.default <- function(len,species,thesaurus=NULL,
                           group=NULL,units=c("mm","cm","in"),
                           use.names=TRUE,
                           as.fact=ifelse(is.null(addLens),use.names,FALSE),
                           addLens=NULL,verbose=TRUE,...) {
  ## Some checks
  units <- match.arg(units)
  if (!is.numeric(len)) STOP("'len' must be numeric.")
  if (!inherits(species,c("character","factor")))
    STOP("'species' must be character or factor.")
  ## Prepare the PSD literature values data frame
  PSDlit <- iPrepPSDlit(thesaurus)
  ##  Find species that have known Gabelhouse lengths
  # get list of species in data ... change from factor to character if necessary
  specs <- as.character(unique(species))
  GLHSspecs <- specs[specs %in% unique(PSDlit$species)]
  ## Create data.frames with species that are NA and w/o Gabelhouse lengths and
  ## one with Gabelhouse lengths. The loop below will then start with a 
  ## the non-Gabelhouse species and sequentially add the Gabelhouse fish 
  # Create data.frame with length, species, rownumbers, and PSD values (blank)
  # - rownumbers is needed to get back the original order
  # - PSD will eventually have the Gabelhouse length categories
  data <- data.frame(len,species,rownums=seq_along(len),PSD=rep(NA,length(len)))
  # data.frame where species is NA and doesn't have Gabelhouse length
  ndata <- data[is.na(data$species) | !data$species %in% GLHSspecs,]
  if (verbose & nrow(ndata)>0)
    MESSAGE("Species in the data with no Gabelhouse (PSD) lengths in `PSDlit`: ",
            iStrCollapse(unique(ndata$species)),".")
  # data.frame where species have Gabelhouse lengths ... make sure no NAs
  data <- data[data$species %in% GLHSspecs,]
  data <- data[!is.na(data$species),]
  
  ## Cycle through each species where PSD values are known, add PSD categories
  ## and append to data.frame that contained species w/o Gabelhouse lengths
  for (i in seq_along(GLHSspecs)) {
    # isolate a data.frame with the current species
    tmpdf <- data[data$species==GLHSspecs[i],]
    # add Gabelhouse lengths ... put in additional lengths if they are provided
    if (GLHSspecs[i] %in% names(addLens))
      tmpAddLens <- addLens[[GLHSspecs[i]]]
    else tmpAddLens <- NULL
    # get the Gabelhouse length categories
    if (!is.null(group)) {
      if (GLHSspecs[i] %in% names(group)) tmp_group <- group[[GLHSspecs[i]]]
      else tmp_group <- NULL
    } else tmp_group <- NULL
    glhse <- psdVal(GLHSspecs[i],group=tmp_group,units=units,addLens=tmpAddLens,
                    dat=PSDlit)
    # computes the Gabelhouse length categories and adds to the data frame
    if (all(is.na(tmpdf$len))) {
      if (verbose) message("All values in 'len' were missing for ",GLHSspecs[i])
      tmpdf$PSD <- tmpdf$len
    } else tmpdf$PSD <- lencat(tmpdf$len,breaks=glhse,
                               use.names=use.names,as.fact=FALSE)
    # bind current species to the new data frame being created
    ndata <- rbind(ndata,tmpdf)
  }
  ## reorder the data.frame to match original rows
  ndata <- ndata[order(ndata$rownums),]
  ## factor the PSD variable if using category names
  if (as.fact)
    ndata$PSD <- factor(ndata$PSD,
                        levels=c("substock","stock","quality",
                                 "preferred","memorable","trophy"))
  ## return just the vector of PSD values
  ndata$PSD
}

#' @rdname psdAdd
#' @export
psdAdd.formula <- function(len,data=NULL,thesaurus=NULL,
                           group=NULL,units=c("mm","cm","in"),
                           use.names=TRUE,
                           as.fact=ifelse(is.null(addLens),use.names,FALSE),
                           addLens=NULL,verbose=TRUE,...) {
  ## Perform some checks on the formula
  tmp <- iHndlFormula(len,data,expNumR=1,expNumE=1,expNumENums=0,expNumEFacts=1)
  if (tmp$vnum!=2)
    STOP("'len' must have one variable on the left-hand-side ",
         "and one variable on the right-hand-side.")
  if (!tmp$metExpNumR)
    STOP("'len' must have a left-hand-side with one and only one variable.")
  if (!(tmp$Rclass %in% c("numeric","integer")))
    STOP("Variable on left-hand-side of 'len' is not numeric",
         " (thus, not lengths).")
  if (!tmp$metExpNumE)
    STOP("'len' must have a right-hand-side with one and only one variable.")
  if (!tmp$metExpNumEFacts)
    STOP("'len' must have one and only one factor variable (species)",
         " on right-hand-side.")
  ## Send to default method
  psdAdd.default(tmp$mf[[tmp$Rpos]],tmp$mf[[tmp$EFactPos]],thesaurus,group,units,
                 use.names,as.fact,addLens,verbose,...)
}
