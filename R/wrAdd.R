#' @title Computes a vector of relative weights specific to a species in an entire data frame.
#'
#' @description Returns a vector that contains the relative weight specific to each species for all individuals in an entire data frame.
#'
#' @details This computes a vector that contains the relative weight specific to each species for all individuals in an entire data frame. The vector can be appended to an existing data.frame to create a variable that contains the relative weights for each individual. The relative weight value will be \code{NA} for each individual for which a standard weight equation does not exist in \code{\link{WSlit}}, a standard weight equation for the units given in \code{units=} does not exist in \code{\link{WSlit}}, or if the individual is shorter or longer than the lengths for which the standard weight equation should be applied. Either the linear or quadratic equation has been listed as preferred for each species, so only that equation will be used.
#' 
#' The species names in \code{species} must match the spelling and capitalization of \code{species} in \code{\link{WSlit}}. Use \code{wsVal()} to see a list of all species for which standard weight equations exist in \code{\link{WSlit}} and, more importantly, how the species names are spelled and capitalized.
#' 
#' The \code{thesaurus} argument may be used to relate alternate species names to the species names used in \code{WSlit}. For example, you (or your data) may use \dQuote{Bluegill Sunfish}, but \dQuote{Bluegill} is used in \code{WSlit}. The alternate species name can be used here if it is defined in a named vector (or list) given to \code{thesarus=}. The alternate species name is the value and the species name in \code{PSDlit} is the name in this vector/list - e.g., \code{c("Bluegill"="Bluegill Sunfish")}. See the examples for a demonstration.
#' 
#' Some species have length categories separated by sub-group. For example, length categories exist for both lentic and lotic populations of Brown Trout. The length values for a sub-group may be obtained by either including the species name in \code{species} and the sub-group name in \code{group} in \code{WsOpts} or by using the combined species and sub-group name, with the sub-group name in parentheses, in \code{species}. Both methods are demonstrated in the examples. Note that an error is returned if a species has sub-groups but neither method is used to define the sub-group.
#' 
#' Some (few) species have more than one equation listed in \code{\link{WSlit}} (for the specified units). In these instances the user must select one of the equations to use with \code{WsOpts}. \code{WsOpts} is a list of lists where the inside list contains one or more of \code{group}, \code{ref}, or \code{method} (see \code{\link{WSlit}}) required to specify a single equation for a particular species, which is the name of the inner list. See the examples for an illustration of how to use \code{WsOpts}.
#' 
#' See examples and \href{https://fishr-core-team.github.io/FSA/articles/Computing_Relative_Weights.html}{this article} for a demonstration.
#'
#' @param wt A numeric vector that contains weight measurements or a formula of the form \code{wt~len+spec} where \dQuote{wt} generically represents the weight variable, \dQuote{len} generically represents the length variable, and \dQuote{spec} generically represents the species variable. Note that this formula can only contain three variables and they must be in the order of weight first, length second, species third.
#' @param len A numeric vector that contains length measurements. Not used if \code{wt} is a formula.
#' @param spec A character or factor vector that contains the species names. Not used if \code{wt} is a formula.
#' @param data A data.frame that minimally contains variables of the the observed lengths, observed weights, and the species names given in the \code{formula=}.
#' @param thesaurus A named list for providing alternative species names (the values in the list) that correspond to specific names in \code{PSDlit} (the names in the list). See details and examples.
#' @param units A string that indicates whether the weight and length data in \code{formula} are in \code{"metric"} (DEFAULT; mm and g) or \code{"English"} (in and lbs) units.
#' @param WsOpts A named list that provides specific choices for \code{group}, \code{ref}, or \code{method} for species for which more than one standard weight equation exists in \code{\link{WSlit}}.
#' @param \dots Not used.
#'
#' @return A numeric vector that contains the computed relative weights, in the same order as in \code{data=}.
#'
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @section IFAR Chapter: 8-Condition.
#'
#' @seealso See \code{\link{wsVal}}, \code{\link{WSlit}}, and \code{\link{psdAdd}} for related functionality. See \code{\link[plyr]{mapvalues}} for help in changing species names to match those in \code{\link{WSlit}}.
#'
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#'
#' @keywords manip
#'
#' @examples
#' #===== Simple example with 3 species, 2 in WSlit ... nothing unusual
#' tmp <- subset(PSDWRtest,
#'               species %in% c("Yellow Perch","Iowa Darter","Largemouth Bass"),
#'               select=c("species","len","wt"))
#' peek(tmp,n=10)
#' 
#' #----- Add Wr variable ... using formula interface
#' tmp$wr1 <- wrAdd(wt~len+species,data=tmp)
#' #----- same but with non-formula interface
#' tmp$wr2 <- wrAdd(tmp$wt,tmp$len,tmp$species)
#' #----- same but using dplyr
#' if (require(dplyr)) {
#'   tmp <- tmp %>%
#'     mutate(wr3=wrAdd(wt,len,species))
#' }
#' #----- examine results
#' peek(tmp,n=10)
#' 
#' #===== Simple example with only one species in the data.frame
#' tmp <- subset(PSDWRtest,species %in% c("Yellow Perch"),
#'               select=c("species","len","wt"))
#' tmp$wr <- wrAdd(wt~len+species,data=tmp)
#' peek(tmp,n=6)
#' 
#' #===== Example of species with sub-groups but only 1 sub-group in data.frame
#' #-----   Group not in species name so must specify group with WsOpts
#' tmp <- subset(PSDWRtest,species=="Brown Trout" & location=="Trout Lake",
#'               select=c("species","len","wt"))
#' tmp$wr1 <- wrAdd(wt~len+species,data=tmp,
#'                  WsOpts=list("Brown Trout"=list("group"="lotic")))
#' 
#' #-----   Group in species name so don't specify group with WsOpts
#' tmp$species2 <- "Brown Trout (lotic)"
#' tmp$wr2 <- wrAdd(wt~len+species2,data=tmp)  # note use of species2
#' 
#' peek(tmp,n=6)
#' 
#' #===== Example of species with sub-groups and 2 sub-groups in data.frame
#' tmp <- subset(PSDWRtest,species=="Brown Trout",
#'               select=c("species","location","len","wt"))
#' #-----   Must create "species" with sub-groups in name
#' #-----     Many ways to do this, this is just one example for this case
#' tmp$species2 <- ifelse(tmp$location=="Trout Lake",
#'                        "Brown Trout (lotic)","Brown Trout (lentic)")
#' tmp$wr <- wrAdd(wt~len+species2,data=tmp)  # note use of species2
#' peek(tmp,n=6)
#' 
#' #===== Example of a species name that needs the thesaurus
#' tmp <- subset(PSDWRtest,species %in% c("Yellow Perch","Bluegill Sunfish"),
#'               select=c("species","len","wt"))
#' #-----  Below will not add wr for "Bluegill Sunfish" as not in WsLit ("Bluegill" is)
#' tmp$wr1 <- wrAdd(wt~len+species,data=tmp)
#' #-----  Use thesaurus to identify "Bluegill Sunfish" as "Blueill
#' tmp$wr2 <- wrAdd(wt~len+species,data=tmp,thesaurus=c("Bluegill"="Bluegill Sunfish"))
#' peek(tmp,n=10)
#' 
#' #===== Example of species that has Ws eqns for multiple reference values
#' tmp <- subset(PSDWRtest,species=="Ruffe",select=c("species","len","wt"))
#' #-----  Below will err as Ruffe has Ws eqns for multiple reference values
#' # tmp$wr <- wrAdd(wt~len+species,data=tmp)
#' #-----  Must choose which eqn to use with WsOpts
#' tmp$wr <- wrAdd(wt~len+species,data=tmp,
#'                 WsOpts=list(Ruffe=list(ref=75)))
#' peek(tmp,n=6)
#' 
#' #===== Example with two uses of WsOpts (and one species without)
#' tmp <- subset(PSDWRtest,species %in% c("Ruffe","Muskellunge","Iowa Darter"),
#'               select=c("species","len","wt"))
#' tmp$wr <- wrAdd(wt~len+species,data=tmp,
#'                 WsOpts=list(Muskellunge=list(group="overall"),
#'                             Ruffe=list(ref=75)))
#' peek(tmp,n=10)
#' 
#' @rdname wrAdd
#' @export
wrAdd <- function (wt,...) {
  UseMethod("wrAdd") 
}

#' @rdname wrAdd
#' @export
wrAdd.default <- function(wt,len,spec,thesaurus=NULL,
                          units=c("metric","English"),WsOpts=NULL,...) {
  ###### Internal Function
  #===== Print error if no options given, but they are needed
  iErrOpts <- function(df) {
    #----- Restrict data.frame to species, group, ref, and method ... then show
    print(df[,c("species","group","ref","method")])
    #----- Error message for next two possible problems
    STOP("More than one Ws equation exists for ",iStrCollapse(unique(df$species)),
         ". Please use a named list in 'WsOpts=' to select one ",
         "equation for ",iStrCollapse(unique(df$species))," by specifing 'group', ",
         "'ref', or 'method' as appropriate. See details in documentation ",
         "and above (for reference).")
  }
  
  #===== Find Ws eqn for a species, possibly given WsOpts
  iGetWs <- function(Wsdf,species,WsOpts) {
    #----- Make sure species is a character
    if (is.factor(species)) species <- as.character(species)
    #----- Reduce Wsdf to just for that species
    Wsdf <- droplevels(Wsdf[Wsdf$species==species,])
    #----- If >1 Ws eqn for species/units, then need user to further define
    #      with opts, otherwise one Ws eqn is returned
    if (nrow(Wsdf)>1) {
      # ..... Need opts for species but none given (at all) so STOP
      if (is.null(WsOpts)) iErrOpts(Wsdf)
      # ..... Identify which species opts were given for
      spec_opts <- names(WsOpts)
      # ..... If current species not in opts then STOP
      if (!species %in% spec_opts) iErrOpts(Wsdf)
      # ..... If current species in opts then iteratively reduce WSlit result
      #       based on the criteria in opts
      crit_opts <- names(WsOpts[[species]])
      for (i in seq_along(crit_opts)) {
        tmp <- WsOpts[[species]][i]
        if (!names(tmp) %in% c("group","ref","method"))
          STOP("'",names(tmp),"' in 'WsOpts=' must be one of 'group', ",
               "'ref', or 'method'.")
        Wsdf <- droplevels(Wsdf[Wsdf[[names(tmp)]]==tmp,])
        if (nrow(Wsdf)==0)
          STOP("Use of '",crit_opts[i],"=",tmp[[1]],"' for ",iStrCollapse(species),
               " did not return a standard weight equation. Please reconsider your ",
               "use of 'WsOpts=' to restrict to only one equation for ",
               iStrCollapse(species),".")
        else if (nrow(Wsdf)>1) {
          #..... send error if not reduced to only one Ws equation
          print(Wsdf[,c("species","group","ref","method")])
          STOP("Use of '",crit_opts[i],"=",tmp[[1]],"' for ",iStrCollapse(species),
               " did not result in a single standard weight equation. Please",
               " examine the output above to appropriately make another choice",
               " for use in 'WsOpts=' to restrict to only one equation for ",
               iStrCollapse(species),".")
        }
      }
    }
    #----- Return appropriate part of Wsdf (i.e., WSlit)
    Wsdf
  }
  ###### END Internal Function

  ###### BEGIN Main Function
  #===== Some checks
  units <- match.arg(units)
  if (!is.numeric(wt)) STOP("'wt' must be numeric.")
  if (!is.numeric(len)) STOP("'len' must be numeric.")
  if (!inherits(spec,c("character","factor")))
    STOP("'spec' must be character or factor.")
  
  #===== Prepare the Ws literature values data frame
  #----- load WSlit data frame into this functions environment
  WSlit <- iPrepWSlit(thesaurus)
  #----- isolate only those data for which those units exist
  WSlit <- droplevels(WSlit[WSlit$units==units,])
  
  #===== Create temporary results data.frame with length, weight, species,
  #      rownumbers, and Wr values (blank)
  data <- data.frame(len,wt,spec,rownums=seq_along(len),
                     Wr=rep(NA,length(len)))
  #===== Initiate a blank new data frame with same columns as old data frame
  ndata <- data[-c(seq_len(nrow(data))),]  
  #===== get list of species in data ... change from factor to character if necessary
  specs <- as.character(unique(spec))
  
  #===== cycle through each species where WS equations are known
  for (i in seq_along(specs)) {
    #----- isolate the current species
    tmp <- data[data[,3]==specs[i],]
    #----- compute Wr
    if (specs[i] %in% unique(WSlit$species)) {
      #..... standard weight exists for this species
      wseqn <- iGetWs(WSlit,specs[i],WsOpts)
      #..... predict log Ws
      predlog <- wseqn$int+wseqn$slope*log10(tmp[,1])
      #..... If quadratic coefficient exists then use it
      if (!is.na(wseqn$quad)) predlog <- predlog + wseqn$quad*(log10(tmp[,1])^2)
      #..... convert to Wr and add to tmp
      tmp$Wr <- tmp[,2]/(10^predlog)*100
      #..... change Wr for lengths less than min.len to NA
      tmp$Wr[tmp[,1]<wseqn$min.len] <- NA
      #..... change Wr for lengths greater than max.len (if it exists) to NA
      if (!is.na(wseqn$max.len)) tmp$Wr[tmp[,1]>wseqn$max.len] <- NA
    }
    #----- bind current species to the new data frame being created
    ndata <- rbind(ndata,tmp)
  }
  #===== reorder the data.frame to match original rows
  ndata <- ndata[order(ndata$rownums),]
  #===== return just the vector of Wr values
  ndata$Wr  
}

#' @rdname wrAdd
#' @export
wrAdd.formula <- function(wt,data,thesaurus=NULL,units=c("metric","English"),...) {
  #===== Perform some checks on the formula
  tmp <- iHndlFormula(wt,data,expNumR=1,expNumE=2,expNumENums=1,expNumEFacts=1)
  if (tmp$vnum!=3) STOP("'wt' must have one variable on the left-hand-side ",
                        "and two variables on the right-hand-side.")
  if (!tmp$metExpNumR) STOP("'wt' must have a left-hand-side.")
  if (!(tmp$Rclass %in% c("numeric","integer")))
    STOP("Variable on left-hand-side of 'wt' is not numeric (thus, not weights).")
  if (!tmp$metExpNumE) STOP("'wt' must have a right-hand-side with two variables.")
  if (!tmp$metExpNumENums)
    STOP("'wt' must have one and only one numeric variable (lengths) on right-hand-side.")
  if (!tmp$metExpNumEFacts)
    STOP("'wt' must have one and only one factor variable (species) on right-hand-side.")

  #===== Call the wrAdd.default
  wrAdd.default(tmp$mf[,tmp$Rpos],tmp$mf[,tmp$ENumPos],
                tmp$mf[,tmp$EFactPos],thesaurus,units,...)
}

# ==============================================================================
# Internal -- prepare WSlit by loading it and replacing its default names with
#             names in the thesaurus, if any
# ==============================================================================
iPrepWSlit <- function(thesaurus) {
  # Load WSlit into dat in this function namespace
  dat <- FSA::WSlit
  if (!is.null(thesaurus)) {
    # Some sanity checks on thesaurus
    if (!(is.vector(thesaurus) | is.list(thesaurus)))
      STOP("'thesaurus' must be either a vector or list. ",
           "Make sure it is not 'factor'ed.")
    if (length(names(thesaurus))==0)
      STOP("Values in 'thesaurus' must be named (with species names from 'WSlit'.)")
    if (!is.character(thesaurus))
      STOP("Values in 'thesaurus' must be strings of species names.")
    # thesaurus appears to be a named vector/list of strings ... start processing
    # Alphabetize names in thesaurus to match dat/WSlit
    thesaurus <- thesaurus[order(names(thesaurus))]
    # Remove name from thesaurus if not in dat/WSlit
    thes.nokeep <- which(!names(thesaurus) %in% unique(dat$species))
    if (length(thes.nokeep)>0) {
      MESSAGE("The following species names were in 'thesaurus' but do ",
              "not have an entry in 'WSlit' and will be ignored: ",
              iStrCollapse(names(thesaurus)[thes.nokeep]),".")
      thesaurus <- thesaurus[-thes.nokeep]      
    }
    # Find species in dat/PSDlit in kept thesaurus and change names to thesaurus names
    if (length(thesaurus)>0) {
      tmp <- match(dat$species,names(thesaurus))
      thes.pos <- which(!is.na(tmp))
      dat$species[thes.pos] <- unname(thesaurus[tmp[thes.pos]])
    }
  }
  # Return dat/WSlit
  dat
}

