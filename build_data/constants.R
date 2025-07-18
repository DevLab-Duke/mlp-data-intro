
#
#   Constants or quasi-constants that might need periodic updating
#

#' Civic Event Categories
#'
#' A character vector containing the complete list of civic event categories
#' tracked in the MLP dataset. These categories capture various forms of civic
#' and political activity including protests, government actions, and civil 
#' rights violations.
#'
#' @format A character vector with 20 elements:
#' \describe{
#'   \item{arrest}{Arrests of individuals}
#'   \item{censor}{Censorship activities}
#'   \item{electionactivity}{General election-related activities}
#'   \item{electionirregularities}{Election irregularities and violations}
#'   \item{cooperate}{Cooperative activities between actors}
#'   \item{coup}{Coup attempts or coups}
#'   \item{defamationcase}{Defamation cases and legal proceedings}
#'   \item{disaster}{Natural disasters and emergency responses}
#'   \item{legalaction}{Legal actions and proceedings}
#'   \item{legalchange}{Changes in laws and legal frameworks}
#'   \item{martiallaw}{Martial law declarations}
#'   \item{mobilizesecurity}{Mobilization of security forces}
#'   \item{corruption}{Corruption-related events}
#'   \item{activism}{Civil society and activist activities}
#'   \item{protest}{Protest activities}
#'   \item{purge}{Purges of officials or groups}
#'   \item{raid}{Raids and enforcement actions}
#'   \item{threaten}{Threats and intimidation}
#'   \item{violencelethal}{Lethal violence events}
#'   \item{violencenonlethal}{Non-lethal violence events}
#' }
#'
#' @source MLP (Machine Learning for Peace) project categorization
#' @seealso \code{\link{cr_vars}} for civic relevant subset
#' @examples
#' # View all civic event categories
#' civic
#' 
#' # Check if a category exists
#' "protest" %in% civic
#' 
#' # Count total categories
#' length(civic)
#' @export
civic <- c("arrest", "censor", "electionactivity", "electionirregularities", "cooperate",
           "coup", "defamationcase", "disaster", "legalaction", "legalchange",
           "martiallaw", "mobilizesecurity", "corruption", "activism", "protest", "purge",
           "raid", "threaten", "violencelethal", "violencenonlethal")

#' Regional and International Influence (RAI) Event Categories
#'
#' A character vector containing all 22 categories of regional and international
#' influence events tracked in the MLP dataset. These categories capture various
#' forms of international engagement including diplomatic, economic, military,
#' and cultural activities.
#'
#' @format A character vector with 21 elements:
#' \describe{
#'   \item{arms_transfer_security_aid_assistance}{Arms transfers and security aid}
#'   \item{bribery_economic_corruption}{Economic corruption and bribery}
#'   \item{cyber_attack}{Cyber attacks and digital warfare}
#'   \item{diplomatic_action}{General diplomatic actions}
#'   \item{diplomatic_mediation}{Diplomatic mediation efforts}
#'   \item{diplomatic_meeting}{Diplomatic meetings and summits}
#'   \item{diplomatic_statement}{Diplomatic statements and declarations}
#'   \item{diplomatic_ties}{Establishment or changes in diplomatic ties}
#'   \item{foreign_aid_assistance}{Foreign aid and assistance programs}
#'   \item{foreign_investment}{Foreign investment activities}
#'   \item{intelligence_counterintelligence}{Intelligence and counterintelligence}
#'   \item{joint_security_force_exercise}{Joint military and security exercises}
#'   \item{media_campaign_intervention}{Media campaigns and interventions}
#'   \item{military_activity}{Military activities and operations}
#'   \item{political_process_policy_intervention}{Political and policy interventions}
#'   \item{security_engagement}{Security engagement and cooperation}
#'   \item{social_academic_cultural_activity}{Social, academic, and cultural activities}
#'   \item{tech_transfer_investment}{Technology transfer and investment}
#'   \item{trade_agreement_exchange}{Trade agreements and exchanges}
#'   \item{trade_financial_sanction}{Trade and financial sanctions}
#'   \item{transnational_organization_crime}{Transnational organized crime}
#' }
#'
#' @source MLP (Machine Learning for Peace) project categorization
#' @examples
#' # View all RAI event categories
#' rai
#' 
#' # Check if a category exists
#' "diplomatic_action" %in% rai
#' 
#' # Count total categories
#' length(rai)
#' @export
rai <- c("arms_transfer_security_aid_assistance", "bribery_economic_corruption", "cyber_attack", "diplomatic_action", "diplomatic_mediation", "diplomatic_meeting",
         "diplomatic_statement", "diplomatic_ties", "foreign_aid_assistance", "foreign_investment", "intelligence_counterintelligence", "joint_security_force_exercise",
         "media_campaign_intervention", "military_activity", "political_process_policy_intervention", "security_engagement", "social_academic_cultural_activity",
         "tech_transfer_investment", "trade_agreement_exchange", "trade_financial_sanction", "transnational_organization_crime")

#' Civic Relevant Variables
#'
#' A character vector containing the subset of civic event categories that are
#' considered relevant to civic space. These variables represent civic events
#' that have been filtered to include only those pertaining to political and
#' civic space activities, excluding events that are narrowly criminal or
#' non-political in nature.
#'
#' @format A character vector with 11 elements representing civic space-relevant events:
#' \describe{
#'   \item{arrest}{Arrests of individuals (civic space relevant)}
#'   \item{cooperate}{Cooperative activities (positive civic space indicator)}
#'   \item{corruption}{Corruption-related events}
#'   \item{defamationcase}{Defamation cases and legal proceedings}
#'   \item{legalaction}{Legal actions and proceedings}
#'   \item{legalchange}{Changes in laws and legal frameworks}
#'   \item{purge}{Purges of officials or groups}
#'   \item{raid}{Raids and enforcement actions}
#'   \item{threaten}{Threats and intimidation}
#'   \item{violencelethal}{Lethal violence events}
#'   \item{violencenonlethal}{Non-lethal violence events}
#' }
#'
#' @details These variables undergo additional filtering to determine civic space
#' relevance. For example, arrests related to political activities are included
#' while arrests for purely criminal activities are excluded. This filtering
#' creates separate "_ncr" (non-civic relevant) versions of these variables.
#'
#' @source MLP (Machine Learning for Peace) project categorization
#' @seealso \code{\link{civic}} for complete list of civic categories
#' @examples
#' # View civic relevant variables
#' cr_vars
#' 
#' # Check overlap with civic variables
#' all(cr_vars %in% civic)
#' 
#' # Find civic variables not in civic relevant subset
#' setdiff(civic, cr_vars)
#' @export
cr_vars = c("arrest", "cooperate", "corruption", "defamationcase", "legalaction", "legalchange", "purge", "raid", "threaten", "violencelethal", "violencenonlethal")

#' Countries in MLP Dataset
#'
#' A character vector containing all 65 developing countries included in the
#' MLP (Machine Learning for Peace) dataset. Countries are organized by region:
#' Eastern Europe/Central Asia (EE/CA), Middle East & North Africa (MENA),
#' Latin America & Caribbean (LAC), East Asia (EA), and Sub-Saharan Africa (SSA).
#'
#' @format A character vector with 65 country names organized by region:
#' \describe{
#'   \item{EE/CA (16 countries)}{Eastern Europe and Central Asia}
#'   \item{MENA (4 countries)}{Middle East and North Africa}
#'   \item{LAC (12 countries)}{Latin America and Caribbean}
#'   \item{EA (11 countries)}{East Asia}
#'   \item{SSA (22 countries)}{Sub-Saharan Africa}
#' }
#'
#' @source MLP (Machine Learning for Peace) project country selection
#' @seealso \code{\link{country_regions}} for country-to-region mapping
#' @examples
#' # View all countries
#' countries
#' 
#' # Count total countries
#' length(countries)
#' 
#' # Check if a country is included
#' "Nigeria" %in% countries
#' 
#' # Count countries by region using country_regions
#' sapply(country_regions, length)
#' @export
countries <- c("Albania", "Armenia", "Belarus", "Georgia", "Hungary", "Kosovo", "Serbia", "Azerbaijan", "Moldova", "Macedonia", "Dominican Republic",
               "Turkey", "Ukraine", "Uzbekistan", "Kyrgyzstan", "Kazakhstan", # EE/CA
               "Algeria", "Mali","Morocco","Niger", # MENA
               "Colombia", "Costa Rica", "Ecuador", "El Salvador", "Guatemala", "Honduras",
               "Jamaica", "Mexico", "Nicaragua", "Paraguay", "Panama", "Peru",  # LAC
               "Bangladesh", "Cambodia", "India", "Indonesia", "Malaysia", "Nepal", "Pakistan", "Philippines", "Solomon Islands", "Sri Lanka", "Timor Leste", #EA
               "Angola", "Benin", "Burkina Faso", "Cameroon", "DR Congo", "Ethiopia", "Ghana", "Kenya", "Liberia", "Malawi",
               "Mauritania", "Mozambique", "Namibia", "Nigeria", "Rwanda", "Senegal", "South Africa", "South Sudan",
               "Tanzania", "Tunisia", "Uganda", "Zambia", "Zimbabwe") # SSA

#' Country-to-Region Mapping
#'
#' A named list mapping countries to their respective geographic regions in the
#' MLP dataset. This mapping is used for regional analysis and regional source
#' filtering.
#'
#' @format A named list with 5 elements:
#' \describe{
#'   \item{EE_CA}{Eastern Europe and Central Asia (16 countries)}
#'   \item{MENA}{Middle East and North Africa (4 countries)}
#'   \item{LAC}{Latin America and Caribbean (12 countries)}
#'   \item{EA}{East Asia (11 countries)}
#'   \item{SSA}{Sub-Saharan Africa (22 countries)}
#' }
#'
#' @source MLP (Machine Learning for Peace) project regional classification
#' @seealso \code{\link{countries}} for complete country list, \code{\link{region_sources}} for regional source mapping
#' @examples
#' # View all regions
#' names(country_regions)
#' 
#' # Get countries in Sub-Saharan Africa
#' country_regions$SSA
#' 
#' # Count countries by region
#' sapply(country_regions, length)
#' 
#' # Find which region a country belongs to
#' find_region <- function(country) {
#'   for (region in names(country_regions)) {
#'     if (country %in% country_regions[[region]]) {
#'       return(region)
#'     }
#'   }
#'   return(NULL)
#' }
#' find_region("Nigeria")
#' @export
country_regions <- list(
  "EE_CA" = c("Albania", "Armenia", "Belarus", "Georgia", "Hungary", "Kosovo", "Serbia", "Azerbaijan", "Moldova", "Macedonia", "Dominican Republic", "Turkey", "Ukraine", "Uzbekistan", "Kyrgyzstan", "Kazakhstan"),
  "MENA" = c("Algeria", "Mali", "Morocco", "Niger"),
  "LAC" = c("Colombia", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Paraguay", "Peru", "Panama", "Costa Rica"),
  "EA" = c("Bangladesh", "Cambodia", "India", "Indonesia", "Malaysia", "Nepal", "Pakistan", "Philippines", "Solomon Islands", "Sri Lanka", "Timor Leste"),
  "SSA" = c("Angola", "Benin", "Burkina Faso", "Cameroon", "DR Congo", "Ethiopia", "Ghana", "Kenya", "Liberia", "Malawi", "Mauritania", "Mozambique", "Namibia", "Nigeria", "Rwanda", "Senegal", "South Africa", "South Sudan", "Tanzania", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
)


#' International News Sources
#'
#' A character vector containing CSV filenames of validated international news
#' sources used in the MLP dataset. These 15 major international outlets provide
#' global coverage and are included for all countries in the analysis.
#'
#' @format A character vector with 15 elements representing international news sources:
#' \describe{
#'   \item{aljazeera.com.csv}{Al Jazeera}
#'   \item{bbc.com.csv}{BBC}
#'   \item{csmonitor.com.csv}{Christian Science Monitor}
#'   \item{france24.com.csv}{France 24}
#'   \item{nytimes.com.csv}{New York Times}
#'   \item{reuters.com.csv}{Reuters}
#'   \item{scmp.com.csv}{South China Morning Post}
#'   \item{theguardian.com.csv}{The Guardian}
#'   \item{themoscowtimes.com.csv}{The Moscow Times}
#'   \item{washingtonpost.com.csv}{Washington Post}
#'   \item{wsj.com.csv}{Wall Street Journal}
#'   \item{lemonde.fr.csv}{Le Monde}
#'   \item{liberation.fr.csv}{Libération}
#'   \item{elpais.com.csv}{El País}
#'   \item{lefigaro.fr.csv}{Le Figaro}
#' }
#'
#' @details These sources were selected based on their global reach, editorial
#' independence, and consistent coverage of international events. Note that
#' xinhuanet was recently replaced with sapo.pt in the source list.
#'
#' @source MLP (Machine Learning for Peace) project source validation
#' @seealso \code{\link{rsources}} for regional sources, \code{\link{local_source_select}} for local sources
#' @examples
#' # View all international sources
#' isources
#' 
#' # Count international sources
#' length(isources)
#' 
#' # Check if a source is included
#' "bbc.com.csv" %in% isources
#' 
#' # Remove .csv extension for display
#' gsub(".csv", "", isources)
#' @export
isources = c("aljazeera.com.csv", "bbc.com.csv", "csmonitor.com.csv", "france24.com.csv",
             "nytimes.com.csv", "reuters.com.csv", "scmp.com.csv", "theguardian.com.csv",
             "themoscowtimes.com.csv", "washingtonpost.com.csv", "wsj.com.csv",
             "lemonde.fr.csv", "liberation.fr.csv", "elpais.com.csv",
             "lefigaro.fr.csv")

#' Regional News Sources
#'
#' A character vector containing CSV filenames of validated regional news sources
#' used in the MLP dataset. These 12 outlets provide regional coverage and are
#' selectively applied based on geographic relevance using the \code{\link{region_sources}}
#' function.
#'
#' @format A character vector with 12 elements representing regional news sources:
#' \describe{
#'   \item{africanews.com.csv}{Africanews}
#'   \item{asia.nikkei.com.csv}{Asia Nikkei}
#'   \item{asiatimes.com.csv}{Asia Times}
#'   \item{balkaninsight.com.csv}{Balkan Insight}
#'   \item{cnnespanol.cnn.com.csv}{CNN en Español}
#'   \item{euronews.com.csv}{Euronews}
#'   \item{indiatimes.com.csv}{India Times}
#'   \item{iwpr.net.csv}{Institute for War & Peace Reporting}
#'   \item{neweasterneurope.eu.csv}{New Eastern Europe}
#'   \item{timesofindia.indiatimes.com.csv}{Times of India}
#'   \item{telemundo.com.csv}{Telemundo}
#'   \item{theeastafrican.co.ke.csv}{The East African}
#' }
#'
#' @details Regional sources are mapped to specific geographic regions through
#' the \code{region_rsources} list. When \code{use_region_filter = TRUE}, only
#' regionally relevant sources are included for each country.
#'
#' @source MLP (Machine Learning for Peace) project source validation
#' @seealso \code{\link{isources}} for international sources, \code{\link{region_rsources}} for region mapping, \code{\link{region_sources}} for filtering function
#' @examples
#' # View all regional sources
#' rsources
#' 
#' # Count regional sources
#' length(rsources)
#' 
#' # Get regional sources for a specific country
#' region_sources("Nigeria")
#' 
#' # Check which regions use each source
#' for (region in names(region_rsources)) {
#'   cat(region, ":", length(region_rsources[[region]]), "sources\n")
#' }
#' @export
rsources = c("africanews.com.csv", "asia.nikkei.com.csv","asiatimes.com.csv","balkaninsight.com.csv",
             "cnnespanol.cnn.com.csv","euronews.com.csv",
             "indiatimes.com.csv", "iwpr.net.csv", "neweasterneurope.eu.csv", "timesofindia.indiatimes.com.csv",
             "telemundo.com.csv", "theeastafrican.co.ke.csv")

#' Region-to-Regional-Sources Mapping
#'
#' A named list mapping geographic regions to their relevant regional news sources.
#' This mapping is used by the \code{\link{region_sources}} function to filter
#' regional sources based on geographic relevance when \code{use_region_filter = TRUE}.
#'
#' @format A named list with 5 elements:
#' \describe{
#'   \item{EE_CA}{Eastern Europe/Central Asia sources (4 sources)}
#'   \item{MENA}{Middle East/North Africa sources (2 sources)}
#'   \item{LAC}{Latin America/Caribbean sources (2 sources)}
#'   \item{EA}{East Asia sources (4 sources)}
#'   \item{SSA}{Sub-Saharan Africa sources (2 sources)}
#' }
#'
#' @details This mapping ensures that only geographically relevant regional sources
#' are included for each country. For example, countries in Sub-Saharan Africa will
#' only include Africanews and The East African, not CNN en Español.
#'
#' @source MLP (Machine Learning for Peace) project source validation
#' @seealso \code{\link{rsources}} for complete regional source list, \code{\link{region_sources}} for filtering function, \code{\link{country_regions}} for country-region mapping
#' @examples
#' # View all region-source mappings
#' region_rsources
#' 
#' # Get sources for Sub-Saharan Africa
#' region_rsources$SSA
#' 
#' # Count sources by region
#' sapply(region_rsources, length)
#' 
#' # Check which regions use a specific source
#' source_to_find <- "euronews.com.csv"
#' regions_using_source <- names(region_rsources)[sapply(region_rsources, function(x) source_to_find %in% x)]
#' cat("Regions using", source_to_find, ":", paste(regions_using_source, collapse = ", "))
#' @export
region_rsources <- list(
  "EE_CA" = c("balkaninsight.com.csv", "euronews.com.csv", "iwpr.net.csv", "neweasterneurope.eu.csv"),
  "MENA" = c("africanews.com.csv", "theeastafrican.co.ke.csv"),
  "LAC" = c("cnnespanol.cnn.com.csv", "telemundo.com.csv"),
  "EA" = c("asia.nikkei.com.csv", "asiatimes.com.csv", "indiatimes.com.csv", "timesofindia.indiatimes.com.csv"),
  "SSA" = c("africanews.com.csv", "theeastafrican.co.ke.csv")
)

#' Get Regional Sources for Country
#'
#' Returns appropriate regional sources for a given country based on its region.
#' This function looks up which region a country belongs to and returns only
#' the regional sources relevant to that geographic area.
#'
#' @param country Character string. Name of country to look up regional sources for.
#' @return Character vector of regional source filenames (CSV files) relevant to the country's region.
#' @details This function uses the \code{country_regions} and \code{region_rsources}
#' mappings to determine which regional sources are geographically relevant.
#' If a country is not found in the region mappings, it returns an empty vector
#' and issues a warning.
#' @seealso \code{\link{country_regions}} for country-region mapping, \code{\link{region_rsources}} for region-source mapping
#' @examples
#' # Get regional sources for Nigeria (SSA region)
#' region_sources("Nigeria")
#' 
#' # Get regional sources for Ukraine (EE_CA region)
#' region_sources("Ukraine")
#' 
#' # Get regional sources for Colombia (LAC region)
#' region_sources("Colombia")
#' 
#' # Check what happens with invalid country
#' # region_sources("InvalidCountry") # Returns empty vector with warning
#' @export
region_sources <- function(country) {
  # Find which region the country belongs to
  country_region <- NULL
  for (region in names(country_regions)) {
    if (country %in% country_regions[[region]]) {
      country_region <- region
      break
    }
  }
  
  if (is.null(country_region)) {
    warning(sprintf("Country '%s' not found in region mappings", country))
    return(character(0))
  }
  
  # Return regional sources for that region
  region_rsources[[country_region]] %||% character(0)
}

#' Get Local Sources for Country
#'
#' Returns country-specific local news sources for a given country. Some countries
#' have sources differentiated by independence status (independent vs state-controlled)
#' based on media freedom indicators.
#'
#' @param country Character string. Name of country to look up local sources for.
#' @return Named list containing local source information:
#' \describe{
#'   \item{lsources}{Character vector of all local source CSV filenames}
#'   \item{ind_sources}{Character vector of independent source names (without .csv), if applicable}
#'   \item{state_sources}{Character vector of state-controlled source names (without .csv), if applicable}
#' }
#' @details Countries where more than 50% of media volume came from non-independent
#' sources and RSF (Reporters Sans Frontières) score >= 100 have sources differentiated
#' by independence status. These countries previously used weighting in analysis.
#' Sources are stored as CSV filenames in the lsources field, while independence
#' classifications use source names without the .csv extension.
#' @seealso \code{\link{isources}} for international sources, \code{\link{rsources}} for regional sources
#' @examples
#' # Get local sources for Nigeria (no independence differentiation)
#' local_source_select("Nigeria")
#' 
#' # Get local sources for Zimbabwe (has independence differentiation)
#' zim_sources <- local_source_select("Zimbabwe")
#' zim_sources$lsources  # All sources
#' zim_sources$ind_sources  # Independent sources only
#' zim_sources$state_sources  # State-controlled sources only
#' 
#' # Check which countries have independence differentiation
#' countries_with_ind <- c()
#' for (country in countries) {
#'   sources <- local_source_select(country)
#'   if ("ind_sources" %in% names(sources)) {
#'     countries_with_ind <- c(countries_with_ind, country)
#'   }
#' }
#' countries_with_ind
#' @export
local_source_select <- function(country){
  dat <- list(
    "Kenya" = list(
      lsources = c("kbc.co.ke.csv", "citizen.digital.csv", "nation.africa.csv", "theeastafrican.co.ke.csv") 
    ),
    "Nigeria" = list(
      lsources = c("guardian.ng.csv", "thenewsnigeria.com.ng.csv", "vanguardngr.com.csv", "thenationonlineng.net.csv")
    ),
    "Zimbabwe" = list(
      lsources = c("thestandard.co.zw.csv", "theindependent.co.zw.csv", "herald.co.zw.csv", "chronicle.co.zw.csv", "newsday.co.zw.csv", "thezimbabwean.co.csv",
                   "zimbabwesituation.com.csv", "newzimbabwevision.com.csv", "zimlive.com.csv"),
      ind_sources = c("thestandard.co.zw", "theindependent.co.zw", "newsday.co.zw"),
      state_sources = c("herald.co.zw", "chronicle.co.zw")

    ),
    "Albania" = list(
      lsources = c("gazetatema.net.csv", "panorama.com.al.csv", "telegraf.al.csv") 
    ),
    "Colombia" = list(
      lsources = c("elcolombiano.com.csv", "elespectador.com.csv", "elheraldo.co.csv", "eltiempo.com.csv")
    ),
    "Ukraine" = list(
      lsources = c("delo.ua.csv", "interfax.com.ua.csv", "kp.ua.csv", "pravda.com.ua.csv", "kyivpost.com.csv", "kyivindependent.com.csv") # Kyiv post only old data
    ),
    "Serbia" = list(
      lsources = c("rs.n1info.com.csv", "juznevesti.com.csv", "insajder.net.csv", "danas.rs.csv", "balkaninsight.com.csv")
    ),
    "Uganda" = list(
      lsources = c("monitor.co.ug.csv", "observer.ug.csv", "newvision.co.ug.csv", "nilepost.co.ug.csv", "sunrise.ug.csv", "eagle.co.ug.csv")
    ),
    "Benin" = list(
      lsources = c("lanouvelletribune.info.csv", "news.acotonou.com.csv", "lematinal.media.csv", "levenementprecis.com.csv") 
    ),
    "Morocco" = list(
      lsources = c("leconomiste.com.csv","lematin.ma.csv", "assabah.ma.csv")
    ),
    "Ethiopia" = list(
      lsources = c("addisfortune.news.csv", "addisstandard.com.csv", "capitalethiopia.com.csv", "thereporterethiopia.com.csv", "ethiopianmonitor.com.csv",
                   "addisadmassnews.com.csv")
    ),
    "Georgia" = list(
      lsources = c("ambebi.ge.csv", "georgiatoday.ge.csv")
    ),
    "Senegal" = list(
      lsources = c( "xalimasn.com.csv", "lesoleil.sn.csv", "enqueteplus.com.csv", "lasnews.sn.csv", "ferloo.com.csv", "nouvelobs.com.csv", "sudquotidien.sn.csv")
    ),
    "Tanzania" = list(
      lsources = c("ippmedia.com.csv", "dailynews.co.tz.csv","habarileo.co.tz.csv","thecitizen.co.tz.csv", "mtanzania.co.tz.csv", "jamhurimedia.co.tz.csv",
                   "mzalendo.co.tz.csv")
    ),
    "Ecuador" = list(
      lsources = c("elcomercio.com.csv", "eldiario.ec.csv", "elnorte.ec.csv", "eluniverso.com.csv", "metroecuador.com.ec.csv")
    ),
    "Mali" = list(
      lsources = c("maliweb.net.csv", "malijet.com.csv", "news.abamako.com.csv") 
    ),
    "Zambia" = list(
      lsources = c("lusakatimes.com.csv","mwebantu.com.csv", "diggers.news.csv", "openzambia.com.csv", "lusakavoice.com.csv", "dailynationzambia.com.csv", "zambianewsnetwork.com.csv", "zambianobserver.com.csv")
    ),
    "Kosovo" = list(
      lsources = c("kosova-sot.info.csv", "balkaninsight.com.csv", "prishtinainsight.com.csv", "botasot.info.csv")
    ),
    "Mauritania" = list(
      lsources = c("alwiam.info.csv","lecalame.info.csv","journaltahalil.com.csv", "alakhbar.info.csv", "saharamedias.net.csv")
    ),
    "Paraguay" = list(
      lsources = c("abc.com.py.csv","lanacion.com.py.csv","ultimahora.com.csv"),
      ind_sources = c("abc.com.py"),
      state_sources = c("lanacion.com.py", "ultimahora.com")

    ),
    "Niger" = list(
      lsources = c("actuniger.com.csv","nigerinter.com.csv","lesahel.org.csv", "tamtaminfo.com.csv", "airinfoagadez.com.csv",
                   "nigerexpress.info.csv", "journalduniger.com.csv") 
    ),
    "Jamaica" = list(
      lsources = c("jamaica-gleaner.com.csv","jamaicaobserver.com.csv")
    ),
    "Honduras" = list(
      lsources = c("elheraldo.hn.csv","laprensa.hn.csv", "proceso.hn.csv", "tiempo.hn.csv")
    ),
    "Rwanda" = list(
      lsources = c("newtimes.co.rw.csv",  "therwandan.com.csv", "kigalitoday.com.csv", "umuseke.rw.csv")
    ),
    "Ghana" = list(
      lsources = c( "dailyguidenetwork.com.csv", "ghanaweb.com.csv", "graphic.com.gh.csv", "newsghana.com.gh.csv")
    ),
    "Philippines" = list(
      lsources = c("mb.com.ph.csv","manilastandard.net.csv", "inquirer.net.csv", "manilatimes.net.csv"),
      ind_sources = c("inquirer.net"),
      state_sources = c("mb.com.ph","manilastandard.net", "manilatimes.net")
    ),
    "Guatemala" = list(
      lsources = c("prensalibre.com.csv","republica.gt.csv", "lahora.gt.csv", "soy502.com.csv")
    ),
    "Belarus" = list(
      lsources = c("nashaniva.by.csv", "novychas.by.csv", "nv-online.info.csv", "belgazeta.by.csv", "zviazda.by.csv", "sb.by.csv"),
      ind_sources = c("nashaniva.by", "novychas.by", "nv-online.info", "belgazeta.by"),
      state_sources = c("zviazda.by", "sb.by")
    ),
    "DR Congo" = list(
      lsources = c("radiookapi.net.csv", "lesoftonline.net.csv","acpcongo.com.csv", "lephareonline.net.csv", "groupelavenir.org.csv", "matininfos.net.csv",
                   "cas-info.ca.csv", "actualite.cd.csv", "7sur7.cd.csv")
    ),
    "Cambodia" = list(
      lsources = c("kohsantepheapdaily.com.kh.csv","moneaksekar.com.csv", "phnompenhpost.com.csv", "cambodiadaily.com.csv")
    ),
    "Turkey" = list(
      lsources = c("diken.com.tr.csv", "t24.com.tr.csv","sozcu.com.tr.csv", "posta.com.tr.csv", "sabah.com.tr.csv")
    ),
    "El Salvador" = list(
      lsources = c("laprensagrafica.com.csv","elfaro.net.csv", "elsalvador.com.csv", "diario.elmundo.sv.csv", "diarioelsalvador.com.csv", "revistafactum.com.csv",
                   "gatoencerrado.news.csv") #, "mala-yerba.com.csv"
    ),
    "South Africa" = list(
      lsources = c("timeslive.co.za.csv","news24.com.csv", "dailysun.co.za.csv", "sowetanlive.co.za.csv", "isolezwe.co.za.csv", "iol.co.za.csv", "son.co.za.csv")
    ),
    "Bangladesh" = list(
      lsources = c("prothomalo.com.csv","bd-pratidin.com.csv", "kalerkantho.com.csv", "jugantor.com.csv", "dailyjanakantha.com.csv") #
    ),
    "Tunisia" = list(
      lsources = c("assarih.com.csv","babnet.net.csv", "jomhouria.com.csv", "lapresse.tn.csv")
    ),
    "Nicaragua" = list(
      lsources = c("confidencial.com.ni.csv","laprensani.com.csv", "nuevaya.com.ni.csv", "articulo66.com.csv", "laverdadnica.com.csv", "ondalocalni.com.csv",
                   "canal2tv.com.csv", "lajornadanet.com.csv")
    ),
    "Indonesia" = list(
      lsources = c("thejakartapost.com.csv","jawapos.com.csv", "kompas.com.csv", "mediaindonesia.com.csv", "sindonews.com.csv", "beritasatu.com.csv", "hariansib.com.csv")
    ),
    "Armenia" = list(
      lsources = c("azatutyun.am.csv", "aravot.am.csv", "168.am.csv", "1in.am.csv", "golosarmenii.am.csv")
    ),
    "Angola" = list(
      lsources = c("opais.co.ao.csv","jornalf8.net.csv", "angola24horas.com.csv", "portaldeangola.com.csv", "angola-online.net.csv", "vozdeangola.com.csv", "jornaldeangola.ao.csv"),
      ind_sources = c("opais.co.ao","jornalf8.net", "angola24horas.com", "portaldeangola.com", "angola-online.net", "vozdeangola.com"),
      state_sources = c("jornaldeangola.ao")
    ),
    "Sri Lanka" = list(
      lsources = c("dailymirror.lk.csv","island.lk.csv", "divaina.lk.csv", "adaderana.lk.csv", "lankadeepa.lk.csv"),
      ind_sources = c("adaderana.lk"),
      state_sources = c("dailymirror.lk","island.lk", "divaina.lk", "lankadeepa.lk")
    ),
    "Hungary" = list(
      lsources = c("index.hu.csv", "24.hu.csv", "168.hu.csv", "hvg.hu.csv", "demokrata.hu.csv") 
    ),
    "Cameroon" = list(
      lsources = c("journalducameroun.com.csv", "camerounweb.com.csv", "237actu.com.csv", "237online.com.csv", "cameroonvoice.com.csv", "lebledparle.com.csv", "thesunnewspaper.cm.csv") 
    ),
    "Malaysia" = list(
      lsources = c("malaymail.com.csv", "nst.com.my.csv", "thestar.com.my.csv", "utusan.com.my.csv", "thesun.my.csv", "malaysiakini.com.csv"),
      ind_sources = c("malaymail.com", "nst.com.my", "malaysiakini.com"),
      state_sources = c("thestar.com.my", "utusan.com.my", "thesun.my")
    ),
    "Malawi" = list(
      lsources = c("mwnation.com.csv", "nyasatimes.com.csv", "times.mw.csv", "faceofmalawi.com.csv", "malawivoice.com.csv") 
    ),
    "Uzbekistan" = list(
      lsources = c("fergana.ru.csv", "kun.uz.csv", "gazeta.uz.csv","podrobno.uz.csv","batafsil.uz.csv","sof.uz.csv","anhor.uz.csv","asiaterra.info.csv","daryo.uz.csv") 
    ),
    "Mozambique" = list(
      lsources = c("correiodabeiraserra.com.csv", "canal.co.mz.csv", "mmo.co.mz.csv", "cartamz.com.csv", "verdade.co.mz.csv", "clubofmozambique.com.csv", "portalmoznews.com.csv", "jornaldomingo.co.mz.csv", "tvm.co.mz.csv") 
    ),
    "India" = list(
      lsources = c("amarujala.com.csv", "indianexpress.com.csv", "thehindu.com.csv", "hindustantimes.com.csv", "deccanherald.com.csv", "firstpost.com.csv", "indiatimes.com.csv", "timesofindia.indiatimes.com.csv"),
      ind_sources = c("indianexpress.com", "thehindu.com", "deccanherald.com"),
      state_sources = c("amarujala.com", "firstpost.com", "hindustantimes.com", "indiatimes.com", "timesofindia.indiatimes.com")
    ),
    "Azerbaijan" = list(
      lsources = c("azeritimes.com.csv", "azadliq.info.csv", "abzas.org.csv", "turan.az.csv", "zerkalo.az.csv",
                   "mikroskopmedia.com.csv", "xalqcebhesi.az.csv", "musavat.com.csv", "ru.echo.az.csv") 
    ),
    "Kyrgyzstan" = list(
      lsources = c("akipress.com.csv", "24.kg.csv", "kloop.kg.csv", "super.kg.csv", "vb.kg.csv", "kaktus.kg.csv", "kaktus.media.csv") 
    ),
    "Kazakhstan" = list(
      lsources = c("caravan.kz.csv", "diapazon.kz.csv", "kaztag.kz.csv", "rus.azattyq.org.csv") 
    ),
    "Peru" = list(
      lsources = c("elcomercio.pe.csv", "gestion.pe.csv", "larepublica.pe.csv", "ojo-publico.com.csv", "idl-reporteros.pe.csv") 
    ),
    "Moldova" = list(
      lsources = c("timpul.md.csv", "tribuna.md.csv", "unimedia.info.csv", "voceabasarabiei.md.csv", "publika.md.csv", "ipn.md.csv", "zdg.md.csv") 
    ),
    "Macedonia" = list(
      lsources = c("koha.mk.csv", "slobodenpecat.mk.csv", "makfax.com.mk.csv", "skopjediem.com.csv", "novamakedonija.com.mk.csv")
    ),
    "Dominican Republic" = list (
      lsources = c("diariolibre.com.csv", "listindiario.com.csv", "elnacional.com.do.csv", "hoy.com.do.csv", "elcaribe.com.do.csv", "elviajero.com.do.csv")
    ),
    "Algeria" = list(
      lsources = c("twala.info.csv", "24hdz.com.csv", "echoroukonline.com.csv", "elkhabar.com.csv", "el-massa.com.csv", "elwatan-dz.com.csv", "echaab.dz.csv")
    ),
    "South Sudan" = list(
      lsources = c("radiotamazuj.org.csv", "sudantribune.com.csv", "paanluelwel.com.csv", "onecitizendaily.com.csv", "eyeradio.org.csv") 
    ),
    "Liberia" = list(
      lsources = c("thenewdawnliberia.com.csv", "liberianobserver.com.csv", "analystliberiaonline.com.csv", "frontpageafricaonline.com.csv", "inquirernewspaper.com.csv", "thenewsnewspaper.online.csv")
    ),
    "Pakistan" = list(
      lsources = c("jang.com.pk.csv", "nation.com.pk.csv", "dailytimes.com.pk.csv", "pakobserver.net.csv", "tribune.com.pk.csv")
    ),
    "Nepal" = list(
      lsources = c("onlinekhabar.com.csv", "english.onlinekhabar.com.csv", "en.setopati.com.csv", "thehimalayantimes.com.csv", "kathmandupost.com.csv", "nepalitimes.com.csv")
    ),
    "Namibia" = list(
      lsources = c("namibian.com.na.csv", "confidentenamibia.com.csv", "thevillager.com.na.csv", "observer24.com.na.csv", "informante.web.na.csv")
    ),
    "Burkina Faso" = list(
      lsources = c("lefaso.net.csv", "burkina24.com.csv", "evenement-bf.net.csv", "laborpresse.net.csv")
    ),
    "Timor Leste" = list(
      lsources = c("thediliweekly.com.csv")
    ),
    "Solomon Islands" = list(
      lsources = c("solomonstarnews.com.csv", "solomontimes.com.csv", "sibconline.com.sb.csv")
    ),
    "Costa Rica" = list(
      lsources = c("larepublica.net.csv", "news.co.cr.csv", "ticotimes.net.csv", "diarioextra.com.csv")
    ),
    "Panama" = list(
      lsources = c("elsiglo.com.pa.csv", "critica.com.pa.csv", "panamaamerica.com.pa.csv", "newsroompanama.com.csv")
    ),
    "Mexico" = list(
      lsources = c("elsureste.com.mx.csv", "caribepeninsular.mx.csv", "nortedigital.mx.csv", "milenio.com.csv", "heraldodemexico.com.mx.csv", "excelsior.com.mx.csv")
    )
    
  )

  if (!country %in% names(dat)) {
    stop(sprintf("No local source data for '%s'", country))
  }

  dat[[country]]
}

#' Get Last Valid Month for Country
#'
#' Returns the last month with valid data for a given country in the MLP dataset.
#' As of the final update, all countries have data through December 1, 2024.
#' This function is used to filter datasets to exclude months with incomplete
#' or invalid data.
#'
#' @param country Character string. Name of the country to look up.
#' @return Character string in "YYYY-MM-DD" format representing the last valid month ("2024-12-01" for all countries).
#' @details This function uses a simple lookup approach with all countries currently
#' set to "2024-12-01". The function could be refactored to use a lookup table
#' if different countries have different end dates in the future.
#' @examples
#' # Get last valid month for Nigeria
#' country_last_month("Nigeria")
#' 
#' # Get last valid month for Ukraine
#' country_last_month("Ukraine")
#' 
#' # Use in data filtering
#' # df <- df[df$date <= country_last_month("Kenya"), ]
#' @export
country_last_month <- function(country){
  if(country %in% "Kenya"){
    last_month = "2024-12-01"
  } else if(country %in% "Nigeria"){
    last_month = "2024-12-01"
  } else if(country %in% "Zimbabwe"){
    last_month = "2024-12-01"
  } else if(country %in% "Albania"){
    last_month = "2024-12-01"
  } else if(country %in% "Colombia"){
    last_month = "2024-12-01"
  } else if(country %in% "Ukraine"){
    last_month = "2024-12-01"
  } else if(country %in% "Serbia"){
    last_month = "2024-12-01"
  } else if(country %in% "Uganda"){
    last_month = "2024-12-01"
  } else if(country %in% "Benin"){
    last_month = "2024-12-01"
  } else if(country %in% "Morocco"){
    last_month = "2024-12-01"
  } else if(country %in% "Ethiopia"){
    last_month = "2024-12-01"
  } else if(country %in% "Georgia"){
    last_month = "2024-12-01"
  } else if(country %in% "Senegal"){
    last_month = "2024-12-01"
  } else if(country %in% "Tanzania"){
    last_month = "2024-12-01"
  } else if(country %in% "Ecuador"){
    last_month = "2024-12-01"
  } else if(country %in% "Mali"){
    last_month = "2024-12-01"
  } else if(country %in% "Zambia"){
    last_month = "2024-12-01"
  } else if(country %in% "Kosovo"){
    last_month = "2024-12-01"
  } else if(country %in% "Mauritania"){
    last_month = "2024-12-01"
  } else if(country %in% "Paraguay"){
    last_month = "2024-12-01"
  } else if(country %in% "Niger"){
    last_month = "2024-12-01"
  } else if(country %in% "Jamaica"){
    last_month = "2024-12-01"
  } else if(country %in% "Honduras"){
    last_month = "2024-12-01"
  } else if(country %in% "Rwanda"){
    last_month = "2024-12-01"
  } else if(country %in% "Ghana"){
    last_month = "2024-12-01"
  } else if(country %in% "Philippines"){
    last_month = "2024-12-01"
  } else if(country %in% "Guatemala"){
    last_month = "2024-12-01"
  } else if(country %in% "Belarus"){
    last_month = "2024-12-01"
  } else if(country %in% "Cambodia"){
    last_month = "2024-12-01"
  } else if(country %in% "DR Congo"){
    last_month = "2024-12-01"
  } else if(country %in% "Turkey"){
    last_month = "2024-12-01"
  } else if(country %in% "El Salvador"){
    last_month = "2024-12-01"
  } else if(country %in% "South Africa"){
    last_month = "2024-12-01"
  } else if(country %in% "Bangladesh"){
    last_month = "2024-12-01"
  } else if(country %in% "Tunisia"){
    last_month = "2024-12-01"
  } else if(country %in% "Nicaragua"){
    last_month = "2024-12-01"
  } else if(country %in% "Indonesia"){
    last_month = "2024-12-01"
  } else if(country %in% "Armenia"){
    last_month = "2024-12-01"
  } else if(country %in% "Angola"){
    last_month = "2024-12-01"
  } else if(country %in% "Sri Lanka"){
    last_month = "2024-12-01"
  } else if(country %in% "Hungary"){
    last_month = "2024-12-01"
  } else if(country %in% "Cameroon"){
    last_month = "2024-12-01"
  } else if(country %in% "Malaysia"){
    last_month = "2024-12-01"
  } else if(country %in% "Malawi"){
    last_month = "2024-12-01"
  } else if(country %in% "Uzbekistan"){
    last_month = "2024-12-01"
  } else if(country %in% "Mozambique"){
    last_month = "2024-12-01"
  } else if(country %in% "India"){
    last_month = "2024-12-01"
  } else if(country %in% "Azerbaijan"){
    last_month = "2024-12-01"
  } else if(country %in% "Kyrgyzstan"){
    last_month = "2024-12-01"
  } else if(country %in% "Kazakhstan"){
    last_month = "2024-12-01"
  } else if(country %in% "Peru"){
    last_month = "2024-12-01"
  } else if(country %in% "Moldova"){
    last_month = "2024-12-01"
  } else if (country %in% "Dominican Republic"){
    last_month = "2024-12-01"
  } else if(country %in% "Macedonia"){
    last_month = "2024-12-01"
  } else if(country %in% "Algeria"){
    last_month = "2024-12-01"
  } else if(country %in% "South Sudan"){
    last_month = "2024-12-01"
  } else if(country %in% "Liberia"){
    last_month = "2024-12-01"
  } else if(country %in% "Pakistan"){
    last_month = "2024-12-01"
  } else if(country %in% "Nepal"){
    last_month = "2024-12-01"
  } else if(country %in% "Namibia"){
    last_month = "2024-12-01"
  } else if(country %in% "Burkina Faso"){
    last_month = "2024-12-01"
  } else if(country %in% "Timor Leste"){
    last_month = "2024-12-01"
  } else if(country %in% "Solomon Islands"){
    last_month = "2024-12-01"
  } else if(country %in% "Costa Rica"){
    last_month = "2024-12-01"
  } else if(country %in% "Panama"){
    last_month = "2024-12-01"
  }else if(country %in% "Mexico"){
     last_month = "2024-12-01"
  }
  invisible(last_month)
}

