
#
#   Constants or quasi-constants that might need periodic updating
#

#' @export
civic <- c("arrest", "censor", "electionactivity", "electionirregularities", "cooperate",
           "coup", "defamationcase", "disaster", "legalaction", "legalchange",
           "martiallaw", "mobilizesecurity", "corruption", "activism", "protest", "purge",
           "raid", "threaten", "violencelethal", "violencenonlethal")

#' @export
rai <- c("arms_transfer_security_aid_assistance", "bribery_economic_corruption",
         "diaspora_activation", "diplomatic_mediation", "diplomatic_recognition",
         "diplomatic_sanction", "diplomatic_statement", "diplomatic_visit",
         "economic_aid_assistance", "intelligence_counterintelligence",
         "investment", "joint_security_force_exercise", "media_campaign_intervention",
         "political_process_policy_intervention", "professional_cultural_exchange",
         "security_cooperation", "security_force_facility_presence",
         "surveillance", "tech_transfer_investment", "trade_agreement_exchange",
         "trade_financial_sanction", "transnational_organization_crime")

#' @export
cr_vars = c("arrest", "cooperate", "corruption", "defamationcase", "legalaction", "legalchange", "purge", "raid", "threaten", "violencelethal", "violencenonlethal")

## Define countries
#' @export
countries <- c("Albania", "Armenia", "Belarus", "Georgia", "Hungary", "Kosovo", "Serbia", "Azerbaijan", "Moldova", "Macedonia", "Dominican Republic",
               "Turkey", "Ukraine", "Uzbekistan", "Kyrgyzstan", "Kazakhstan", # EE/CA
               "Algeria", "Mali","Morocco","Niger", # MENA
               "Colombia", "Ecuador", "El Salvador", "Guatemala", "Honduras",
               "Jamaica", "Nicaragua", "Paraguay", "Peru",  # LAC
               "Bangladesh", "Cambodia", "India", "Indonesia", "Malaysia", "Nepal", "Pakistan", "Philippines", "Solomon Islands", "Sri Lanka", "Timor Leste", #EA
               "Angola", "Benin", "Burkina Faso", "Cameroon", "DR Congo", "Ethiopia", "Ghana", "Kenya", "Liberia", "Malawi",
               "Mauritania", "Mozambique", "Namibia", "Nigeria", "Rwanda", "Senegal", "South Africa", "South Sudan",
               "Tanzania", "Tunisia", "Uganda", "Zambia", "Zimbabwe") # SSA


## Usable sources

#' @describeIn whitelist_sources Vector of usable international sources.
#' @export
isources = c("aljazeera.com.csv", "bbc.com.csv", "csmonitor.com.csv", "france24.com.csv",
             "nytimes.com.csv", "reuters.com.csv", "scmp.com.csv", "theguardian.com.csv",
             "themoscowtimes.com.csv", "washingtonpost.com.csv", "wsj.com.csv",
             "lemonde.fr.csv", "liberation.fr.csv", "elpais.com.csv",
             "lefigaro.fr.csv", "xinhuanet.com.csv")

#' @describeIn whitelist_sources Vector of usable regional sources.
#' @export
rsources = c("africanews.com.csv", "asia.nikkei.com.csv","asiatimes.com.csv","balkaninsight.com.csv",
             "cnnespanol.cnn.com.csv","euronews.com.csv",
             "indiatimes.com.csv", "iwpr.net.csv", "neweasterneurope.eu.csv", "timesofindia.indiatimes.com.csv",
             "telemundo.com.csv", "theeastafrican.co.ke.csv")

#' @describeIn whitelist_sources Function that returns country-specific local sources.
#' @export
local_source_select <- function(country){
  dat <- list(
    "Kenya" = list(
      lsources = c("kbc.co.ke.csv", "citizen.digital.csv", "nation.africa.csv", "theeastafrican.co.ke.csv") #"businessdailyafrica.com.csv", "nation.co.ke.csv","citizentv.co.ke.csv"
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
      lsources = c("gazetatema.net.csv", "panorama.com.al.csv", "telegraf.al.csv") # Telegraf used to be out, but I added it back 9/26/22
    ),
    "Colombia" = list(
      lsources = c("elcolombiano.com.csv", "elespectador.com.csv", "elheraldo.co.csv", "eltiempo.com.csv")#, "portafolio.co.csv"
    ),
    "Ukraine" = list(
      lsources = c("delo.ua.csv", "interfax.com.ua.csv", "kp.ua.csv", "pravda.com.ua.csv", "kyivpost.com.csv", "kyivindependent.com.csv") # Kyiv post only old data
    ),
    "Serbia" = list(
      lsources = c("rs.n1info.com.csv", "juznevesti.com.csv", "insajder.net.csv", "danas.rs.csv", "balkaninsight.com.csv")
    ),
    "Uganda" = list(
      lsources = c("monitor.co.ug.csv", "observer.ug.csv", "newvision.co.ug.csv", "nilepost.co.ug.csv", "sunrise.ug.csv", "eagle.co.ug.csv") #  removed 9/26
    ),
    "Benin" = list(
      lsources = c("lanouvelletribune.info.csv", "news.acotonou.com.csv", "lematinal.media.csv", "levenementprecis.com.csv") #"leprogresinfo.net.csv", "beninwebtv.com.csv"
    ),
    "Morocco" = list(
      lsources = c("leconomiste.com.csv","lematin.ma.csv", "assabah.ma.csv")
    ),
    "Ethiopia" = list(
      lsources = c("addisfortune.news.csv", "addisstandard.com.csv", "capitalethiopia.com.csv", "thereporterethiopia.com.csv", "ethiopianmonitor.com.csv",
                   "addisadmassnews.com.csv") #, "yeroo.org.csv")
    ),
    "Georgia" = list(
      lsources = c("ambebi.ge.csv", "georgiatoday.ge.csv")#, "interpressnews.ge.csv")
    ),
    "Senegal" = list(
      lsources = c( "xalimasn.com.csv", "lesoleil.sn.csv", "enqueteplus.com.csv", "lasnews.sn.csv", "ferloo.com.csv", "nouvelobs.com.csv", "sudquotidien.sn.csv") #
    ),
    "Tanzania" = list(
      lsources = c("ippmedia.com.csv", "dailynews.co.tz.csv","habarileo.co.tz.csv","thecitizen.co.tz.csv", "mtanzania.co.tz.csv", "jamhurimedia.co.tz.csv",
                   "mzalendo.co.tz.csv")
    ),
    "Ecuador" = list(
      lsources = c("elcomercio.com.csv", "eldiario.ec.csv", "elnorte.ec.csv", "eluniverso.com.csv", "metroecuador.com.ec.csv")
    ),
    "Mali" = list(
      lsources = c("maliweb.net.csv", "malijet.com.csv", "news.abamako.com.csv") #"lessor.site.csv", "malijet.com.csv"; "news.abamako.com.csv" added 9/26
    ),
    "Zambia" = list(
      lsources = c("lusakatimes.com.csv","mwebantu.com.csv", "diggers.news.csv", "openzambia.com.csv", "lusakavoice.com.csv", "dailynationzambia.com.csv", "zambianewsnetwork.com.csv", "zambianobserver.com.csv")
      # ind_sources = c("mwebantu.com"),
      # state_sources = c("lusakatimes.com")
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
                   "nigerexpress.info.csv", "journalduniger.com.csv") #"iciniger.com.csv",
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
      #lsources = c("mb.com.ph.csv","manilastandard.net.csv", "inquirer.net.csv", "manilatimes.net.csv")
      lsources = c("mb.com.ph.csv","manilastandard.net.csv", "inquirer.net.csv", "manilatimes.net.csv"),
      ind_sources = c("inquirer.net"),
      state_sources = c("mb.com.ph","manilastandard.net", "manilatimes.net")
    ),
    "Guatemala" = list(
      lsources = c("prensalibre.com.csv","republica.gt.csv", "lahora.gt.csv", "soy502.com.csv")
    ),
    "Belarus" = list(
      lsources = c("nashaniva.by.csv", "novychas.by.csv", "nv-online.info.csv", "belgazeta.by.csv", "zviazda.by.csv", "sb.by.csv"),
      #lsources = c("nashaniva.by.csv", "novychas.by.csv", "zviazda.by.csv", "sb.by.csv") # "nv-online.info.csv", "belgazeta.by.csv" removed 9/26
      ind_sources = c("nashaniva.by", "novychas.by", "nv-online.info", "belgazeta.by"),
      state_sources = c("zviazda.by", "sb.by")
    ),
    "DR Congo" = list(
      lsources = c("radiookapi.net.csv", "lesoftonline.net.csv","acpcongo.com.csv", "lephareonline.net.csv", "groupelavenir.org.csv", "matininfos.net.csv",
                   "cas-info.ca.csv", "actualite.cd.csv", "7sur7.cd.csv")
      # ind_sources = c("radiookapi.net"),
      # state_sources = c("lesoftonline.net","acpcongo.com", "lephareonline.net", "groupelavenir.org") # groupelavenir.org is only old data
    ),
    "Cambodia" = list(
      lsources = c("kohsantepheapdaily.com.kh.csv","moneaksekar.com.csv", "phnompenhpost.com.csv", "cambodiadaily.com.csv")
    ),
    "Turkey" = list(
      lsources = c("diken.com.tr.csv", "t24.com.tr.csv","sozcu.com.tr.csv", "posta.com.tr.csv", "sabah.com.tr.csv")
    ),
    "El Salvador" = list(
      lsources = c("laprensagrafica.com.csv","elfaro.net.csv", "elsalvador.com.csv", "diario.elmundo.sv.csv", "diarioelsalvador.com.csv", "revistafactum.com.csv",
                   "gatoencerrado.news.csv")
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
      #lsources = c("dailymirror.lk.csv","island.lk.csv", "divaina.lk.csv", "adaderana.lk.csv", "lankadeepa.lk.csv") # added 9/26
      lsources = c("dailymirror.lk.csv","island.lk.csv", "divaina.lk.csv", "adaderana.lk.csv", "lankadeepa.lk.csv"),
      ind_sources = c("adaderana.lk"),
      state_sources = c("dailymirror.lk","island.lk", "divaina.lk", "lankadeepa.lk")
    ),
    "Hungary" = list(
      lsources = c("index.hu.csv", "24.hu.csv", "168.hu.csv", "hvg.hu.csv", "demokrata.hu.csv") # added 10/25
    ),
    "Cameroon" = list(
      lsources = c("journalducameroun.com.csv", "camerounweb.com.csv", "237actu.com.csv", "237online.com.csv", "cameroonvoice.com.csv", "lebledparle.com.csv", "thesunnewspaper.cm.csv") # added 10/25
    ),
    "Malaysia" = list(
      lsources = c("malaymail.com.csv", "nst.com.my.csv", "thestar.com.my.csv", "utusan.com.my.csv", "thesun.my.csv", "malaysiakini.com.csv"),
      ind_sources = c("malaymail.com", "nst.com.my", "malaysiakini.com"),
      state_sources = c("thestar.com.my", "utusan.com.my", "thesun.my")
    ),
    "Malawi" = list(
      lsources = c("mwnation.com.csv", "nyasatimes.com.csv", "times.mw.csv", "faceofmalawi.com.csv", "malawivoice.com.csv") # added 10/25
    ),
    "Uzbekistan" = list(
      lsources = c("fergana.ru.csv", "kun.uz.csv", "gazeta.uz.csv","podrobno.uz.csv","batafsil.uz.csv","sof.uz.csv","anhor.uz.csv","asiaterra.info.csv","daryo.uz.csv") # added 11/21
    ),
    "Mozambique" = list(
      lsources = c("correiodabeiraserra.com.csv", "canal.co.mz.csv", "mmo.co.mz.csv", "cartamz.com.csv", "verdade.co.mz.csv", "clubofmozambique.com.csv", "portalmoznews.com.csv", "jornaldomingo.co.mz.csv", "tvm.co.mz.csv") # added 1/24
    ),
    "India" = list(
      lsources = c("amarujala.com.csv", "indianexpress.com.csv", "thehindu.com.csv", "hindustantimes.com.csv", "deccanherald.com.csv", "firstpost.com.csv", "indiatimes.com.csv", "timesofindia.indiatimes.com.csv"),
      ind_sources = c("indianexpress.com", "thehindu.com", "deccanherald.com"),
      state_sources = c("amarujala.com", "firstpost.com", "hindustantimes.com", "indiatimes.com", "timesofindia.indiatimes.com")
    ),
    "Azerbaijan" = list(
      lsources = c("azeritimes.com.csv", "azadliq.info.csv", "abzas.org.csv", "turan.az.csv", "zerkalo.az.csv",
                   "mikroskopmedia.com.csv", "xalqcebhesi.az.csv", "musavat.com.csv", "ru.echo.az.csv") # added 3/15 #"ru.echo.az.csv",
    ),
    "Kyrgyzstan" = list(
      lsources = c("akipress.com.csv", "24.kg.csv", "kloop.kg.csv", "super.kg.csv", "vb.kg.csv", "kaktus.kg.csv", "kaktus.media.csv") # added 4/10
    ),
    "Kazakhstan" = list(
      lsources = c("caravan.kz.csv", "diapazon.kz.csv", "kaztag.kz.csv", "rus.azattyq.org.csv") # added 5/19
    ),
    "Peru" = list(
      lsources = c("elcomercio.pe.csv", "gestion.pe.csv", "larepublica.pe.csv", "ojo-publico.com.csv", "idl-reporteros.pe.csv") # added 5/19
    ),
    "Moldova" = list(
      lsources = c("timpul.md.csv", "tribuna.md.csv", "unimedia.info.csv", "voceabasarabiei.md.csv", "publika.md.csv", "ipn.md.csv", "zdg.md.csv") # added 5/19
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
      lsources = c("radiotamazuj.org.csv", "sudantribune.com.csv", "paanluelwel.com.csv", "onecitizendaily.com.csv", "eyeradio.org.csv") #, "southsudanliberty.com.csv", "cityreviewss.com.csv"
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
    )


  )

  if (!country %in% names(dat)) {
    stop(sprintf("No local source data for '%s'", country))
  }

  dat[[country]]
}

# Identify the most recent month for each country; with final update, all countries are valid through `2024-12-01`
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
  }
  invisible(last_month)
}

