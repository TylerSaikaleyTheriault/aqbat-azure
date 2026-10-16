summary_ui <- function(i18n) {
  fluidPage(
    fluidRow(
      column(12,
        class = "col-md-12",
        div(
          tags$section(
            style = "",
            class = "alert alert-info",
            # h2(i18n$t("Inactivity notice")),
            p(i18n$t("This tool works best on desktops and laptops. It is not optimized for tablets or mobile devices.")),
          ),
        ),
        h2(i18n$t("About the tool"), style = "margin-top: 0"),
        p(i18n$t("Use the Air Quality Benefits Assessment Tool (AQBAT) to:")),
        tags$div(
          tags$ul(
            tags$li(i18n$t("estimate the benefits (positive impacts) or damages (negative impacts) of proposed regulatory initiatives for outdoor air quality")),
            tags$li(
              i18n$t("define a wide range of scenarios by combining the following:"),
              tags$ul(
                tags$li(i18n$t("geographic areas at the census division level")),
                tags$li(i18n$t("scenario years (2001-2063)")),
                tags$li(i18n$t("pollutant concentration data")),
                tags$li(i18n$t("health endpoint valuations")),
                tags$li(i18n$t("Health Canada endorsed concentration-response functions"))
              ),
            ),
            tags$li(i18n$t("upload inputs, run specific scenarios and download outputs")),
            tags$li(i18n$t("conduct Monte Carlo simulations to examine the effects of uncertainties on estimated health impacts")),
          )
        )
      ),
      br(), br(),
      # column(12,
      #     class = "col-md-12 col-lg-6",
      #     div(
      #     style = "width: 100%;",
      #     img(
      #       src = "aqbat schematic.png",
      #       alt = i18n$t("This figure shows a flowchart of the steps involved in estimating health benefits using the Air Quality Benefits Assessment Tool (AQBAT). Any change in industrial emissions or emissions from other processes is inputted into AQBAT as the change in ambient air pollution concentrations. The ambient air pollution concentrations, together with population data, allows for estimates of the change in population exposure. Concentration response functions can then be applied with the change in population exposure to estimate the impact on population health effects. Lastly, endpoint valuations can be applied to the estimated change in population health effects to determine the value of health benefits or damages."),
      #       style = "width: 100%; height: auto; max-width: 585px;"
      #     )
      #   )
      # )
    ),
    h2(i18n$t("Quick start steps")),
    p(i18n$t("Use the \"CRFs\", \"Valuation\", and \"Pollutant data upload\" tabs to review and input data. View the output in the \"Results\" tab.")),
    # tags$ol(
    #   tags$li(i18n$t("Determine scenario year(s), and prepare, validate, and save pollutant data; see sample input file below - in the sample file, status quo concentrations are denoted by _2 (e.g. pm25_2) and counterfactual concentrations (e.g. natural background concentrations) are denoted by _1 (e.g. pm25_1); AQBAT uses the difference between status quo and counterfactual values to estimate health benefits or damages resulting from air quality changes.")),
    #   tags$li(i18n$t("Review concentration response functions, select the currency year, base year for discounting and discount rate in the Valuation tab, and change threshold concentrations if needed.")),
    #   tags$li(i18n$t("Upload the pollutant data by clicking the Pollutant Data Upload tab; you can upload multiple files e.g. for multiple years using the ctrl or shift keys.")),
    #   tags$li(i18n$t("Review and download tabular results and maps by clicking the Results tab."))
    # ),
    h2(i18n$t("Recent applications of AQBAT")),
    tags$ul(
      # Citations built with tags$a(..., lang=) so screen readers use correct language (same pattern as reference template)
      local({
        cur_lang <- i18n$get_translation_language()
        list(
          tags$li(
            HTML(i18n$t("Stieb DM, Smith-Doiron M, Quick M, Christidis T, Xi G, Miles RM, van Donkelaar A, Martin RV, Hystad P, Tjepkema M. ")),
            tags$a(target = "_parent", href = "https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2023GH000816", lang = "en", i18n$t("Inequality in the Distribution of Air Pollution Attributable Mortality Within Canadian Cities")),
            HTML(i18n$t(". Geohealth. 2023 Aug 29;7(9):e2023GH000816."))
          ),
          tags$li(
            HTML(i18n$t("Egyed M, Blagden P, Plummer D, Makar P, Matz CJ, Flannigan M, MacNeill M, Lavigne E, Ling B, Lopez, DV, Edwards B, Pavlovic R, Racine J, Raymond P, Rittmaster R, Wilson A, Xi G. 2022. Air Quality. In P. Berry & R. Schnitter (Eds.), ")),
            tags$a(target = "_parent", href = if (cur_lang == "en") "https://changingclimate.ca/health-in-a-changing-climate/chapter/5-0/" else "https://changingclimate.ca/health-in-a-changing-climate/fr/chapter/5-0/", lang = cur_lang, i18n$t("Health of Canadians in a Changing Climate: Advancing our Knowledge for Action")),
            HTML(i18n$t(". Ottawa, ON: Government of Canada."))
          ),
          tags$li(
            HTML(i18n$t("Matz CJ, Egyed M, Xi G, Racine J, Pavlovic R, Rittmaster R, Henderson SB, Stieb DM. ")),
            tags$a(target = "_parent", href = "https://doi.org/10.1016/j.scitotenv.2020.138506", lang = "en", i18n$t("Health impact analysis of PM2.5 from wildfire smoke in Canada (2013-2015, 2017-2018)")),
            HTML(i18n$t(". Sci Total Environ. 2020 Jul 10;725:138506. ")),
            tags$a(href = if (cur_lang == "en") "https://health-infobase.canada.ca/datalab/wildfire-blog.html" else "https://sante-infobase.canada.ca/labo-de-donnees/blogue-feux-de-friches.html", lang = cur_lang, i18n$t("See an interactive map of results"))
          ),
          tags$li(
            HTML(i18n$t("Health Canada. 2024. ")),
            tags$a(target = "_parent", href = if (cur_lang == "en") "https://www.canada.ca/en/health-canada/services/publications/healthy-living/health-impacts-air-pollution-2018.html" else "https://www.canada.ca/fr/sante-canada/services/publications/vie-saine/impacts-pollution-atmospherique-2018.html", lang = cur_lang, i18n$t("Health Impacts of Air Pollution in Canada in 2018")),
            "."
          )
        )
      })
    ),
    # Contact Us
    h2(i18n$t("Contact us")),
    span(i18n$t("Please contact aqbat-oebqa@hc-sc.gc.ca to ask a question, provide feedback or report a problem.")),
    br(), br(),
    tags$section(
      id = "",
      class = "panel panel-info",
      tags$header(
        class = "panel-heading",
        tags$h3(
          class = "panel-title",
          i18n$t("Disclaimer")
        )
      ),
      tags$div(
        class = "panel-body",
        tags$p(
          i18n$t("The Air Quality Benefit Assessment Tool (AQBAT) is intended to help estimate the health benefits or damages associated with a change in exposure to outdoor air pollution. Although Health Canada makes every effort to ensure the accuracy and reliability of the information provided by AQBAT, it is not intended as a substitute for professional advice. Health Canada assumes no responsibility for the use of this product or any information resulting from it.")
        )
      )
    )
  )
}
