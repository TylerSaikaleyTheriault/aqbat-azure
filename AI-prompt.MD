I am working on an R Shiny app. I am using the i18n package.

When I give you something like this:

            fluidRow(
                  column(4, textInput("o3source1", "Source", value = "Burnett et al. 2004")),
                  column(4, selectInput("o3_rtype1", "Regression type", c("log-linear", "linear"))),
                  column(4, numericInput("o3_crf1", "HR/RR/OR", 1.0084, min = 1, step = 0.001, max = 5)),
                ),

I want you to give me back the same object, but with i18n being used, like this

          # 
          tags$li(HTML(i18n$t("The <b>threshold concentration</b> subpanel allows users to specify threshold concentrations below which there is no association between air pollution and adverse health effects."))),
          # tags$li(HTML("The <b>threshold concentration</b> subpanel allows users to specify threshold concentrations below which there is no association between air pollution and adverse health effects.")),
          tags$li(HTML(i18n$t("The <b>PM2.5 CRFs</b> subpanel provides concentration response functions (CRFs) for PM2.5 (with a 10 ug/m3 change) and ten associated health endpoints. Users can make changes and use their own parameters."))),
          tags$li(HTML(i18n$t("The <b>CRFs(O3, CO, SO2, NO2)</b> subpanel lists CRFs for O3, CO, SO2, NO2 and eight associated health endpoints. Users can make changes and use their own parameters."))),
         

So I want you to add i18n to places where translation would be needed.

Further, I want you to create this object for me:

    {
      "en": "Summer O3(ppb)",
      "fr": "FR-trans"
    },
    {
      "en": "NO2(ppb)",
      "fr": "FR-trans"
    },

However, give me the actual French translation. Do you understand? You can reply yes or no. There is no need to provide me with a sample.