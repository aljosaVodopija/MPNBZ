bootstrapPage(
  tags$style(
    type = "text/css",
    "div.outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0; }"
  ),
  tabsetPanel(
    id = "tabi",
    tabPanel(
      "Izračun modela",
      value = "izracun",
      div(
        style = "padding: 20px",
        fixedRow(
          column(
            4,
            sliderInput(
              inputId = "opazovalniCasOkuzbe",
              label = "Opazovalni čas okužbe",
              min = 1,
              max = 31,
              value = 10
            ),
            textInput(
              inputId = "imeIzhodneDatoteke",
              label = "Ime izhodne datoteke",
              value = format(Sys.time(), "Simulacija (%Y-%m-%d %H:%M).RData")
            ),
            hr(),
            h4("Začetna okužba"),
            textInput(
              inputId = "krajiOkuzbe",
              label = "Kraj okužbe",
              value = "Grosuplje, Ptuj"
            ),
            sliderInput(
              inputId = "zacetnoSteviloOkuzenih",
              label = "Začetno število okuženih živali",
              min = 0,
              max = 1000,
              value = 200
            )
          ),
          column(
            4,
            h4("Verjetnosti prenosa okužbe"),
            sliderInput(
              inputId = "stopnjaUgrizov",
              label = "Stopnja ugrizov",
              min = 0,
              max = 100,
              value = 17,
              step = 1,
              post = "%"
            ),
            sliderInput(
              inputId = "prenosGostiteljNaVektor",
              label = "Verjetnost prenosa z gostitelja na vektor",
              min = 0,
              max = 15,
              value = 1,
              step = 0.1,
              post = "%"
            ),
            sliderInput(
              inputId = "prenosVektorNaGostitelj",
              label = "Verjetnost prenosa z vektorja na gostitelj",
              min = 0,
              max = 100,
              value = 90,
              step = 1,
              post = "%"
            ),
            sliderInput(
              inputId = "trenje",
              label = "Trenje vetra",
              min = 0,
              max = 1,
              value = 0.1,
              step = 0.01
            )
          ),
          column(
            4,
            h4("Populacija muh"),
            sliderInput(
              inputId = "steviloMuhNaGovedo",
              label = "Število muh na glavo goveda",
              min = 0,
              max = 5000,
              value = 900
            ),
            sliderInput(
              inputId = "steviloMuhNaDrobnico",
              label = "Število muh na glavo drobnice",
              min = 0,
              max = 500,
              value = 100
            ),
            sliderInput(
              inputId = "natalitetaMuh",
              label = "Največja dnevna nataliteta muh",
              min = 0,
              max = 1,
              value = 0.3,
              step = 0.01,
              post = "%"
            )
          )
        ),
        hr(),
        fixedRow(column(
          12,
          actionButton(inputId = "pozeniSimulacijo",
                       label = "Poženi simulacijo")
        ))
      )
    ),
    tabPanel(
      "Prikaz modela",
      value = "prikaz",
      div(
        class = "outer",
        leafletOutput(
          outputId = "map",
          width = "100%",
          height = "100%"
        ),
        absolutePanel(
          selectInput(
            inputId = "imeVhodneDatoteke",
            label = "Datoteka s simulacijo",
            choices = c("Izberite model..." = "", dir(path = mapaIzhodnihPodatkov))
          ),
          conditionalPanel(
            "input['imeVhodneDatoteke'] != ''",
            sliderInput(
              inputId = "dan",
              label = "Dan",
              min = 0,
              max = 0,
              value = 0,
              step = 1
            )
          ),
          top = 10,
          right = 10
        )
      )
    )
  )
)
