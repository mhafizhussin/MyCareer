panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content)
    )
}

calculateScore <- function(s1, s2, s3, s4, a, b) {
    x=data.frame("value" = c(s1,s2,s3,s4))
    x$assign <- ifelse(x$value<4,a,b)
    print(x)
    y <- as.data.frame(table(x$assign))
    
    if(nrow(y) == 2)
        if(y[1,2]>y[2,2]) return(a) else return(b)
    else
        if(y[1,1] == a) return(a) else return(b)
}

# vacancy <- read.csv('data/vacancy.csv')

jobListing <- function(){
   
    titlePanel("Personality Kerjaya Anda")
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(12,
                    div(class="panel panel-default", 
                        div(class="panel-body",  
                                tags$div( align = "center",
                                      div( align = "center", 
                                           h4("Bahagian 1 - Bagaimana anda mendapatkan tenaga anda?"),
                                           style = "font-weight:bold"
                                      )),
                                dropdownButton(
                                    inputId = "mydropdown",
                                    label = "Soalan 1",
                                    icon = icon("sliders"),
                                    status = "primary",
                                    circle = FALSE,
                                    knobInput(
                                        inputId = "s11",
                                        label = "seorang yang selalu bersosial",
                                        value = 3,
                                        min = 0,
                                        max = 6,
                                        displayPrevious = TRUE,
                                        lineCap = "round",
                                        fgColor = "#428BCA",
                                        inputColor = "#428BCA"
                                    ),
                                    div(align = "center")
                                ),
                                br(),
                                dropdownButton(
                                    inputId = "mydropdown",
                                    label = "Soalan 2",
                                    icon = icon("sliders"),
                                    status = "primary",
                                    circle = FALSE,
                                    knobInput(
                                        inputId = "s12",
                                        label = "fokus pada dunia luar",
                                        value = 3,
                                        min = 0,
                                        max = 6,
                                        displayPrevious = TRUE,
                                        lineCap = "round",
                                        fgColor = "#428BCA",
                                        inputColor = "#428BCA"
                                    ),
                                    div(align = "center")
                                ),
                                br(),
                                dropdownButton(
                                    inputId = "mydropdown",
                                    label = "Soalan 3",
                                    icon = icon("sliders"),
                                    status = "primary",
                                    circle = FALSE,
                                    knobInput(
                                        inputId = "s13",
                                        label = "meluangkan masa dengan orang lain",
                                        value = 3,
                                        min = 0,
                                        max = 6,
                                        displayPrevious = TRUE,
                                        lineCap = "round",
                                        fgColor = "#428BCA",
                                        inputColor = "#428BCA"
                                    ),
                                    div(align = "center")
                                ),
                                br(),
                                dropdownButton(
                                    inputId = "mydropdown",
                                    label = "Soalan 4",
                                    icon = icon("sliders"),
                                    status = "primary",
                                    circle = FALSE,
                                    knobInput(
                                        inputId = "s14",
                                        label = "suka bercakap dan memulakan perbualan",
                                        value = 3,
                                        min = 0,
                                        max = 6,
                                        displayPrevious = TRUE,
                                        lineCap = "round",
                                        fgColor = "#428BCA",
                                        inputColor = "#428BCA"
                                    ),
                                    div(align = "center")
                                )
                            ),
                        )
                    
                    
                    
                ),
                br(),
                column(12,
                       div(class="panel panel-default", 
                           div(class="panel-body",  
                               tags$div( align = "center",
                                         div( align = "center", 
                                              h4("Bahagian 2 - Bagaimana anda melihat dunia?"),
                                              style = "font-weight:bold"
                                         )),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 1",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s21",
                                       label = "memberi perhatian kepada perincian",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               ),
                               br(),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 2",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s22",
                                       label = "fokus kepada realiti",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               ),
                               br(),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 3",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s23",
                                       label = "berfikir secara konkrit",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               ),
                               br(),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 4",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s24",
                                       label = "suka perkara praktikal",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               )
                           ),
                       )
                       
                ),
                br(),
                column(12,
                       div(class="panel panel-default", 
                           div(class="panel-body",  
                               tags$div( align = "center",
                                         div( align = "center", 
                                              h4("Bahagian 3 - Bagaimana anda membuat keputusan?"),
                                              style = "font-weight:bold"
                                         )),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 1",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s31",
                                       label = "berdasarkan logik",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               ),
                               br(),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 2",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s32",
                                       label = "berminat dengan idea-idea",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               ),
                               br(),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 3",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s33",
                                       label = "melayan semua orang dengan cara yang sama",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               ),
                               br(),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 4",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s34",
                                       label = "lebih saintifik dalam menggambarkan dunia",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               ),
                           ),
                       )
                       
                ),
                br(),
                column(12,
                       div(class="panel panel-default", 
                           div(class="panel-body",  
                               tags$div( align = "center",
                                         div( align = "center", 
                                              h4("Bahagian 4 - Bagaimana anda merancang?"),
                                              style = "font-weight:bold"
                                         )),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 1",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s41",
                                       label = "tersusun dan berstruktur",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               ),
                               br(),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 2",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s42",
                                       label = "membuat perancangan terlebih dahulu",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               ),
                               br(),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 3",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s43",
                                       label = "berpegang pada rancangan",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               ),
                               br(),
                               dropdownButton(
                                   inputId = "mydropdown",
                                   label = "Soalan 4",
                                   icon = icon("sliders"),
                                   status = "primary",
                                   circle = FALSE,
                                   knobInput(
                                       inputId = "s44",
                                       label = "mahu membuat keputusan sendiri",
                                       value = 3,
                                       min = 0,
                                       max = 6,
                                       displayPrevious = TRUE,
                                       lineCap = "round",
                                       fgColor = "#428BCA",
                                       inputColor = "#428BCA"
                                   ),
                                   div(align = "center")
                               )
                           ),
                       )
                       
                ),
                div(align = "center",
                    br(),
                    actionButton("submit_2", "Submit"),
                    br()
                )
                
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Keputusan",
                         fluidRow(
                             div(align = "center",
                                 br(),
                                 h2("Personaliti Anda!"),
                                 br(), br(),
                                 # h3("Anda ialah:"),
                                 h4(tableOutput("values_2")),
                                 br(), br(),
                                 h4(textOutput('first')),
                                 br(), br(),
                                 h2("Kerjaya terbaik Untuk Personaliti Anda"),
                                 br(), br(),
                                 h4(tableOutput('second')),
                                 br(),
                             )
                         )
                ),
                tabPanel("Carian Kerja",
                         # Create a new Row in the UI for selectInputs
                         fluidRow(
                             column(4,
                                    selectInput("pos",
                                                "Position:",
                                                c("All",
                                                  unique(as.character(vacancy$position))))
                             ),
                             column(4,
                                    selectInput("edu",
                                                "Educations:",
                                                c("All",
                                                  unique(as.character(vacancy$education))))
                             ),
                             column(4,
                                    selectInput("state",
                                                "Location:",
                                                c("All",
                                                  unique(as.character(vacancy$state))))
                             )
                         ),
                         # Create a new row for the table.
                         DT::dataTableOutput("table")
                         )
            )
        )
    )
}

personalityTest <- function(){
    
    mainPanel( width = 12,
               
               fluidRow(),
               
               fluidRow(),
               
               fluidRow(
                   div(align = "center",
                       br(),
                       br(),
                       actionButton("submit", "Submit"),
                       br(),
                       br()
                   )
               ),
               
               fluidRow(
                   div(align = "center",
                       br(),
                       br(),
                       tableOutput("values"),
                       br(),
                       br()
                    )
               ),
               fluidRow(
                   column(2),
                   column(8,
                          div(align = "center",
                              DT::DTOutput('careertbl')
                              )
                          ),
                   column(2)
               )
    )
    
}

careerPath <- function() {
    sidebarLayout( 
        
        sidebarPanel( 
            width = 3,
            
            tags$div(
                style = "height:50px;",
                selectizeInput("changeAvatar", "Change Icon:",
                               choices = c(
                                   "Map Marker" = "map-marker", 
                                   "Rocket" = "rocket", 
                                   "Leaf" = "leaf"),
                               selected = "rocket"
                ),
                
                tags$div(
                    style = "height:50px;",
                    
                    uiOutput("printInput1"),
                    uiOutput("printInput2"),
                    uiOutput("printInput3"),
                    uiOutput("printInput4"),
                    uiOutput("printInput5")
                )
            )
        ),  # Closes sidebarPanel
        mainPanel( width = 8,
                   fluidRow(
                       column(2.5,
                              div(class="panel panel-default", 
                                  div(class="panel-body",  width = "600px",
                                      align = "center",
                                      div(
                                          tags$img(src = "one.svg", 
                                                   width = "50px", height = "50px")
                                      ),
                                      div(
                                          h5(
                                              "Pilih satu kerjaya untuk permulaan. Boleh memilih pekerjaan berdasarkan personaliti atau terokai kerjaya baru."
                                          )
                                      )
                                  )
                              )
                       ),
                       column(2.5,
                              div(class="panel panel-default",
                                  div(class="panel-body",  width = "600px", 
                                      align = "center",
                                      div(
                                          tags$img(src = "two.svg", 
                                                   width = "50px", height = "50px")
                                      ),
                                      div(
                                          h5(
                                              "Kemudian dari kerjaya itu, anda boleh meneroka set kerjaya orang yang berikutnya."
                                          )
                                      )
                                  )
                              )
                       ),
                       column(2.5,
                              div(class="panel panel-default",
                                  div(class="panel-body",  width = "600px", 
                                      align = "center",
                                      div(
                                          tags$img(src = "three.svg", 
                                                   width = "50px", height = "50px")),
                                      div(
                                          h5(
                                              "Rancang sehingga lima langkah di dalam kerjaya anda. Apabila anda sudah bersedia, anda boleh menyimpan atau mencetak laporan peribadi anda."
                                          )
                                      )
                                  )
                              )
                       ),
                   ),
                   fluidRow(
                       
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"
                       ),
                       introBox(
                           panel_div(class_type = "default",
                                     content = tags$div(
                                         uiOutput("displayName"),
                                         visNetwork::visNetworkOutput("visTest", height = "350px")
                                     )
                           ),
                           data.step = 4,
                           data.intro = "Your selections will be displayed here in a graph."
                       )
                   ),
                   fluidRow(
                       div(class="panel panel-default",
                           div(class="panel-body",  width = "600px",
                               tags$div(class = "wrap",
                                        div(class = "left", 
                                            style="display: inline-block;vertical-align:top; width: 150px;",
                                            uiOutput("stepNo")
                                        ),
                                        div(class = "right",
                                            style="display: inline-block;vertical-align:top; width: 150px;",
                                            
                                            introBox(
                                                
                                                checkboxInput('returnpdf', 'Save as PDF?', FALSE),
                                                data.step = 5, data.intro = "Stay on track with your plans by downloading your path."
                                                
                                            ),
                                            uiOutput("download")
                                        ),
                                        div(class = "center",
                                            style="display: inline-block;vertical-align:top; width: 150px;",
                                            introBox(
                                                actionButton("goBack", 
                                                             label = "Back", 
                                                             icon = icon("arrow-circle-left", class = "fa-2x"),
                                                             width= "100px", height= "40px"),
                                                data.step = 3,
                                                data.intro = "Go back a step to edit your selection anytime."
                                            )
                                        ),
                                        div(class = "center",
                                            style="display: inline-block;vertical-align:top; width: 150px;",
                                            introBox(
                                                actionButton("btn1", 
                                                             label = "Add", 
                                                             icon = icon("arrow-circle-right", class = "fa-2x"),
                                                             width= "100px", height= "40px"),
                                                data.step = 2,
                                                data.intro = "Confirm your selection by clicking here."
                                            )
                                        )
                               ),
                               # Insert Table Output
                               introBox(
                                   uiOutput("btns"),
                                   data.step = 1, 
                                   data.intro = "Mulakan dengan memilih salah satu kerjaya yang ada didalam database kami."
                               )
                           )
                       ),
                       plotOutput("myplot")
                   )
        )  # Closes the mainPanel
    )
}

about <- function(){
}