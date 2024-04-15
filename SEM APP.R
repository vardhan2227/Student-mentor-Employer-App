library(shiny)
library(leaflet)

# Define users (in a real-world application, this data would be stored securely)
users <- data.frame(
  username = c("user1", "user2", "user3"),
  password = c("password1", "password2", "password3"),
  stringsAsFactors = FALSE
)

# Define UI for login page with background image
login_ui <- fluidPage(
  tags$style(HTML('
    body {
      background-image: url("https://railsware.com/blog/wp-content/uploads/2019/07/Why-we-use-ReactJS-for-our-projects-Illustration.jpg");
      background-size: cover;
    }
  ')),
  titlePanel("Login"),
  HTML("<style>#login-error { color: red; }</style>"),
  div(
    style = "display: flex; align-items: center; justify-content: center; height: 100vh;",
    div(
      style = "width: 300px; background-color: rgba(255, 255, 255, 0.8); padding: 20px;",
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      actionButton("login", "Log In"),
      p(id = "login-error", "")
    )
  )
)

# Define UI for signup page with background image
signup_ui <- fluidPage(
  tags$style(HTML('
    body {
      background-image: url("https://railsware.com/blog/wp-content/uploads/2019/07/Why-we-use-ReactJS-for-our-projects-Illustration.jpg");
      background-size: cover;
    }
  ')),
  titlePanel("Sign Up"),
  div(
    style = "display: flex; align-items: center; justify-content: center; height: 100vh;",
    div(
      style = "width: 300px; background-color: rgba(255, 255, 255, 0.8); padding: 20px;",
      textInput("new_username", "New Username:"),
      passwordInput("new_password", "New Password:"),
      actionButton("signup", "Sign Up"),
      p(id = "signup-message", "")
    )
  )
)

# Define server logic for login page
login_server <- function(input, output, session) {
  observeEvent(input$login, {
    # Check if username and password match
    user <- users[users$username == input$username, ]
    if (nrow(user) == 0 || user$password != input$password) {
      updateTextInput(session, "username", value = "")
      updateTextInput(session, "password", value = "")
      updateParagraph(session, "login-error", "Invalid username or password.")
    } else {
      # If successful login, navigate to the About Us tab
      updateTabsetPanel(session, "auth_tabs", selected = "About Us")
      # For simplicity, let's just print a success message here
      print("Login successful!")
    }
  })
}

# Define server logic for signup page
signup_server <- function(input, output, session) {
  observeEvent(input$signup, {
    # Check if username already exists
    if (input$new_username %in% users$username) {
      updateTextInput(session, "new_username", value = "")
      updatePasswordInput(session, "new_password", value = "")
      updateParagraph(session, "signup-message", "Username already exists. Please choose another one.")
    } else {
      # If username is unique, add new user
      new_user <- data.frame(username = input$new_username, password = input$new_password, stringsAsFactors = FALSE)
      users <<- rbind(users, new_user)
      updateParagraph(session, "signup-message", "Sign up successful! You can now log in.")
      
      # Switch to the Login tab
      updateTabsetPanel(session, "auth_tabs", selected = "Login")
    }
  })
}

# Define UI for the application
ui <- navbarPage(
  "Authentication App",
  id = "auth_tabs",
  selected = "Login", # Set the initial tab as "Login"
  tabPanel("Login", login_ui),
  tabPanel("Sign Up", signup_ui),
  tabPanel("My Profile", 
           fluidPage(
             h1("My Profile", align = "center"),
             h3("Basic Resume Form", align = "center"),
             br(),
             fluidRow(
               column(6, textInput("name", "Name:")),
               column(6, textInput("email", "Email:"))
             ),
             fluidRow(
               column(6, textInput("phone", "Phone Number:")),
               column(6, textInput("education", "Education:"))
             ),
             fluidRow(
               column(6, textInput("experience", "Experience:")),
               column(6, textInput("skills", "Skills:"))
             ),
             fluidRow(
               column(12, actionButton("submit_resume", "Submit"))
             )
           )
  ),
  tabPanel("My Connections", 
           fluidPage(
             h1("My Connections", align = "center"),
             h3("Friend Connections", align = "center"),
             br(),
             fluidRow(
               column(4, verbatimTextOutput("friend1")),
               column(4, verbatimTextOutput("friend2")),
               column(4, verbatimTextOutput("friend3"))
             ),
             h3("Mentor Connections", align = "center"),
             br(),
             fluidRow(
               column(4, verbatimTextOutput("mentor1")),
               column(4, verbatimTextOutput("mentor2")),
               column(4, verbatimTextOutput("mentor3"))
             ),
             h3("Employer Connections", align = "center"),
             br(),
             fluidRow(
               column(4, verbatimTextOutput("employer1")),
               column(4, verbatimTextOutput("employer2")),
               column(4, verbatimTextOutput("employer3"))
             ),
             br(),
             fluidRow(
               column(12, plotOutput("user_role_plot"))
             )
           )
  ),
  tabPanel("My Messages", 
           fluidPage(
             h1("My Messages", align = "center"),
             h3("Messages from Employers", align = "center"),
             br(),
             fluidRow(
               column(12, verbatimTextOutput("employer_msg1")),
               column(12, verbatimTextOutput("employer_msg2")),
               column(12, verbatimTextOutput("employer_msg3"))
             ),
             h3("Messages from Mentors", align = "center"),
             br(),
             fluidRow(
               column(12, verbatimTextOutput("mentor_msg1")),
               column(12, verbatimTextOutput("mentor_msg2")),
               column(12, verbatimTextOutput("mentor_msg3"))
             )
           )
  ),
  tabPanel("Search Page", 
           fluidPage(
             h1("Search Page", align = "center"),
             br(),
             fluidRow(
               column(12, textInput("search_query", "Search for companies or profiles:")),
               column(12, uiOutput("search_results"))
             )
           )
  ),
  tabPanel("Job Board", 
           fluidPage(
             h1("Job Board", align = "center"),
             h3("Apply for a Job", align = "center"),
             br(),
             fluidRow(
               column(6, textInput("job_title", "Job Title:")),
               column(6, textInput("company", "Company:"))
             ),
             fluidRow(
               column(6, fileInput("resume", "Upload Resume/CV:")),
               column(6, textInput("name", "Your Name:"))
             ),
             fluidRow(
               column(6, textInput("email", "Your Email:")),
               column(6, textInput("skills", "Skills:"))
             ),
             fluidRow(
               column(6, textInput("experience", "Experience:")),
               column(6, textInput("cover_letter", "Cover Letter:"))
             ),
             fluidRow(
               column(12, actionButton("apply_job", "Apply"))
             )
           )
  ),
  tabPanel("About Us", 
           fluidPage(
             h1("About Us", align = "center"),
             br(),
             fluidRow(
               column(6,
                      tags$div(
                        HTML("
                              <h3>Disclaimer</h3>
                              <p>The Business Platform (&quot;the Platform&quot;) is provided for informational and networking purposes only. By accessing and using the Platform, you agree to comply with and be bound by the following disclaimer:</p>
                              <p><strong>Accuracy of Information:</strong> While we strive to ensure the accuracy and reliability of the information provided on the Platform, we make no representations or warranties of any kind, express or implied, about the completeness, accuracy, reliability, suitability, or availability of the information, products, services, or related graphics contained on the Platform for any purpose. Any reliance you place on such information is strictly at your own risk.</p>
                              <p><strong>User Responsibility:</strong> Users of the Platform are solely responsible for verifying the accuracy, completeness, and suitability of any information provided on the Platform. Users should exercise due diligence and seek professional advice where necessary before making any decisions based on the information provided.</p>
                              <p><strong>Networking and Communication:</strong> The Platform serves as a common networking and communication platform for students, mentors, and employers. While we endeavor to facilitate meaningful connections and interactions, we do not guarantee the availability, competence, or reliability of any user or entity participating on the Platform. Users are encouraged to conduct their own assessments and exercises caution when engaging with others on the Platform.</p>
                              <p><strong>No Endorsement:</strong> The inclusion of any user profiles, job listings, mentorship opportunities, or other content on the Platform does not imply endorsement or recommendation by us. We do not endorse, sponsor, or approve any user-generated content and cannot be held responsible for the views or actions of individual users.</p>
                              <p><strong>Limitation of Liability:</strong> In no event will we be liable for any loss or damage, including without limitation, indirect or consequential loss or damage, or any loss or damage whatsoever arising from loss of data or profits arising out of, or in connection with, the use of the Platform.</p>
                              <p><strong>Modification of Terms:</strong> We reserve the right to modify, update, or amend this disclaimer at any time without prior notice. It is the responsibility of users to review this disclaimer periodically for any changes.</p>
                              <p>By accessing and using the Platform, you acknowledge that you have read, understood, and agree to be bound by this disclaimer.</p>
                              ")
                      )),
               column(6, plotOutput("user_role_plot")),
               column(6, leafletOutput("user_location_map"))
             )
           )
  ),
  tabPanel("Mentor Support",
           fluidPage(
             h1("Mentor Support", align = "center"),
             h3("Basic Resume Form", align = "center"),
             br(),
             fluidRow(
               column(6, textInput("mentor_name", "Mentor's Name:")),
               column(6, textInput("mentor_email", "Mentor's Email:"))
             ),
             fluidRow(
               column(12, actionButton("submit_mentor_request", "Submit Mentor Request"))
             )
           )
  )
)

# Custom CSS to align "Mentor Support" tab to bottom left
css <- "
.ui-navbar li:last-child {
  position: fixed;
  bottom: 0;
  left: 0;
}
"

# Define server logic for the application
server <- function(input, output, session) {
  # Generate dummy connections data
  output$friend1 <- renderText({
    "Friend 1: John Doe"
  })
  output$friend2 <- renderText({
    "Friend 2: Jane Smith"
  })
  output$friend3 <- renderText({
    "Friend 3: Alex Johnson"
  })
  output$mentor1 <- renderText({
    "Mentor 1: Sarah Lee"
  })
  output$mentor2 <- renderText({
    "Mentor 2: Michael Brown"
  })
  output$mentor3 <- renderText({
    "Mentor 3: Emily Davis"
  })
  output$employer1 <- renderText({
    "Employer 1: Company X"
  })
  output$employer2 <- renderText({
    "Employer 2: Company Y"
  })
  output$employer3 <- renderText({
    "Employer 3: Company Z"
  })
  
  # Generate dummy messages from employers
  output$employer_msg1 <- renderText({
    "Message 1 from Employer: We are pleased to inform you that your application has been shortlisted."
  })
  output$employer_msg2 <- renderText({
    "Message 2 from Employer: Could you please provide additional details about your work experience?"
  })
  output$employer_msg3 <- renderText({
    "Message 3 from Employer: Unfortunately, your application was not successful at this time."
  })
  
  # Generate dummy messages from mentors
  output$mentor_msg1 <- renderText({
    "Message 1 from Mentor: Congratulations on your recent achievements!"
  })
  output$mentor_msg2 <- renderText({
    "Message 2 from Mentor: I have reviewed your resume and have some suggestions for improvement."
  })
  output$mentor_msg3 <- renderText({
    "Message 3 from Mentor: Let's schedule a meeting to discuss your career goals."
  })
  
  # Dummy search results
  output$search_results <- renderUI({
    search_results <- c("Company A", "Company B", "Company C", "Profile 1", "Profile 2", "Profile 3")
    if (!is.null(input$search_query) && input$search_query != "") {
      search_results <- grep(input$search_query, search_results, value = TRUE, ignore.case = TRUE)
    }
    if (length(search_results) > 0) {
      tags$ul(
        lapply(search_results, function(result) {
          tags$li(result)
        })
      )
    } else {
      "No results found."
    }
  })
  
  # Generate the user role distribution plot
  output$user_role_plot <- renderPlot({
    # Generate some dummy data for the plot
    years <- seq(2020, 2025)
    users_count <- c(100, 150, 200, 250, 300, 350)  # Just dummy numbers for illustration
    
    # Plotting
    plot(years, users_count, type = "o", xlab = "Year", ylab = "Number of Users",
         main = "User Growth Over Time", col = "blue", lwd = 2)
  })
  
  # Generate the user location map
  output$user_location_map <- renderLeaflet({
    # Dummy data for user locations
    # Define user locations including 10 additional countries, including India
    locations <- data.frame(
      lat = c(40.7128, 34.0522, 37.7749,  # USA: New York, Los Angeles, San Francisco
              41.8781, 29.7604, 39.9526,  # USA: Chicago, Houston, Philadelphia
              33.4484, 35.6895, 32.7157,  # USA: Phoenix, Tokyo, San Diego
              42.3601, 38.9072, 51.5074,  # USA: Boston, Washington D.C., London
              52.5200, 48.8566, 53.3498,  # Germany: Berlin, France: Paris, Ireland: Dublin
              55.7558, 59.3293, 60.1695,  # Russia: Moscow, Sweden: Stockholm, Finland: Helsinki
              52.3676, 48.8566, 45.4215,  # Netherlands: Amsterdam, Poland: Warsaw, Italy: Milan
              37.7749, 34.0522, 34.0522,  # USA: San Jose, Santa Monica, Hollywood
              34.0522, 34.0522, 34.0522,  # USA: Beverly Hills, West Hollywood, Long Beach
              34.0522, 34.0522, 34.0522,  # USA: Anaheim, Santa Ana, Irvine
              34.0522, 34.0522, 34.0522,  # USA: Glendale, Pasadena, Burbank
              34.0522, 34.0522, 34.0522,  # USA: Inglewood, Torrance, Culver City
              34.0522, 34.0522, 34.0522,  # USA: Manhattan Beach, Redondo Beach, Marina del Rey
              34.0522, 34.0522, 34.0522,  # USA: Hermosa Beach, Malibu, Venice
              34.0522, 34.0522, 34.0522,  # USA: El Segundo, Calabasas, Westlake Village
              34.0522, 34.0522, 34.0522,  # USA: Agoura Hills, Santa Clarita, Thousand Oaks
              34.0522, 34.0522, 34.0522,  # USA: Simi Valley, Moorpark, Camarillo
              19.0760,                       # India: Mumbai
              51.1657, 35.6895, 46.2276,   # New Zealand: Wellington, Japan: Tokyo, Canada: Toronto
              45.4215, 35.6895, 20.5937    # USA: Seattle, San Diego, India: New Delhi
      ),
      lon = c(-74.0060, -118.2437, -122.4194,  # USA: New York, Los Angeles, San Francisco
              -87.6298, -95.3698, -75.1652,   # USA: Chicago, Houston, Philadelphia
              -112.0740, 139.6917, -117.1611, # USA: Phoenix, Tokyo, San Diego
              -71.0589, -77.0369, -0.1278,    # USA: Boston, Washington D.C., London
              13.4050, 2.3522, -6.2603,       # Germany: Berlin, France: Paris, Ireland: Dublin
              37.6173, 18.0686, 24.9384,      # Russia: Moscow, Sweden: Stockholm, Finland: Helsinki
              4.9041, 2.3522, 9.1859,         # Netherlands: Amsterdam, Poland: Warsaw, Italy: Milan
              -122.4194, -118.2437, -118.2437,# USA: San Jose, Santa Monica, Hollywood
              -118.2437, -118.2437, -118.2437,# USA: Beverly Hills, West Hollywood, Long Beach
              -118.2437, -118.2437, -118.2437,# USA: Anaheim, Santa Ana, Irvine
              -118.2437, -118.2437, -118.2437,# USA: Glendale, Pasadena, Burbank
              -118.2437, -118.2437, -118.2437,# USA: Inglewood, Torrance, Culver City
              -118.2437, -118.2437, -118.2437,# USA: Manhattan Beach, Redondo Beach, Marina del Rey
              -118.2437, -118.2437, -118.2437,# USA: Hermosa Beach, Malibu, Venice
              -118.2437, -118.2437, -118.2437,# USA: El Segundo, Calabasas, Westlake Village
              -118.2437, -118.2437, -118.2437,# USA: Agoura Hills, Santa Clarita, Thousand Oaks
              -118.2437, -118.2437, -118.2437,# USA: Simi Valley, Moorpark, Camarillo
              72.8777,                        # India: Mumbai
              -0.1257, 139.6917, -79.3832,    # New Zealand: Wellington, Japan: Tokyo, Canada: Toronto
              -75.6903, -78.9190, 78.9629     # USA: Seattle, San Diego, India: New Delhi
      ),
      name = c("New York", "Los Angeles", "San Francisco",  # USA
               "Chicago", "Houston", "Philadelphia",        # USA
               "Phoenix", "Tokyo", "San Diego",             # USA, Japan
               "Boston", "Washington D.C.", "London",       # USA, UK
               "Berlin", "Paris", "Dublin",                 # Germany, France, Ireland
               "Moscow", "Stockholm", "Helsinki",           # Russia, Sweden, Finland
               "Amsterdam", "Warsaw", "Milan",              # Netherlands, Poland, Italy
               "San Jose", "Santa Monica", "Hollywood",     # USA
               "Beverly Hills", "West Hollywood", "Long Beach",  # USA
               "Anaheim", "Santa Ana", "Irvine",            # USA
               "Glendale", "Pasadena", "Burbank",          # USA
               "Inglewood", "Torrance", "Culver City",     # USA
               "Manhattan Beach", "Redondo Beach", "Marina del Rey",  # USA
               "Hermosa Beach", "Malibu", "Venice",        # USA
               "El Segundo", "Calabasas", "Westlake Village",  # USA
               "Agoura Hills", "Santa Clarita", "Thousand Oaks",  # USA
               "Simi Valley", "Moorpark", "Camarillo",     # USA
               "Mumbai",                                  # India
               "Wellington", "Tokyo", "Toronto",          # New Zealand, Japan, Canada
               "Seattle", "San Diego", "New Delhi"        # USA, India
      )
    )
    
    
    
    # Create leaflet map
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = locations, ~lon, ~lat, color = "red", radius = 5) %>%
      addControl(
        html = "<h3 style='text-align:center;'>User Locations Across Different Countries</h3>",
        position = "topright"
      )
  })
} 

# Run the application
shinyApp(ui = tagList(ui, tags$style(css)), server = server)