shinyUI(pageWithSidebar(
  headerPanel("Baseball Autograph Auction Sales Predictor"),
  sidebarPanel(
    selectInput("player", "Choose a player:", 
                choices = c("Hank Aaron", "Sparky Anderson", "Luis Aparicio", "Luke Appling", "Richie Ashburn", "Ernie Banks", "Johnny Bench", "Yogi Berra", "Wade Boggs", "Lou Boudreau", "George Brett", "Lou Brock", "Jim Bunning", "Rod Carew", "Steve Carlton", "Gary Carter", "Orlando Cepeda", "Joe DiMaggio", "Bobby Doerr", "Don Drysdale", "Dennis Eckersley", "Bob Feller", "Rollie Fingers", "Carlton Fisk", "Whitey Ford", "Bob Gibson", "Tony Gwynn", "Rickey Henderson", "Billy Herman", "Jim Hunter", "Monte Irvin", "Reggie Jackson", "Ferguson Jenkins", "Al Kaline", "George Kell", "Harmon Killebrew", "Ralph Kiner", "Sandy Koufax", "Bob Lemon", "Mickey Mantle", "Juan Marichal", "Eddie Mathews", "Willie Mays", "Bill Mazeroski", "Willie McCovey", "Johnny Mize", "Paul Molitor", "Joe Morgan", "Eddie Murray", "Stan Musial", "Hal Newhouser", "Phil Niekro", "Jim Palmer", "Tony Perez", "Gaylord Perry", "Kirby Puckett", "Pee Wee Reese", "Jim Rice", "Cal Ripken", "Phil Rizzuto", "Robin Roberts", "Brooks Robinson", "Frank Robinson", "Nolan Ryan", "Ryne Sandberg", "Mike Schmidt", "Red Schoendienst", "Tom Seaver", "Enos Slaughter", "Ozzie Smith", "Duke Snider", "Warren Spahn", "Willie Stargell", "Bruce Sutter", "Don Sutton", "Earl Weaver", "Hoyt Wilhelm", "Billy Williams", "Dick Williams", "Ted Williams", "Dave Winfield", "Early Wynn", "Carl Yastrzemski", "Robin Yount")),
    
    selectInput("category", "Choose a category:", 
                choices = c("Photos","Trading Cards")),
    textInput("seller", "Enter Seller:", "probstein123"),
    checkboxInput('auth', 'Autheniticated', FALSE),
    numericInput("minbid", "Enter the minimum bid:", 10),
    
    submitButton("Calculate")
  ),
  # mainPanel(
   # tableOutput('contents')
  #)
  
  mainPanel(
    tableOutput("playerinfo"),
    verbatimTextOutput("sellprob"),
    verbatimTextOutput("price")
  )
))