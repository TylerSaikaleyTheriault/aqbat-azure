library(rsconnect)

# rsconnect::setAccountInfo(
#   name = Sys.getenv("RSCONNECT_ACCOUNT_NAME"),
#   token = Sys.getenv("RSCONNECT_ACCOUNT_TOKEN"),
#   secret = Sys.getenv("RSCONNECT_ACCOUNT_SECRET")
# )

account_name <- Sys.getenv("RSCONNECT_ACCOUNT_NAME")
print(account_name)

rsconnect::setAccountInfo(
  name='ajsim',
  token='9E8C5F8EB0A9E753F10EC821EDEA53A1',
  secret='1eI3kqks/sjkZma8e8k/MGob6CD+LA3e1rLR/dfL'
)

# Set the directory to the root directory of your repository
app_dir <- "./"

# Generate the 'dist' folder
deployApp(appDir = app_dir)

