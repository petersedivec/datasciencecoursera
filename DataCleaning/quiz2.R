# Question #1
# Client ID
# 02ccb0eb13f29c794b08
# Client Secret
# e9ddae113d5eab1c45c138feb12585204ed31a4a

library(httr)

# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications;
#    Use any URL you would like for the homepage URL (http://github.com is fine)
#    and http://localhost:1410 as the callback url
#
#    Insert your client ID and secret below - if secret is omitted, it will
#    look it up in the GITHUB_CONSUMER_SECRET environmental variable.
myapp <- oauth_app("github", "02ccb0eb13f29c794b08", secret="e9ddae113d5eab1c45c138feb12585204ed31a4a")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
ans <- GET("https://api.github.com/repos/jtleek/datasharing")

# Question #2


# Question #4

con = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(con)
close(con)

# Question #5
x <- read.fwf(
    file="getdata-wksst8110.for",
    skip=4,
    widths=c(12, 7,4, 9,4, 9,4, 9,4))