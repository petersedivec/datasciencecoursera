percenDiff <- function(x) {
    if (x$fips == "24510") {
        x$change = (x$total - 3731.12000) / 3731.12000
    } else {
        x$change = (x$total - 146.82) / 146.82
    }
}