pisum = function() {
    t = 0.0
    for (j in 1:500) {
        t = 0.0
        for (k in 1:1000) {
            t = t + 1.0/(k*k)
        }
    }
    return(t)
}
