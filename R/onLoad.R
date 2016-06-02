.onLoad = function(libname, pkfname) {
    packageStartupMessage("***\n",
        "WARNING: there is some concern about the way logLik is calculated by R for a nls fit\n",
        "(see http://r.789695.n4.nabble.com/LogLik-of-nls-td4657207.html).\n",
        "Maybe it would be better to calculate it \"by hand\" from the nlsLM fit?\n",
        "***\n")
}
