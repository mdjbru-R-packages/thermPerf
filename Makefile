all:
	R -e "library(devtools); document(\".\"); install(\".\")"
