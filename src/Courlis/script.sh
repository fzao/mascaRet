find . -type f -name "*.f90" -print -exec iconv -f ISO8859-1 -t UTF-8 {} -o res/{} \;
