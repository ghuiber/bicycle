# This is an attempt to kill the "no visible bindings"
# note I get when I use the ggplot unquoted aes() syntax.
# Solution suggested here: https://r-pkgs.org/package-within.html?#echo-a-working-package
# Discussion here: https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
utils::globalVariables(c('.',
                         'af_triangle',
                         'cs_triangle', 'dt_triangle',
                         'f_triangle',
                         'ht_ext_triangle',
                         'ht_triangle',
                         'r',
                         'rake_triangle',
                         'sa_ext_triangle',
                         'sa_triangle',
                         'ss_triangle',
                         'st_triangle',
                         'tt_triangle',
                         'x',
                         'y'))
