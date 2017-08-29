#!/usr/bin/bash

dir=$PWD/000_profile_`date +%Y-%m-%d_%H%M%S`
mkdir $dir
mkdir $dir/prof

profit () {
    echo "Profiling by $1 ($2)"
    base=$1
    opt="$2"
    ./fagin +RTS -p $opt -RTS > /dev/null
    hp2ps -c fagin.hp
    ps2pdf fagin.ps
    mv fagin.pdf $dir/$base.pdf
    rm fagin.ps
    mv fagin.hp $dir/prof/$base.hp
    mv fagin.prof $dir/prof/$base.prof
}

git log | awk 'BEGIN{RS="commit "; FS="\n"} NR == 2 {print "Commit: " $0}' > $dir/git-info
git status >> $dir/git-info

profit "hp_cost-center"  "-hc"
profit "hm_module"       "-hm"
profit "hd_closure_desc" "-hd"
profit "hy_type"         "-hy"
profit "hr_retainer"     "-hr"
profit "hb_biography"    "-hb"

pdfunite $dir/*pdf $dir/all-memory-plots.pdf

cabal bench --benchmark-options="-o $dir/benchmarks.html"
