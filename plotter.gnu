set term png
set terminal png enhanced size 1280, 1024


# set output "diff.png"
# set xlabel "Height"
# set ylabel "Difficulty target"
# set logscale y
# plot 'difficulties.txt' using 1:2 with linespoints


set style line 1 lc rgb "black"
set key box linestyle 1
set key width 1
set key height 1
set output "interval.png"
set xlabel "Height in epoch"
set ylabel "Time interval between blocks (s)"
set xrange [0:2016]
# set style line 10
f(x) = a*x + b
fit f(x) 'intervals.txt' u 0:1  via a, b
plot 'intervals.txt' using 0:1 title "Time interval between blocks" with points pointtype 3, f(x) lw 3 lc rgb "red" title "Approximation" with lines


