set autoscale   # scale axes automatically
unset log       # remove any log-scaling
unset label     # remove any previous labels
set style line 99 lc rgb '#e5e5e5' lw 1
set grid mxtics xtics mytics ytics ls 99
set terminal svg
set output outputfile
set xlabel 'Clock error [s]'
set ylabel 'Probability [%]'
set key left top
set ytics 0,10,100
set mxtics 5
set mytics 5
#set xr [-0.0075:0.0125]
#set xr [-0.005:0.01]
set style line 1 lc rgb '#004586' lt 1 lw 2 pt -1
set style line 2 lc rgb '#ff420e' lt 1 lw 2 pt -1
set style line 3 lc rgb '#ccff00' lt 1 lw 2 pt -1
plot inputfile using 1:2 title 'empirical cdf' with linespoints ls 1, \
     inputfile using 1:3 title 'estimated cdf' with linespoints ls 2, \
     inputfile using 1:4 title 'difference' with linespoints ls 3, \
