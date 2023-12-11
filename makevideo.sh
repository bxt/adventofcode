# Turns all visualisations into one big video.
#
# To find the possible days to generate gifs for run:
# > find . -type d -name target -prune -o -type f -iname 'vis*' -print | sort
#
# To find the days the gifs were already rendered for run:
# > find . -type d -name target -prune -o -iname '*.gif' -print | sort

# First we generate some title screens:

cd 2022
  cargo run --package adventofcode2022 --bin adventofcode2022drawtitle -r
  ./target/release/adventofcode2022drawtitle 2021 04
  ./target/release/adventofcode2022drawtitle 2021 05
  ./target/release/adventofcode2022drawtitle 2021 06
  ./target/release/adventofcode2022drawtitle 2021 07
  ./target/release/adventofcode2022drawtitle 2021 09
  ./target/release/adventofcode2022drawtitle 2021 11
  ./target/release/adventofcode2022drawtitle 2021 15
  ./target/release/adventofcode2022drawtitle 2021 19
  ./target/release/adventofcode2022drawtitle 2022 08
  ./target/release/adventofcode2022drawtitle 2022 09
  ./target/release/adventofcode2022drawtitle 2022 14
  ./target/release/adventofcode2022drawtitle 2022 22
  ./target/release/adventofcode2022drawtitle 2022 24
  ./target/release/adventofcode2022drawtitle 2023 05
  ./target/release/adventofcode2022drawtitle 2023 10
cd ..

# You can use this for debugging filter pipelines:

# echo 'nullsrc,scale=iw*3:-1[v0],nullsrc,scale=iw*3:-1[v1],[v0][v1] hstack,nullsink' | \
# graph2dot -o graph.tmp && \
# dot -Tpng graph.tmp -o graph.png && \
# open graph.png

# Humungous command to concat title screens and gifs:

ffmpeg \
  -i 2022/target/output-2021-04.gif \
  -i 2022/target/output-2021-05.gif \
  -i 2022/target/output-2021-06.gif \
  -i 2022/target/output-2021-07.gif \
  -i 2022/target/output-2021-09.gif \
  -i 2022/target/output-2021-11.gif \
  -i 2022/target/output-2021-15.gif \
  -i 2022/target/output-2021-19.gif \
  -i 2022/target/output-2022-08.gif \
  -i 2022/target/output-2022-09.gif \
  -i 2022/target/output-2022-14.gif \
  -i 2022/target/output-2022-22.gif \
  -i 2022/target/output-2022-24.gif \
  -i 2022/target/output-2023-05.gif \
  -i 2022/target/output-2023-10.gif \
  \
  -i 2021/day04/output.gif \
  -i 2021/day05/output.gif \
  -i 2021/day06/output.gif \
  -i 2021/day07/output.gif \
  -i 2021/day09/output.gif \
  -i 2021/day11/output.gif \
  -i 2021/day15/output.gif \
  -i 2021/day19/output.gif \
  -i 2022/day08/output.gif \
  -i 2022/day09/output.gif \
  -i 2022/day14/output.gif \
  -i 2022/day22/output.gif \
  -i 2022/day24/output.gif \
  -i 2023/day05/output2.gif \
  -i 2023/day10/output.gif \
  -movflags faststart -pix_fmt yuv420p \
  -filter_complex '
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202104],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202105],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202106],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202107],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202109],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202111],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202115],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202119],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202208],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202209],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202214],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202222],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202224],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202305],
    loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d202310],

    scale=iw*1:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202104],
    scale=iw*1:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202105],
    scale=iw*1:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202106],
    scale=iw*1:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202107],
    scale=iw*1:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202109],
    scale=iw*2:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202111],
    scale=iw*2:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202115],
    scale=iw*2:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202119],
    scale=iw*4:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202208],
    scale=iw*4:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202209],
    scale=iw*6:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202214],
    scale=iw*6:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202222],
    scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202224],
    scale=iw*3:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202305],
    scale=iw*3:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v202310],

    [d202104]
    [v202104]
    [d202105]
    [v202105]
    [d202106]
    [v202106]
    [d202107]
    [v202107]
    [d202109]
    [v202109]
    [d202111]
    [v202111]
    [d202115]
    [v202115]
    [d202119]
    [v202119]
    [d202208]
    [v202208]
    [d202209]
    [v202209]
    [d202214]
    [v202214]
    [d202222]
    [v202222]
    [d202224]
    [v202224]
    [d202305]
    [v202305]
    [d202310]
    [v202310]
    concat=n=30
  ' \
  -fps_mode vfr \
  output.mp4
