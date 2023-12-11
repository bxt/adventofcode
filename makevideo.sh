# Turns all visualisations into one big video.
#
# To find the possible days to generate gifs for run:
# > find . -type d -name target -prune -o -type f -iname 'vis*' -print | sort
#
# To find the days the gifs were already rendered for run:
# > find . -type d -name target -prune -o -iname '*.gif' -print | sort

# A JSON defining what days we have:

LIST=$(
cat <<'EOF'
[
  { "year": 2021, "day": "04", "file": "output.gif", "scale": 1 },
  { "year": 2021, "day": "05", "file": "output.gif", "scale": 1 },
  { "year": 2021, "day": "06", "file": "output.gif", "scale": 1 },
  { "year": 2021, "day": "07", "file": "output.gif", "scale": 1 },
  { "year": 2021, "day": "09", "file": "output.gif", "scale": 1 },
  { "year": 2021, "day": "11", "file": "output.gif", "scale": 2 },
  { "year": 2021, "day": "15", "file": "output.gif", "scale": 2 },
  { "year": 2021, "day": "19", "file": "output.gif", "scale": 2 },
  { "year": 2022, "day": "08", "file": "output.gif", "scale": 4 },
  { "year": 2022, "day": "09", "file": "output.gif", "scale": 4 },
  { "year": 2022, "day": "14", "file": "output.gif", "scale": 6 },
  { "year": 2022, "day": "22", "file": "output.gif", "scale": 6 },
  { "year": 2022, "day": "24", "file": "output.gif", "scale": 5 },
  { "year": 2023, "day": "05", "file": "output2.gif", "scale": 3 },
  { "year": 2023, "day": "10", "file": "output.gif", "scale": 3 }
]
EOF
)

# First we generate some title screens:

cd 2022
  cargo run --package adventofcode2022 --bin adventofcode2022drawtitle -r
  echo "$LIST" | jq -r '.[] | "./target/release/adventofcode2022drawtitle \(.year|@sh) \(.day|@sh)"' | sh
cd ..

# You can use this for debugging filter pipelines:

# echo 'nullsrc,scale=iw*3:-1[v0],nullsrc,scale=iw*3:-1[v1],[v0][v1] hstack,nullsink' | \
# graph2dot -o graph.tmp && \
# dot -Tpng graph.tmp -o graph.png && \
# open graph.png

# Humungous command to concat title screens and gifs:

ffmpeg \
  $(echo "$LIST" | jq -r '.[] | "-i 2022/target/output-\(.year|@sh)-\(.day).gif"') \
  $(echo "$LIST" | jq -r '.[] | "-i \(.year|@sh)/day\(.day)/\(.file)"') \
  -movflags faststart -pix_fmt yuv420p \
  -filter_complex "$(
      echo "$LIST" | jq -jr '.[] | "loop=loop=20:size=1,scale=iw*5:-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [d\(.year)\(.day)],"'
      echo "$LIST" | jq -jr '.[] | "scale=iw*\(.scale):-1:flags=neighbor,pad=2560:1600:-1:-1:001122 [v\(.year)\(.day)],"'
      echo "$LIST" | jq -jr '.[] | "[d\(.year)\(.day)] [v\(.year)\(.day)] "'
      echo "$LIST" | jq -jr 'length | . * 2 | "concat=n=\(.)"'
    )" \
  -fps_mode vfr \
  output.mp4
