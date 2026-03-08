#!/bin/bash

ffmpeg_clip() {
  FORMATS="['%H:%M:%S', '%M:%S', '%H:%M:%S.%f', '%M:%S.%f']"
  if [ -z "$3" ]; then
    echo "Usage: ffmpeg_clip <filepath> <starttime> <endtime>"
    echo "Time formats: $FORMATS"
    return
  fi
  (set -eu -o pipefail
    FILE_PATH="$1"
    STARTTIME="$2"
    ENDTIME="$3"

    splitpath "$FILE_PATH"
    OUT_PATH="${filestem} $(echo "$STARTTIME-$ENDTIME" | sed 's/:/./g').${FFMPEG_EXT:-${extension}}"
    ACCURATE_SEEK="00:00:00"

    DUR=$(cat << EOF | python3
import datetime
import sys
lastexec = None

def strptime(val):
  for format_ in $FORMATS:
    try:
      return datetime.datetime.strptime(val, format_)
    except ValueError as e:
      lastexec = e
      continue
  sys.stderr.write(str(lastexec) + "\n")
  return val
  # sys.exit(1)

start = strptime('$STARTTIME')
end = strptime('$ENDTIME')
print(end - start)
sys.exit()
EOF
)

    (set -x; ffmpeg -hide_banner ${FFMPEG_EXTRA:-} -ss $STARTTIME -i "$FILE_PATH" -ss $ACCURATE_SEEK -t $DUR "$OUT_PATH")
  )
}

# Run ffmpeg at a calculated bitrate to make 8mb mp4s.
ffmpeg_compress() {
  if [ -z "$1" ]; then
    echo "Usage: ffmpeg_compress <filepath> [target size in mb (default 8)]"
    return
  fi
  (set -eu -o pipefail
  mb=${2:-15} # default to 15 mb
  maxwidth=${3:-1280} # default to 720p
  IFS=, read secs src_bitrate < <(ffprobe -i "$1" -show_entries format=duration,bit_rate -of 'csv=p=0' 2>/dev/null)
  src_bitrate_audio=$(ffprobe -v 0 -select_streams a:0 -show_entries stream=bit_rate -of compact=p=0:nk=1 -i "$1")
  akbs=$(echo "($mb*1800)/$secs" | bc)
  vkbs=$(echo "($mb*6500)/$secs" | bc)
  # Sanity check: Don't set bitrate higher than source
  srckbs=$(echo "$src_bitrate/1000" | bc)
  vkbs=$(( vkbs > srckbs ? srckbs : vkbs ))
  # If audio bitrate is not defined, audio (and total file) may be larger than source
  if [[ "$src_bitrate_audio" != "N/A" ]]; then
    srckbs_a=$(echo "$src_bitrate_audio/1000" | bc)
    akbs=$(( akbs > srckbs_a ? srckbs_a : akbs ))
  fi
  echo "Audio: ${akbs}k, Video: ${vkbs}k"
  splitpath "$1"
  #  -c:v libx265
  (set -x; ffmpeg -hide_banner \
    ${FFMPEG_EXTRA:-} \
    -i "$1" \
    -vf "scale='min(${maxwidth},iw)':-2" -b:v ${vkbs}k -b:a ${akbs}k \
    "${filestem}_${mb}mb.${FFMPEG_EXT:-"mp4"}"
  )
  # (set -x; ffmpeg -hide_banner -i "$1" -b:v ${vkbs}k -b:a ${akbs}k "$1_${mb}mb.mp4")
  )
}
alias discordmp4="ffmpeg_compress"

ffmpeg_clip_compress ()
{
    FORMATS="['%H:%M:%S', '%M:%S', '%H:%M:%S.%f', '%M:%S.%f']";
    if [ -z "$3" ]; then
        echo "Usage: ffmpeg_clip <filepath> <starttime> <endtime> [mb (15)] [maxwidth (800)]";
        echo "Time formats: $FORMATS";
        echo "Use FFMPEG_EXTRA to add params, like FFMPEG_EXTRA='-map_metadata -1'";
        echo "Use FFMPEG_EXT to override the output extension";
        return;
    fi;
    ( set -eu -o pipefail;
    FILE_PATH="$1";
    STARTTIME="$2";
    ENDTIME="$3";

    mb=${4:-15};
    maxwidth=${5:-800};

    splitpath "$FILE_PATH";
    OUT_PATH="${filestem} $(echo "$STARTTIME-$ENDTIME" | sed 's/:/./g').${FFMPEG_EXT:-"mp4"}";

    # Clip
    ACCURATE_SEEK="00:00:00";
    DUR=$(cat <<EOF |
import datetime
import sys
lastexec = None

def strptime(val):
  for format_ in $FORMATS:
    try:
      return datetime.datetime.strptime(val, format_)
    except ValueError as e:
      lastexec = e
      continue
  sys.stderr.write(str(lastexec) + "\n")
  return datetime.datetime.utcfromtimestamp(float(val))
  # sys.exit(1)

start = strptime('$STARTTIME')
end = strptime('$ENDTIME')
print(end - start)
sys.exit()
EOF
  python3);

    secs=$(cat <<EOF |
import datetime
import sys
lastexec = None

def strptime(val):
  for format_ in $FORMATS:
    try:
      return datetime.datetime.strptime(val, format_)
    except ValueError as e:
      lastexec = e
      continue
  sys.stderr.write(str(lastexec) + "\n")
  return datetime.datetime.utcfromtimestamp(float(val))
  # sys.exit(1)

start = strptime('$STARTTIME')
end = strptime('$ENDTIME')
print(int((end - start).total_seconds()))
sys.exit()
EOF
  python3);

    # Compress
    IFS=, read _ src_bitrate < <(ffprobe -i "$1" -show_entries format=duration,bit_rate -of 'csv=p=0' 2> /dev/null);
    src_bitrate_audio=$(ffprobe -v 0 -select_streams a:0 -show_entries stream=bit_rate -of compact=p=0:nk=1 -i "$1");
    akbs=$(echo "($mb*1800)/$secs" | bc);
    vkbs=$(echo "($mb*6500)/$secs" | bc);
    srckbs=$(echo "$src_bitrate/1000" | bc);
    vkbs=$(( vkbs > srckbs ? srckbs : vkbs ));
    if [[ "$src_bitrate_audio" != "N/A" || "${force_ext:-}" == "webm" ]]; then
        if [[ "${force_ext:-}" == "webm" ]]; then
          srckbs_a="512"; # Max bitrate for libopus
        else
          srckbs_a=$(echo "$src_bitrate_audio/1000" | bc);
        fi;
        akbs=$(( akbs > srckbs_a ? srckbs_a : akbs ));
    fi;
    echo "Audio: ${akbs}k, Video: ${vkbs}k";

    set -x;
    ffmpeg -hide_banner \
      ${FFMPEG_EXTRA:-} \
      -ss $STARTTIME \
      -i "$FILE_PATH" \
      -ss $ACCURATE_SEEK -t $DUR \
      -vf "scale='min(${maxwidth},iw)':-2" -b:v ${vkbs}k -b:a ${akbs}k \
      "$OUT_PATH"
  )
}
