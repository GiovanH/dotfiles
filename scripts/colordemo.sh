#!/bin/bash

e="\033["
vline=$(tput smacs 2>/dev/null; printf 'x'; tput rmacs 2>/dev/null)
[ "$vline" = "x" ] && vline="|"

printf "${e}1;4mf\\\\b${e}0m${e}4m none  white    black     red     \
green    yellow   blue    magenta    cyan  ${e}0m\\n"

rows='brgybmcw'

for f in 0 7 `seq 6`; do
  no=""; bo=""; p=""
  for b in n 7 0 `seq 6`; do
    co="3$f"; [ $b = n ] || co="$co;4$b"
    no="${no}${e}${co}m  ${p}${co} ${e}0m"
    bo="${bo}${e}1;${co}m${p}1;${co} ${e}0m"
    p=" "
  done
  fc=$(echo $rows | cut -c$((f+1)))
  printf "$fc $vline$no\nb$fc$vline$bo\n"
done

for fgbg in 38 48 ; do # Foreground / Background
  for color in {0..255} ; do # Colors
    # Display the color
    printf "\e[${fgbg};5;%sm  %3s  \e[0m" $color $color
    # Display 6 colors per lines
    if [ $((($color + 1) % 6)) == 4 ] ; then
      echo # New line
    fi
  done
  echo # New line
done

echo -e "${COLOR_BOLD}COLOR_BOLD${COLOR_NC}"
echo -e "${COLOR_NC}COLOR_NC${COLOR_NC}"
echo -e "${COLOR_WHITE}COLOR_WHITE${COLOR_NC}"
echo -e "${COLOR_BLACK}COLOR_BLACK${COLOR_NC}"
echo -e "${COLOR_BLUE}COLOR_BLUE${COLOR_NC}"
echo -e "${COLOR_LIGHT_BLUE}COLOR_LIGHT_BLUE${COLOR_NC}"
echo -e "${COLOR_GREEN}COLOR_GREEN${COLOR_NC}"
echo -e "${COLOR_LIGHT_GREEN}COLOR_LIGHT_GREEN${COLOR_NC}"
echo -e "${COLOR_CYAN}COLOR_CYAN${COLOR_NC}"
echo -e "${COLOR_LIGHT_CYAN}COLOR_LIGHT_CYAN${COLOR_NC}"
echo -e "${COLOR_RED}COLOR_RED${COLOR_NC}"
echo -e "${COLOR_LIGHT_RED}COLOR_LIGHT_RED${COLOR_NC}"
echo -e "${COLOR_PURPLE}COLOR_PURPLE${COLOR_NC}"
echo -e "${COLOR_LIGHT_PURPLE}COLOR_LIGHT_PURPLE${COLOR_NC}"
echo -e "${COLOR_BROWN}COLOR_BROWN${COLOR_NC}"
echo -e "${COLOR_YELLOW}COLOR_YELLOW${COLOR_NC}"
echo -e "${COLOR_GRAY}COLOR_GRAY${COLOR_NC}"
echo -e "${COLOR_LIGHT_GRAY}COLOR_LIGHT_GRAY${COLOR_NC}"