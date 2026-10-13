# Gordon and Hirtle, Chart 2 read by pixel

Run 13 October 2026 under PREREG_stage1_3.md (commit 51a5a4d3), which fixes
the reading method. The top panel of Chart 2 (PDF page 11) is rendered at
300 dpi; the vertical scale is set by the gridlines (250 at row 576, zero
at row 1047), the years by the axis ticks (1990 at x 796, 162 pixels per
five years); each reading is the first pixel from the top darker than 185
in a three-pixel column at the year.

## Findings

- The plotted area holds 34 yearly points, 1990 to 2023, by the tick
  spacing, although the chart's title says 1990-2024.
- The readings sum to 6,187. Their Table 1 counts 6,448 bank-years, 261
  more, about the size of one late year, so the chart most likely omits
  2024. The readings are approximate (stated precision: the nearest 5) and
  enter no verdict.

Pinned: /home/anuragr/.cache/wealth_garch/gordon_hirtle_2026.pdf sha256 f1fe4e9c87a9ee8b00248446555a41ed7c339d2c3ff9404382fd408c5510e64e

Reproduce: python3 chart2.py /home/anuragr/.cache/wealth_garch/gordon_hirtle_2026.pdf

```
gordon_hirtle_2026.pdf sha256 f1fe4e9c87a9ee8b00248446555a41ed7c339d2c3ff9404382fd408c5510e64e
Chart 2 top panel, PDF page 11 at 300 dpi: zero at row 1047, 250 at row 576; 1990 at x 796, 32.4 px a year; 34 points
1990 reading  165.6 nearest 5: 165
1991 reading  166.1 nearest 5: 165
1992 reading  158.7 nearest 5: 160
1993 reading  150.7 nearest 5: 150
1994 reading  146.5 nearest 5: 145
1995 reading  143.8 nearest 5: 145
1996 reading  139.6 nearest 5: 140
1997 reading  133.8 nearest 5: 135
1998 reading  129.0 nearest 5: 130
1999 reading  139.6 nearest 5: 140
2000 reading  150.7 nearest 5: 150
2001 reading  157.1 nearest 5: 155
2002 reading  165.6 nearest 5: 165
2003 reading  172.5 nearest 5: 175
2004 reading  164.0 nearest 5: 165
2005 reading  176.2 nearest 5: 175
2006 reading  179.4 nearest 5: 180
2007 reading  185.2 nearest 5: 185
2008 reading  179.4 nearest 5: 180
2009 reading  179.4 nearest 5: 180
2010 reading  170.4 nearest 5: 170
2011 reading  168.8 nearest 5: 170
2012 reading  178.3 nearest 5: 180
2013 reading  188.4 nearest 5: 190
2014 reading  195.3 nearest 5: 195
2015 reading  214.4 nearest 5: 215
2016 reading  228.2 nearest 5: 230
2017 reading  230.4 nearest 5: 230
2018 reading  234.1 nearest 5: 235
2019 reading  225.6 nearest 5: 225
2020 reading  228.2 nearest 5: 230
2021 reading  237.8 nearest 5: 240
2022 reading  239.9 nearest 5: 240
2023 reading  264.3 nearest 5: 265
sum of readings 6187
```
