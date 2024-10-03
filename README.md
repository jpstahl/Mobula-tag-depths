# Mobula-tag-depths

Purpose

Code to display mobula ray tag depth data for tags that popoff in the western hemisphere. Similar code can be found in the repository Mobula-tag-depths (https://github.com/jpstahl/Mobula-tag-depths) that was developed for tags that popoff in the eastern hemisphere.

The code was developed for Wildlife computers survival archival tags (sPAT tags) programmed to 60 days. These tags are deployed in the Hawaii longline fishery in western hemisphere (negative longitude).

I created separate branches for each deployed tag as there are some specific configurations that needed to be changed in the code such as tag number, popoff time zone, and depth range. 

Line plots

The line plots show vertical movements for the last few day of the tag deployment with day/night shading with date/time in the tag popoff time zone to illistrate any patterns with movements and light patterns. The function in the markdown file refers to code in the "Tag depth line plots.R" file created by Bret Cooper using ggplot. Similar plots were originally created in Rchivaltag package; however, issues occurred with plotting in time zones that were not UTC so this code was developed.

Boxplots

The boxplots show vertical movements summarized by hour in 24 hour period for the last few days of the tag deployment with day/night shading with date/time in the tag popoff time zone to illistrate any patterns with movements and light patterns. The function in the markdown file refers to code in the "Tag depth boxplot plots.R" file created by Bret Cooper using ggplot. Similar plots were originally created in Rchivaltag package; however, issues occurred with plotting in time zones that were not UTC so this code was developed.
