Config { font = "-misc-fixed-10-*-*-*-*-*-*-*-*"
       , bgColor = "#242424"
       , fgColor = "grey"
       , border = TopB
       , borderColor = "#242424"
       , allDesktops = False
       , overrideRedirect = False
       , hideOnStart = False
       , persistent = True
       , position = TopSize L 100 20
       , lowerOnStart = False
       , commands = [ Run Weather "KPWM" ["-t","Portland: <tempF>F  <skyCondition>","-L","32","-H","80","--normal","green","--high","red","--low","lightblue"] 36000
                    , Run MultiCpu ["-t","Cpu: <autototal>%"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run BatteryP ["BAT0"] ["-t", "Bat: <left>%", "-L", "10", "-H", "80", "-p", "3", "--", "-O", "<fc=green>On</fc> - ", "-i", "","-L", "-15", "-H", "-5","-l", "red", "-m", "blue", "-h", "green"] 10
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    , Run Com "kquitapp" ["plasma-desktop", "&&", "sleep", "15s", "&&", "plasma-desktop"] "" 0
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ <fc=#ee9a00>%date%</fc>                   | %multicpu% | %memory% * %swap% | %battery% | %KPWM% "
--       , template = "%StdinReader% }{ %multicpu% | %memory% * %swap% | %KPWM% | <fc=#ee9a00>%date%</fc>"
       }
