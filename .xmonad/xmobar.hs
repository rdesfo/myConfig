Config { font = "-misc-fixed-10-*-*-*-*-*-*-*-*"
       , bgColor = "black"
       , fgColor = "grey"
       , border = TopB
       , borderColor = "black"
       , allDesktops = False
       , overrideRedirect = False
       , hideOnStart = False
       , persistent = True
       , position = TopW L 90
       , lowerOnStart = False
       , commands = [ Run Weather "KPWM" ["-t","Portland: <tempF>F  <skyCondition>","-L","32","-H","80","--normal","green","--high","red","--low","lightblue"] 36000
                    , Run MultiCpu ["-t","Cpu: <autototal>%"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Battery ["BAT0"] 10
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %multicpu% | %memory% * %swap% | %battery% | %KPWM% | <fc=#ee9a00>%date%</fc>"
       }
