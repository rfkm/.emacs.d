on run argv
    tell application "Google Chrome"
        if ("reload" = item 1 of argv) then
            tell window 1
                tell active tab
                    reload
                end tell
            end tell
        end if
        if ("stop" = item 1 of argv) then
            tell window 1
                tell active tab
                    stop
                end tell
            end tell
        end if
    end tell
end run
