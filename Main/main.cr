# The main and comain loop have specific features. Main is the main programming loop for I/O, engine init, gui view etc
# The comain on the other hand is for handling concurrent tasks, errors, logging and handling backgrounds task
# Main and CoMain share a terminal emulator mixin, which is native to the Shrine engine. Commands = shrine -help
module Main
    class Enter
        puts "Hello World!"
        # Will add main loop here
    end
end



