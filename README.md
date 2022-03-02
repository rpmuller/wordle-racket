# wordle-racket
A simple wordle clone written in Racket and using the 2htdp/image library for the graphics.

To start a new game, load the file in DrRacket, then grab a game instance by typing into the REPL:

    (define game (play-wordle))
    
Guesses are made by typing words into the game instance:
    
    (game "audio") ; My standard first word
    
The REPL will respond with the colored text and a keyboard image of guessed letters.

There's a known bug with double letters that I have yet to fix.

Next step is to use the 2htdp libraries to make a gui version.

It's also worth looking at [Peter Norvig's Wordle Pytude](https://github.com/norvig/pytudes/blob/main/ipynb/Wordle.ipynb) to see how a really excellent programmer tackles this. I only found the site as I was uploading this program, and I expect it will lead to some simplifications in my code.
