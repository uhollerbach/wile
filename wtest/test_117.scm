(let ((str1 @<<FOO
now is the time
for all good men and true
to come to the aid of their party
FOO
)
      (str2 #<<BAR
	    >NOW IS THE TIME
	    >FOR ALL GOOD MEN AND TRUE
	    >TO COME TO THE AID OF THEIR PARTY
BAR
)
      (str3 #<<BAZ
	    >The Moving Finger writes; and, having writ,
	    >Moves on: nor all your Piety nor Wit
	    >Shall lure it back to cancel half a Line,
	    >Nor all your Tears wash out a Word of it.
	    >BAZ
	    )
;;;      (str4 #<<QUUX
;;;	    >And that inverted Bowl they call the Sky,
;;;	    >Whereunder crawling coop'd we live and die,
;;;	    >Lift not your hands to It for help--for It
;;;	    >As impotently moves as you or I.
;;;	    >QUUX)
      )
  (write-string str1 #\newline)
  (write-string str2 #\newline)
  (write-string str3 #\newline)
;;;  (write-string str4 #\newline)
  )
