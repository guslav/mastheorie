# mastheorie

Der eigentlich Quellcode ist in der Datei app/Main.hs

Passe die Zeile mit omega = "abc" an, wenn du willst, dass die Berechnung für ein größeres oder kleineres \( \Omega \) berechnet wird. Aber Achtung, du wirst große Werte nicht berechnen können. Der Code ist nicht für Performance geschrieben und aufgrund der vielen Möglichkeiten wird das auch nicht möglich sein.

Wenn du das Programm stack installiert hast, kannst du mit "stack build" das Projekt installieren. In der Konsole wird dann geschrieben, wo die executable abgelegt wird. Alternativ kann man natürlich auch "ghc Main.hs -o ausgabedatei" schreiben.
