Componenti del gruppo:

Niccolò Balzarotti 852003
Covelli Matteo 861277

jsonparse.lisp:
Permette la costruzione delle strutture dati che rappresentino degli oggetti
JSON a partire dalla loro rappresentazione come stringhe.

jsonparse (string-in):
Data in input una stringa, la trasforma in un JSON.

jsonaccess (JSON &rest fields):
Permette di ottenere un value presente all'interno del JSON.

jsonread (filename):
Permette di creare un JSON a partire da una stringa in un file.json.

jsondump (JSON filename):
Permette di scrivere all'interno di un file.json l'oggetto JSON.
(Se il file non esiste viene creato)