% Balzarotti Niccolò 852003
% Covelli Matteo 861277

% jsonparse/2 esegue il parse su una stringa trasformandola in un object
jsonparse(StringIn, Result) :-
   string_codes(StringIn, CharCodeOut),
   rimuovi_spazi(CharCodeOut, CharCodeOut2),
   json_object(CharCodeOut2, CharCodeOut3, [], Result),
   rimuovi_spazi(CharCodeOut3, []),
   !.

% jsonparse/2 esegue il parse su una stringa trasformandola in un array
jsonparse(StringIn, Result) :-
   string_codes(StringIn, CharCodeOut),
   rimuovi_spazi(CharCodeOut, CharCodeOut2),
   json_array(CharCodeOut2, CharCodeOut3, [], Result),
   rimuovi_spazi(CharCodeOut3, []),
   !.

% json_object/4 caso in cui la stringa in input è vuota
json_object(CharCodeIn, CharCodeOut, List, jsonobj(Result)) :-
   primo_carattere("{", CharCodeIn, CharCodeOut2),
   rimuovi_spazi(CharCodeOut2, CharCodeOut3),
   primo_carattere("}", CharCodeOut3, CharCodeOut),
   !,
   List = Result.

% json_object/4 riconosce se la stringa passata in input è un object
json_object(CharCodeIn, CharCodeOut, List, jsonobj(Result)) :-
   primo_carattere("{", CharCodeIn, CharCodeOut2),
   !,
   rimuovi_spazi(CharCodeOut2, CharCodeOut3),
   jsonmembers(CharCodeOut3, CharCodeOut4, List, Result),
   rimuovi_spazi(CharCodeOut4, CharCodeOut5),
   primo_carattere("}", CharCodeOut5, CharCodeOut).

% json_array/4 caso in cui la stringa in input è vuota
json_array(CharCodeIn, CharCodeOut, List, jsonarray(Result)) :-
   primo_carattere("[", CharCodeIn, CharCodeOut2),
   rimuovi_spazi(CharCodeOut2, CharCodeOut3),
   primo_carattere("]", CharCodeOut3, CharCodeOut),
   !,
   List = Result.

% json_array/4 Riconosce se la stringa passata in input è un array
json_array(CharCodeIn, CharCodeOut, List, jsonarray(Result)) :-
   primo_carattere("[", CharCodeIn, CharCodeOut2),
   !,
   rimuovi_spazi(CharCodeOut2, CharCodeOut3),
   jsonelements(CharCodeOut3, CharCodeOut4, List, Result),
   rimuovi_spazi(CharCodeOut4, CharCodeOut5),
   primo_carattere("]", CharCodeOut5, CharCodeOut).

% json_members/4 viene utilizzata nel caso della json_object, per
% il caso in cui ho più pair
jsonmembers(CharCodeIn, CharCodeOut, List, Result) :-
   pair(CharCodeIn, CharCodeOut7, List, Result2),
   rimuovi_spazi(CharCodeOut7, CharCodeOut2),
   primo_carattere(",", CharCodeOut2, CharCodeOut3),
   rimuovi_spazi(CharCodeOut3, CharCodeOut4),
   jsonmembers(CharCodeOut4, CharCodeOut, Result2, Result).

% json_members/4 viene utilizzata nel caso della json_object, per il
% caso in cui ho un solo pair
jsonmembers(CharCodeIn, CharCodeOut, List, Result) :-
    pair(CharCodeIn, CharCodeOut, List, Result),
    !.

% json_elements/4 viene utilizzata nel caso della json_array, per il
% caso in cui ho più elementi
jsonelements(CharCodeIn, CharCodeOut, List, Result) :-
   jsonvalue(CharCodeIn, CharCodeOut2, List2),
   rimuovi_spazi(CharCodeOut2, CharCodeOut3),
   primo_carattere(",", CharCodeOut3, CharCodeOut4),
   rimuovi_spazi(CharCodeOut4, CharCodeOut5),
   append(List, [List2], List3),
   jsonelements(CharCodeOut5, CharCodeOut, List3, Result).

% json_elements/4 viene utilizzata nel caso della json_array, per il
% caso in cui ho un solo elemento
jsonelements(CharCodeIn, CharCodeOut, List, Result) :-
   jsonvalue(CharCodeIn, CharCodeOut, List2),
   append(List, [List2], Result),
   !.

% pair/4 dato l'input riconosce le coppie Attribute,Value
pair(CharCodeIn, CharCodeOut, List, Result) :-
   jsonstring(CharCodeIn, CharCodeOut2, Att),
   rimuovi_spazi(CharCodeOut2, CharCodeOut3),
   primo_carattere(":", CharCodeOut3, CharCodeOut4),
   rimuovi_spazi(CharCodeOut4, CharCodeOut5),
   jsonvalue(CharCodeOut5, CharCodeOut, Value),
   append(List, [(Att,Value)], Result).

% json_string/3 dato l'input riconosce la stringa delimitata dai due
% doppi apici
jsonstring(CharCodeIn, CharCodeOut, Att) :-
   primo_carattere("\"", CharCodeIn, CharCodeOut2),
   get_attribute_double(CharCodeOut2, CharCodeOut3, Result),
   primo_carattere('\"', CharCodeOut3, CharCodeOut),
   string_codes(Att, Result).

% get_attribute_double/3 viene utilizzata nella json_string, riconosce
% il contenuto tra i due doppi apici
get_attribute_double([], _, _) :- !.
get_attribute_double([X | Xs], [X | Xs], []) :-
    string_codes("\"", [Char | _]),
    X = Char,
    !.
get_attribute_double([X | Xs], [X | Xs], []) :-
    string_codes("\'", [Char | _]),
    X = Char,
    !,
    fail.
get_attribute_double([X | Xs], Zs, [X | Ys]) :-
   get_attribute_double(Xs, Zs, Ys).

jsonvalue(CharCodeIn, CharCodeOut, Value) :-
   jsonbool(CharCodeIn, CharCodeOut, Value),
   !.
% json_value/3 utilizzata per gestire il caso in cui i valori passati in
% input sono stringhe
jsonvalue(CharCodeIn, CharCodeOut, Value) :-
   jsonstring(CharCodeIn, CharCodeOut, Value),
   !.

% json_value/3 utilizzata per gestire il caso in cui i valori passati in
% input sono innestati
jsonvalue(CharCodeIn, CharCodeOut, Value) :-
   jsoninside(CharCodeIn, CharCodeOut, Value),
   !.

% json_value/3 utilizzata per gestire il caso in cui i valori passati in
% input sono numeri
jsonvalue(CharCodeIn, CharCodeOut, Value) :-
   jsonnumber(CharCodeIn, CharCodeOut, Value),
   !.

% json_inside/3 utilizzata per gestire il caso di input innestati con un
% array
jsoninside(CharCodeIn, CharCodeOut, Result) :-
   json_array(CharCodeIn, CharCodeOut, [], Result),
   !.

% json_inside/3 utilizzata per gestire il caso di input innestati con un
% object
jsoninside(CharCodeIn, CharCodeOut, Result) :-
   json_object(CharCodeIn, CharCodeOut, [], Result),
   !.

% json_number/3 utilizzata per gestire il caso di input numerici
% decimali,negativi
jsonnumber(CharCodeIn, CharCodeOut, Result) :-
   primo_carattere("-", CharCodeIn, CharCodeOut2),
   char_code("-", Minus),
   create_number(CharCodeOut2, CharCodeOut1, Number),
   primo_carattere(".", CharCodeOut1, CharCodeOut3),
   !,
   char_code(".", Dot),
   create_number(CharCodeOut3, CharCodeOut, Decimal),
   append([Minus], Number, MinusNumber),
   append(MinusNumber, [Dot], NumberDot),
   append(NumberDot, Decimal, CompleteNumber),
   number_codes(Result, CompleteNumber).

% json_number/3 utilizzata per gestire il caso di input numerici
% interi,negativi
jsonnumber(CharCodeIn, CharCodeOut, Result) :-
   primo_carattere("-", CharCodeIn, CharCodeOut2),
   create_number(CharCodeOut2, CharCodeOut, Number),
   char_code("-", Minus),
   append([Minus], Number, MinusNumber),
   number_codes(Result, MinusNumber).

% json_number/3 utilizzata per gestire il caso di input numerici
% decimali positivi
jsonnumber(CharCodeIn, CharCodeOut, Result) :-
   create_number(CharCodeIn, CharCodeOut1, Number),
   primo_carattere(".", CharCodeOut1, CharCodeOut2),
   !,
   char_code(".", Dot),
   create_number(CharCodeOut2, CharCodeOut, Decimal),
   append(Number, [Dot], NumberDot),
   append(NumberDot, Decimal, CompleteNumber),
   number_codes(Result, CompleteNumber).

% json_number/3 utilizzata per gestire il caso di input numerici interi
% positivi
jsonnumber(CharCodeIn, CharCodeOut, Result) :-
   create_number(CharCodeIn, CharCodeOut, Number),
   not_empty(Number),
   number_codes(Result, Number).

% create_number/3 utilizzata all'interno della json_number per creare il
% numero partendo dai caratteri in input
create_number([X | Xs], [X | Xs], []) :-
   X < 48,
   !.
create_number([X | Xs], [X | Xs], []) :-
   X > 57,
   !.
create_number([X | Xs], Zs, [X | Ys]) :-
       create_number(Xs, Zs, Ys).

%jsonbool/2 utilizzata per gestire il caso di input booleani
jsonbool(CharCodeIn, CharCodeOut, Result) :-
   create_boolean(CharCodeIn, CharCodeOut, Bool),
   not_empty(Bool),
   atom_codes(App, Bool),
   boolean_trovato(App),
   atom_codes(Result, Bool).

% boolean_trovato/2 utilizzata all'interno della jsonbool per
% controllare che il valore booleano sia corretto
boolean_trovato(Bool) :-
   Bool = true,
   !.
boolean_trovato(Bool) :-
   Bool = false,
   !.
boolean_trovato(Bool) :-
   Bool = null,
   !.

create_boolean([X|Xs], [X|Xs], []) :-
   spazio_trovato(X),
   !.
create_boolean([X|Xs], [X|Xs], []) :-
   X = 44,
   !.
create_boolean([X|Xs], [X|Xs], []) :-
   X = 125,
   !.
create_boolean([X|Xs], [X|Xs], []) :-
   X = 93,
   !.
create_boolean([X|Xs], Zs, [X|Ys]) :-
   create_boolean(Xs, Zs, Ys).

% primo_carattere/3, restituisce una lista di caratteri
% togliendo il primo se uguale a Char (carattere dato in input).
primo_carattere(Char, [X | Xs], JSONCharsOut) :-
    string_codes(Char, [Y | _]),
    Y = X,
    JSONCharsOut = Xs.

% rimuovi_spazi/2, dato in input una lista di caratteri
% produce come output la stessa lista rimuovendo gli spazi.
rimuovi_spazi([], []) :- !.
rimuovi_spazi([X | Xs], Ys) :-
    spazio_trovato(X),
    !,
    rimuovi_spazi(Xs, Ys).
rimuovi_spazi([X | Xs], Ys) :-
    Ys = [X | Xs],
    !.

% spazio_trovato/1 viene utilizzata in rimuovi_spazi/2, e controlla
% se il carattere in input è uno spazio verificando i tre possibili
% casi.
spazio_trovato(X) :-
    spazio(X),
    !.
spazio_trovato(X) :-
    spazio_invio(X),
    !.
spazio_trovato(X) :-
    spazio_tab(X),
    !.

% spazio/1 viene utilizzata nella spazio_trovato/1 per vericare se è
% presente uno spazio (3 possibili casi)
spazio(X) :-
    char_code(' ', Y),
    X = Y,
    !.
spazio_invio(X) :-
    char_code('\n', Y),
    X = Y,
    !.
spazio_tab(X) :-
    char_code('\t', Y),
    X = Y,
    !.

% not_empty/1 verifica se la lista non è vuota
not_empty(List) :- List \= [], !.

% jsonaccess/3 viene utilizzata per ottenere un value presente
% all'interno del json
jsonaccess(jsonobj(X), [], jsonobj(X)) :- !.
jsonaccess(jsonobj(X), "", jsonobj(X)) :- !.
jsonaccess(jsonobj(), _, _) :- !, fail.
jsonaccess(jsonarray(), _, _) :- !, fail.
jsonaccess(Json, [X], Result) :-
   access_elements(Json, X, Result),
   !.
jsonaccess(Json, [X|Xs], Result) :-
   access_elements(Json, X, App),
   !,
   jsonaccess(App, Xs, Result).
jsonaccess(Json, X, Result) :-
   access_elements(Json, X, Result),
   !.

% access_elements/3 viene utilizzata all'interno della jsonaccess/3 per
% riconoscere se il json in input è un object o un array
access_elements(Json, Fields, Result) :-
    jsonobj([Y|Ys]) = Json,
    !,
    access_member([Y|Ys], Fields, Result).
access_elements(Json, Fields , Result) :-
    jsonarray([X|Xs]) = Json,
    !,
    access_element_position([X | Xs], Fields, Result).

% access_member/3 viene utilizzata all'interno di access_elements/3 per
% cercare il value corrispondente
access_member([], _, _) :- fail.
access_member([(X,Y)| _], Z, Result) :-
    string(Z),
    X = Z,
    !,
    Result = Y.
access_member([_| Xs], Z, Result) :-
    string(Z),
    access_member(Xs, Z, Result).

% access_element_position/3 viene utilizzata all'interno di
% access_elements/3 per cercare l'elemento all'interno dell'array
access_element_position([], [_], _) :- fail.
access_element_position([X | _], Y, Result) :-
    number(Y),
    Y = 0,
    !,
    Result = X.
access_element_position([_ | Xs], Y, Result) :-
    number(Y),
    Z is Y-1,
    access_element_position(Xs, Z, Result).

% jsonread/2 permette di creare un JSON a partire da
% una stringa in un file.json
jsonread(Filename, JSON) :-
    open(Filename, read, Input),
    read_stream_to_codes(Input, X),
    close(Input),
    atom_codes(InputString, X),
    jsonparse(InputString, JSON).

% jsondump/2 permette di scrivere all'interno di un file.json.(se il
% file non esiste viene creato)
jsondump(JSON, Filename) :-
    open(Filename, write, Out),
    json_write(JSON, StringIn),
    write(Out, StringIn),
    close(Out).

% json_write/2 viene utilizzata nella jsondump/2 per scrivere un json
% di tipo object. (caso in cui l'object è vuoto)
json_write(JSON, StringIn) :-
   JSON = jsonobj([]),
   !,
   StringIn = "{}".

% json_write/2 caso in cui l'object non è vuoto
json_write(JSON, StringIn) :-
    jsonobj([Y | Ys]) = JSON,
    !,
    string_concat("", "{", StringIn1),
    json_write_object([Y | Ys], "", StringIn2),
    string_concat(StringIn1, StringIn2, StringIn3),
    string_concat(StringIn3, "}", StringIn).

% json_write/2 utilizzata nella jsondump/2 per scrivere un json di tipo
% array. (caso in cui l'array è vuoto)
json_write(JSON, StringIn) :-
   JSON = jsonarray([]),
   !,
   StringIn = "[]".

% json_write/2 caso in cui l'array non è vuoto
json_write(JSON, StringIn) :-
    jsonarray([Y | Ys]) = JSON,
    !,
    string_concat("", "[", StringIn1),
    json_write_array([Y | Ys], "", StringIn2),
    string_concat(StringIn1, StringIn2, StringIn3),
    string_concat(StringIn3, "]", StringIn).

% json_write_object/3 utilizzata nella json_write per creare le coppie
% da scrivere all'interno del file.json. (Caso in cui non ci sono più
% coppie da scrivere)
json_write_object([], StringIn, Result) :-
    !,
    string_codes(StringIn, StringOutCodes),
    length(StringOutCodes, L),
    ultimo_carattere(StringOutCodes, StringOutCodes3, StringOutCodes2, L),
    primo_carattere(",", StringOutCodes3, StringOutCodes4),
    append(StringOutCodes2, StringOutCodes4, StringOutCodes5),
    string_codes(StringIn2, StringOutCodes5),
    Result = StringIn2.

% json_write_object/3 caso in cui sono ancora presenti coppie da
% scrivere
json_write_object([(X,Y)| Xs], StringIn, Result) :-
    json_write_element(X, StringIn2),
    string_concat(StringIn, StringIn2, StringIn3),
    string_concat(StringIn3, ":", StringIn4),
    json_write_element(Y, StringIn5),
    string_concat(StringIn4, StringIn5, StringIn6),
    string_concat(StringIn6, ",", StringIn7),
    json_write_object(Xs, StringIn7, Result).

% json_write_array/3 utilizzata nella json_write/2 per ricavare gli
% elementi da scrivere all'interno del file.json. (Caso in cui non ci
% sono più elementi da scrivere)
json_write_array([], StringIn, Result) :-
    !,
    string_codes(StringIn, StringOutCodes),
    length(StringOutCodes, L),
    ultimo_carattere(StringOutCodes, StringOutCodes3, StringOutCodes2, L),
    primo_carattere(",", StringOutCodes3, StringOutCodes4),
    append(StringOutCodes2, StringOutCodes4, StringOutCodes5),
    string_codes(StringIn2, StringOutCodes5),
    Result = StringIn2.

% json_write_array/3 caso in cui sono ancora presenti elementi da
% scrivere
json_write_array([X| Xs], StringIn, Result) :-
    json_write_element(X, StringIn2),
    string_concat(StringIn, StringIn2, StringIn3),
    string_concat(StringIn3, ",", StringIn4),
    json_write_array(Xs, StringIn4, Result).

% json_write_element/2 utilizzata all'interno di json_write_array/3, il
% carattere viene convertito in numero
json_write_element(X, Result) :-
    number(X),
    !,
    Result = X.
json_write_element(X, Result) :-
   boolean_trovato(X),
   !,
   Result = X.
json_write_element(X, Result) :-
    json_write(X, Result),
    !.
json_write_element(X, Result) :-
    string(X),
    !,
    string_concat("", "\"", StringIn),
    string_concat(StringIn, X, StringIn2),
    string_concat(StringIn2, "\"", StringIn3),
    Result = StringIn3.

% ultimo_carattere/4 rimuve l'ultimo elemento della lista
ultimo_carattere([], _, _, _) :-
   !.
ultimo_carattere([X|Xs], [X|Xs], [], Length) :-
   string_codes(",", [Char|_]),
   Char = X,
   Length = 1,
   !.
ultimo_carattere([X|Xs], Zs, [X|Ys], Lenght) :-
   succ(App, Lenght),
   ultimo_carattere(Xs, Zs, Ys, App).
