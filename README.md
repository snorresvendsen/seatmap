# seatmap
Build rudimentary text/html seat maps in Common Lisp.

A package for handling seat maps for events and venues.

A venue is defined in a text file. Represented as a tree consisting of:

-- Rows - named in a string.

-- Seats - numbered from left or right, or right to left.

See "venue.lisp" for example definition.

An event is a hash table mapped onto a venue. Its keys are symbols on the form
R1.P10
where R (row) is followed by a row label (string),
and P (place/seat) is followed by the place (number).

The value of all seats is set to 0 (zero) by default.

Set a seat value with:
(set-seat 'R1.P1 'occupied)

Set value of mutiple seats with the macro:
(set-seats R1.P1-10 blocked)

Seat values may be any data - symbols, strings, numbers, lists...

The module html.lisp contains forms for printing an event or venue as a html table. Each data element's id tag is set to row and seat number, on the form "Rn.Pm".
