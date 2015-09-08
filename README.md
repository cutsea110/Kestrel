% README
% Katsutoshi Itoh
% 2010-11-28

Kestrel
=======

Kestrel(K combinator bird) is a WIKI clone.
This means that **WIKI** is an expression which can be reduced to **K**.

Kestrel is developped on [yesod] framework.

[yesod]: http://docs.yesodweb.com/


# for DEBUG

stack build
stack exec -- yesod devel --port 3002

# for Product

stack build
./owl

