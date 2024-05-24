erlc $1.erl
erl -noshell -s $1 start -s init stop
rm $1.beam
