#!/bin/sh

make_args () {
    ARGS=""
    SEP=""
    VAR="A"
    for i in $(seq 0 $ARITY); do
        ARGS="$ARGS$SEP$VAR$i"
        SEP=","
    done
}

EXPORTS="-export(["
FUNCTIONS=""

while read line; do
    MOD=$(expr "$line" : '^\(.*\):')
    FUN=$(expr "$line" : '^.*:\(.*\)/')
    ARITY=$(expr "$line" : '^.*/\([0-9]*\)')

    EXPORTS="$EXPORTS$COMMA$FUN/$ARITY"
    COMMA=","

    make_args

    FUNCTIONS="$FUNCTIONS\n$FUN($ARGS)->crashfy($MOD:$FUN($ARGS))."
done

EXPORTS="$EXPORTS])."

echo $EXPORTS
echo $FUNCTIONS
