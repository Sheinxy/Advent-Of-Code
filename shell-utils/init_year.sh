#!/bin/sh

YEAR=`date +%Y`

until [ $# -eq 0 ]; do
    case $1 in
        -y|--year)    YEAR="$2"; shift;;
        *)            YEAR="$1";;
    esac
    shift
done

mkdir $YEAR

if ! fgrep "executable AOC$YEAR" Advent-Of-Code.cabal; then
cat >> Advent-Of-Code.cabal <<EOF
executable AOC$YEAR
    import:           warnings, days
    main-is:          Main.hs
    -- other-modules:
    -- other-extensions:
    build-depends:
        base ^>=4.17.2.1,
        criterion-measurement,
        ghc,
        split,
        Advent-Of-Code

    hs-source-dirs:   $YEAR/app, $YEAR/src
    default-language: Haskell2010

EOF
fi

./shell-utils/generate_main.sh $YEAR
./shell-utils/generate_days.sh $YEAR

cd $YEAR

stylish-haskell -i **/Main.hs
