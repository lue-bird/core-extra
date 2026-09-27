module String.Extra.IsCapitalized exposing (leftAny, leftAnyReuseSlice, leftAnyShortcutReuseSlice, leftToList, sliceAny, sliceToList, toList, uncons)


uncons : String -> Bool
uncons str =
    case String.uncons str of
        Just ( firstChar, _ ) ->
            Char.isUpper firstChar

        Nothing ->
            False


toList : String -> Bool
toList str =
    case String.toList str of
        firstChar :: _ ->
            Char.isUpper firstChar

        [] ->
            False


leftToList : String -> Bool
leftToList str =
    case String.toList (String.left 2 str) of
        firstChar :: _ ->
            Char.isUpper firstChar

        [] ->
            False


sliceToList : String -> Bool
sliceToList str =
    case String.toList (String.slice 0 2 str) of
        firstChar :: _ ->
            Char.isUpper firstChar

        [] ->
            False


sliceAny : String -> Bool
sliceAny str =
    String.any Char.isUpper
        (String.slice 0
            (if String.any charIsUtf8Surrogate (String.slice 0 1 str) then
                2

             else
                1
            )
            str
        )


leftAny : String -> Bool
leftAny str =
    String.any Char.isUpper
        (String.left
            (if String.any charIsUtf8Surrogate (String.left 1 str) then
                2

             else
                1
            )
            str
        )


leftAnyReuseSlice : String -> Bool
leftAnyReuseSlice str =
    let
        firstCodeUnit : String
        firstCodeUnit =
            String.left 1 str
    in
    String.any Char.isUpper
        (if String.any charIsUtf8Surrogate firstCodeUnit then
            String.left 2 str

         else
            firstCodeUnit
        )


leftAnyShortcutReuseSlice : String -> Bool
leftAnyShortcutReuseSlice str =
    let
        firstCodeUnit : String
        firstCodeUnit =
            String.left 1 str
    in
    String.any Char.isUpper firstCodeUnit
        || (String.any charIsUtf8Surrogate firstCodeUnit
                && String.any Char.isUpper (String.left 2 str)
           )


{-| Some code points like 🔧 are represented as 2 consecutive UTF-16 codes
within js strings.

So when we use `String.slice`, the resulting String might only contain
one of these halves which are called surrogates.

To check for that, the only way to tell whether you've encountered
a surrogate (that I can imagine at least) is by (ab)using that Char.toCode
accesses it's first _2_ indexes if the code at the first index indicates there must be a second half,
leading to NaN being returned.

-}
charIsUtf8Surrogate : Char -> Bool
charIsUtf8Surrogate char =
    Basics.isNaN (Basics.toFloat (Char.toCode char))
