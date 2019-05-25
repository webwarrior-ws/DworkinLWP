namespace Settings


type Randomizeable<'T> =
    | Randomized
    | Defined of 'T

