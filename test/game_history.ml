open Backgammon

type play = Location.source * Location.dest

type turn = {dice: int * int;
             plays: play list}
