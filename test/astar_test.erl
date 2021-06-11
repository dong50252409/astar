-module(astar_test).

-include_lib("eunit/include/eunit.hrl").

-define(pathfinder_test(A, B, C), run_pathfinder_test(A, B, C)).

pathfinder_1_test_() ->
    [
        ?pathfinder_test([
            "   S                ",
            "       XXXXXXXXXXX  ",
            "   XXXXX  E X       ",
            "      X             ",
            "      XXXXXXX       ",
            "                    "
        ], max_limited, [{max_limit, 50}]),
        ?pathfinder_test([
            "   S                ",
            "       XXXXXXXXXXX  ",
            "   XXXXX  E X       ",
            "      X     X       ",
            "      XXXXXXX       ",
            "                    "
        ], none, [{max_limit, 1000}]),
        ?pathfinder_test([
            "   S                ",
            "       XXXXXXXXXXX  ",
            "   XXXXX            ",
            "                    ",
            "                    ",
            "           E        "
        ], 15, [{direction_type, 4}]),
        ?pathfinder_test([
            "   S                ",
            "       XXXXXXXXXXX  ",
            "   XXXXX            ",
            "                    ",
            "                    ",
            "           E        "
        ], 12, [{direction_type, 6}]),
        ?pathfinder_test([
            "   S                ",
            "       XXXXXXXXXXX  ",
            "   XXXXX            ",
            "                    ",
            "                    ",
            "           E        "
        ], 11, [{direction_type, 8}]),
        ?pathfinder_test([
            "S                               X   XE  ",
            "XXXXXX XXXXXXXX XXXXXXXXXXXXXXXXX X XX  ",
            "            X                     X  X  ",
            "XX XXXXXXX  XXXXXXXXXXXXXXXXXXXXXXX XX  ",
            "         X                X          X  ",
            "X XXXXX XXXXXXXXXXXXXXXX XX XXXXXXXXXX  ",
            "      X X X         X     X      X      ",
            " XXXX X X X         X XXXXXXXXXXXX  XXXX",
            "   X  X X X         X   X            X  ",
            "  XX  X X X XXXXX   XXXXX  XXXXXXXXXXX  ",
            " XX   X X X     X              X        ",
            "  X X X X X XXXXXXXXXXXXXXXXXXXX        ",
            " XX X X X X                 X           ",
            "  X X     X                 X      X    ",
            "X X XXXXXXX                             ",
            "  X                          XXXX       ",
            " XXXX   XXXXXXXXXXXXXXXX        XXX     ",
            "  X                             X       ",
            "  XXXXXXXX                              ",
            "                                        "
        ], 68, [{direction_type, 8}]),
        ?pathfinder_test([
            "                                                            ",
            " S  XXX                                                     ",
            "    X                                                       ",
            " XXXX                                                       ",
            "                                                            ",
            "                                                            ",
            "              XXXX                                          ",
            "            XXXXXXXXX                                       ",
            "         XXXXXXXX                                           ",
            "      XXXXXXXX                           XX                 ",
            "                                         XX                 ",
            "                     X                   XX                 ",
            "                    XXX                  XX                 ",
            " XXXXXXXXXXXXX      XXX                  XX                 ",
            "    XXXXXX           X                   XX                 ",
            "      XX                                 XX                 ",
            "                                         XX                 ",
            "                                         XX                 ",
            "                 XXXXXX                  XX                 ",
            "                XXXXXXXXXXXX             XXX                ",
            "                    XXXXXXX         XXXXXXXX                ",
            "                                  XXXXXXXXXXX               ",
            "                                 XXXXXXXXXXXXXX             ",
            "                               XXXXXXXXXX                   ",
            "                       XXXXXXXXXXXXXXXXXXX                  ",
            "                               XXXXXXXXXXXXXX               ",
            "                                          XXXXXXXX          ",
            "                                                            ",
            "                                                            ",
            "                                                            ",
            "                                         X                  ",
            "                                         X                  ",
            "                                        XX                  ",
            "         XXX                         XXXX                   ",
            "       XXXXXXXX                XXXXXXXX                     ",
            "     XXXXXXXXXXX      XXXXXXXXXXXXXX                        ",
            "  XXXXXXXXXXXXXXXXXXXXXXXXX                                 ",
            "      XXXXXXXXXXXXXXX                                       ",
            "                                                       X    ",
            "                                                      XX    ",
            "                                                    XXX     ",
            "               XXXXXX                           XXXXXXX     ",
            "                 XXX                XXXXXXXXXXXXXXXXX       ",
            "                               XXXXXXXXXXXXXXXXXXX          ",
            "                                                            ",
            "                                                            ",
            "                     XXXXXXXXXXXXXXX                        ",
            "                 XXXXXXX        XXXX                        ",
            "             XXXXXXX              XX                        ",
            "         XXXXXXXXXXXXXX            X            XXXXXXXXXXXX",
            "           XXXXXXXXXXXXXX                                   ",
            "                                                            ",
            "                                       XXXXXXXXXXXXX        ",
            "                                      XXXXXXXXXXXXXXXXXXXX  ",
            "                                     XXXXXXXXXXXXXXX        ",
            "                                    XXXXXXXXXXXX            ",
            "                                           XXX              ",
            "                                            X               ",
            "                                                          E ",
            "                                                            "
        ], 97,[{direction_type, 8}])
    ].

run_pathfinder_test(WorldDiagram, ExpectedValue, Options) ->
    MaxX = length(WorldDiagram),
    MaxY = length(lists:nth(1, WorldDiagram)),
    {StartGrid, EndGrid, World} = init_word(WorldDiagram),
    ValidFun =
        fun({X, Y}) ->
            X =< MaxX andalso Y =< MaxY andalso 0 < X andalso 0 < Y
                andalso element(Y, maps:get(X, World)) =/= $X
        end,
    case astar:search(StartGrid, EndGrid, ValidFun, Options) of
        {max, Path} ->
            drw_world(Path, World),
            ?assertMatch(ExpectedValue, length(Path));
        RealValue ->
            ?assertMatch(ExpectedValue, RealValue)
    end.

init_word(WorldDiagram) ->
    init_word_1(WorldDiagram, 1, none, none, #{}).

init_word_1([Line | T], Row, StarGird, EndGrid, WorldMap) ->
    case find_grid_col($S, Line, 1) of
        none ->
            StarGird1 = StarGird;
        Col1 ->
            StarGird1 = {Row, Col1}
    end,
    case find_grid_col($E, Line, 1) of
        none ->
            EndGrid1 = EndGrid;
        Col2 ->
            EndGrid1 = {Row, Col2}
    end,
    WorldMap1 = WorldMap#{Row => list_to_tuple(Line)},
    init_word_1(T, Row + 1, StarGird1, EndGrid1, WorldMap1);
init_word_1([], _Row, StartGird, EenGrid, WordMap) ->
    {StartGird, EenGrid, WordMap}.

find_grid_col(C, [C | _], Col) ->
    Col;
find_grid_col(C, [_ | T], Col) ->
    find_grid_col(C, T, Col + 1);
find_grid_col(_C, [], _Col) ->
    none.

drw_world(Path, World) ->
    Fun = fun({X, Y}, AccWorld) -> maps:update(X, setelement(Y, maps:get(X, AccWorld), $o), AccWorld) end,
    NewWorld = lists:foldl(Fun, World, Path),
    drw_world_1(1, NewWorld).
drw_world_1(Row, World) ->
    case World of
        #{Row := Col} ->
            ?debugFmt("~s~n", [tuple_to_list(Col)]),
            drw_world_1(Row + 1, World);
        #{} ->
            ok
    end.