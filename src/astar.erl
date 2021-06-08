-module(astar).

-export([search/4]).

-define(MAX_LIMIT, 16#FFFF).
-define(DISTANCE_FUNCTION, manhattan).

-type grid() :: {integer(), integer()}.
-type result() :: none| {max, Path :: [grid()]} | {{max_limited, Path :: [grid()]}}.
-type option() :: {max_limit, non_neg_integer()} | {distance_function, manhattan|euclidean}.
-type options() :: [option()].


-spec search(StartGrid, EndGrid, ValidFun, Options) -> Result when
    StartGrid :: grid(), EndGrid :: grid(),
    ValidFun :: fun((CurGrid :: grid()) -> boolean()),
    Options :: options(),
    Result :: result().
search(StarGrid, EndGrid, ValidFun, Options) ->
    DistanceFunction = proplists:get_value(distance_function, Options, ?DISTANCE_FUNCTION),
    {HeuristicFun, DirectionList} = heuristic_fun(StarGrid, DistanceFunction),
    NeighbourFn = neighbour_fun(HeuristicFun, ValidFun, DirectionList),
    OpenGrids = push(0, StarGrid, 0, [], gb_trees:empty()),
    ClosedGrids = #{},
    MaxLimit = proplists:get_value(max_limit, Options, ?MAX_LIMIT),
    continue(EndGrid, NeighbourFn, OpenGrids, ClosedGrids, MaxLimit).

%%=====================================================
%% Internal Function
%%=====================================================
heuristic_fun({X1, Y1}, manhattan) ->
    F =
        fun({X2, Y2}) ->
            erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2)
        end,
    DirectionList = [{-1, 0}, {0, -1}, {1, 0}, {0, 1}],
    {F, DirectionList};

heuristic_fun({X1, Y1}, euclidean) ->
    F =
        fun({X2, Y2}) ->
            X = X1 - X2,
            Y = Y1 - Y2,
            erlang:trunc(math:sqrt(X * X + Y * Y) * 10)
        end,
    DirectionList = [{-1, 0}, {-1, -1}, {0, -1}, {1, -1}, {1, 0}, {1, 1}, {0, 1}, {-1, 1}],
    {F, DirectionList}.

neighbour_fun(HeuristicFun, ValidFun, DirectionList) ->
    fun({X, Y} = CGrid, G, Path, OpenGrids, ClosedGrids) ->
        Fun =
            fun({XOffset, YOffset}, AccOpenGrids) ->
                NGrid = {X + XOffset, Y + YOffset},
                case is_closed(NGrid, ClosedGrids) of
                    true ->
                        AccOpenGrids;
                    false ->
                        G1 = G + g(CGrid, NGrid),
                        NewScore = G1 + HeuristicFun(NGrid),
                        case not is_open(NewScore, NGrid, AccOpenGrids)
                            andalso ValidFun(NGrid) of
                            true ->
                                push(NewScore, NGrid, G1, [NGrid | Path], AccOpenGrids);
                            false ->
                                AccOpenGrids

                        end
                end
            end,
        lists:foldl(Fun, OpenGrids, DirectionList)
    end.

g({X, _}, {X, _}) ->
    10;
g({_, Y}, {_, Y}) ->
    10;
g(_, _) ->
    14.

is_open(Score, Grid, OpenGrids) ->
    gb_trees:is_defined({Score, Grid}, OpenGrids).

is_closed(Grid, ClosedGrids) ->
    case ClosedGrids of
        #{Grid := _} ->
            true;
        #{} ->
            false
    end.

push(Score, Grid, G, Path, OpenGrids) ->
    gb_trees:insert({Score, Grid}, {G, Path}, OpenGrids).

continue(EndGrid, NeighbourFun, OpenGrids, ClosedGrids, MaxLimit) when MaxLimit > 0 ->
    case pop_best(OpenGrids, ClosedGrids) of
        none ->
            none;
        {EndGrid, _G, Path, _NewOpenGrids, _NewClosedGrids} ->
            {max, Path};
        {Grid, G, Path, NewOpenGrids, NewClosedGrids} ->
            OpenGrids2 = NeighbourFun(Grid, G, Path, NewOpenGrids, NewClosedGrids),
            continue(EndGrid, NeighbourFun, OpenGrids2, NewClosedGrids, MaxLimit - 1)
    end;
continue(_EndGrid, _NeighbourFun, _OpenGrids, _ClosedGrids, _MaxLimit) ->
    max_limited.

pop_best(OpenGrids, ClosedGrids) ->
    case gb_trees:is_empty(OpenGrids) of
        true ->
            none;
        false ->
            {{_BestScore, Grid}, {G, Path}, NewOpenTree} = gb_trees:take_smallest(OpenGrids),
            NewClosedGrids = ClosedGrids#{Grid => true},
            {Grid, G, Path, NewOpenTree, NewClosedGrids}
    end.

-ifdef(TEST).
-compile(export_all).

-define(pathfinder_test(WorldDiagram, Options), run_pathfinder_test(WorldDiagram, Options)).

pathfinder() ->
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
    ], [{distance_function, euclidean}]).
%%    ?pathfinder_test([
%%        "  S  ",
%%        "XXXX ",
%%        "E    "
%%    ], []).

run_pathfinder_test(WorldDiagram, Options) ->
    {StartGrid, EndGrid, World} = init_word(WorldDiagram),
    MaxX = length(WorldDiagram),
    MaxY = length(lists:nth(1, WorldDiagram)),
    ValidFun =
        fun({X, Y}) ->
            X =< MaxX andalso Y =< MaxY andalso 0 < X andalso 0 < Y
                andalso element(Y, maps:get(X, World)) =/= $X
        end,
    case search(StartGrid, EndGrid, ValidFun, Options) of
        max_limited ->
            io:format("worklimited~n", []);
        {max, Path} ->
            drw_world(Path, World);
        none ->
            io:format("Optimal path: none", [])
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
    Fun = fun({X, Y}, AccWorld) ->
        maps:update(X, setelement(Y, maps:get(X, AccWorld), $o), AccWorld) end,
    NewWorld = lists:foldl(Fun, World, Path),
    drw_world_1(1, NewWorld).
drw_world_1(Row, World) ->
    case World of
        #{Row := Col} ->
            io:format("~s~n", [tuple_to_list(Col)]),
            drw_world_1(Row + 1, World);
        #{} ->
            ok
    end.

-endif.