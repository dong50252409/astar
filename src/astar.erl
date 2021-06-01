-module(astar).

-export([heuristic_fun/1, neighbour_fun/3, search/5]).

-define(MAX_LIMIT, 16#FFFF).

-type grid() :: {integer(), integer()}.
-type direction() :: {-1|0|1, -1|0|1}.
-type result() :: none| {max, Path :: [grid()]} | {{max_limited, Path :: [grid()]}}.
-type options() :: [{max_limit, non_neg_integer()}].


-spec search(StartGrid, EndGrid, ValidFun, DirectionList, Options) -> Result when
    StartGrid :: grid(), EndGrid :: grid(),
    ValidFun :: fun((CurGrid :: grid()) -> boolean()),
    DirectionList :: [direction()],
    Options :: options(),
    Result :: result().
search(StarGrid, EndGrid, ValidFun, DirectionList, Options) ->
    HeuristicFun = heuristic_fun(StarGrid),
    NeighbourFn = neighbour_fun(HeuristicFun, ValidFun, DirectionList),
    OpenTrees = push(0, StarGrid, 0, [], gb_trees:empty()),
    ClosedSets = #{},
    MaxLimit = proplists:get_value(max_limit, Options, ?MAX_LIMIT),
    continue(EndGrid, NeighbourFn, OpenTrees, ClosedSets, MaxLimit).

%%=====================================================
%% Internal Function
%%=====================================================
heuristic_fun({X1, Y1}) ->
    fun({X2, Y2}) ->
        X = erlang:abs(X1 - X2),
        Y = erlang:abs(Y1 - Y2),
        math:sqrt(X * X + Y * Y)
    end.

neighbour_fun(HeuristicFun, ValidFun, DirectionList) ->
    fun({X, Y} = CGrid, G, Path, OpenTrees, ClosedSets) ->
        Fun =
            fun({XOffset, YOffset}, AccOpenTrees) ->
                NGrid = {X + XOffset, Y + YOffset},
                case is_close(NGrid, ClosedSets) of
                    true ->
                        AccOpenTrees;
                    false ->
                        G1 = G + g(CGrid, NGrid),
                        NewScore = G1 + HeuristicFun(NGrid),
                        case not is_open(NewScore, NGrid, AccOpenTrees)
                            andalso ValidFun(NGrid) of
                            true ->
                                push(NewScore, NGrid, G1, [NGrid | Path], AccOpenTrees);
                            false ->
                                AccOpenTrees

                        end
                end
            end,
        lists:foldl(Fun, OpenTrees, DirectionList)
    end.

%% G值开销
g({X, _}, {X, _}) -> 1;
g({_, Y}, {_, Y}) -> 1;
g(_, _) -> 1.41421.

is_open(Score, Grid, OpenTrees) ->
    gb_trees:is_defined({Score, Grid}, OpenTrees).

is_close(Grid, CloseSets) ->
    case CloseSets of
        #{Grid := _} ->
            true;
        #{} ->
            false
    end.

push(Score, Grid, G, Path, OpenTrees) ->
    io:format("push:~w~n", [{Score, Grid, G}]),
    gb_trees:insert({Score, Grid}, {G, Path}, OpenTrees).

continue(EndGrid, NeighbourFun, OpenTrees, ClosedSets, MaxLimit) when MaxLimit > 0 ->
    case pop_best(OpenTrees, ClosedSets) of
        none ->
            none;
        {EndGrid, _G, Path, _NewOpenTrees, _NewClosedSets} ->
            {max, Path};
        {Grid, G, Path, NewOpenTrees, NewClosedSets} ->
            OpenTrees2 = NeighbourFun(Grid, G, Path, NewOpenTrees, NewClosedSets),
            continue(EndGrid, NeighbourFun, OpenTrees2, NewClosedSets, MaxLimit - 1)
    end;
continue(_EndGrid, _NeighbourFun, OpenTrees, ClosedSets, _MaxLimit) ->
    {_Grid, _G, Path, _NewOpenTrees, _NewClosedSets} = pop_best(OpenTrees, ClosedSets),
    {max_limited, Path}.

pop_best(OpenTrees, ClosedSets) ->
    case gb_trees:is_empty(OpenTrees) of
        true ->
            none;
        false ->
            {{_BestScore, Grid}, {G, Path}, NewOpenTree} = gb_trees:take_smallest(OpenTrees),
            NewClosedSets = ClosedSets#{Grid => true},
            io:format("pop_best:~w~n", [Grid]),
            {Grid, G, Path, NewOpenTree, NewClosedSets}
    end.

-ifdef(TEST).
-compile(export_all).

-define(pathfinder_test(WorldDiagram, Options), run_pathfinder_test(WorldDiagram, Options)).

pathfinder() ->
    ?pathfinder_test([
        "  S  ",
        "XXXX ",
        "E    "
    ], []).

run_pathfinder_test(WorldDiagram, Options) ->
    {StartGrid, EndGrid, World} = init_word(WorldDiagram),
    MaxX = length(WorldDiagram),
    MaxY = length(lists:nth(1, WorldDiagram)),
    ValidFun =
        fun({X, Y}) ->
            X =< MaxX andalso Y =< MaxY andalso 0 < X andalso 0 < Y
                andalso element(Y, maps:get(X, World)) =/= $X
        end,
    DirectionList = [{-1, 0}, {-1, -1}, {0, -1}, {1, -1}, {1, 0}, {1, 1}, {0, 1}, {-1, 1}],
    case search(StartGrid, EndGrid, ValidFun, DirectionList, Options) of
        {max_limited, Path} ->
            io:format("worklimited path:~w~n", [Path]);
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
    Fun = fun({X, Y}, AccWorld) -> maps:update(X, setelement(Y, maps:get(X, AccWorld), $o), AccWorld) end,
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