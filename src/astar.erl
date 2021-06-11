-module(astar).

-compile([inline, inline_list_funcs]).

-export([search/4]).

-define(DIRECTION_TYPE, 8).
-define(MAX_LIMIT, 16#FFFF).

-type grid() :: {integer(), integer()}.
-type result() :: {max, Path :: [grid()]} | none| max_limited.
-type direction_type() :: {direction_type, 4, 6, 8}.
-type max_limit() :: {max_limit, non_neg_integer()}.
-type option() :: direction_type() |max_limit().
-type options() :: [option()].

-spec search(StartGrid, EndGrid, ValidFun, Options) -> Result when
    StartGrid :: grid(), EndGrid :: grid(),
    ValidFun :: fun((CurGrid :: grid()) -> boolean()),
    Options :: options(),
    Result :: result().
search(StartGrid, EndGrid, ValidFun, Options) ->
    Direction = proplists:get_value(direction_type, Options, ?DIRECTION_TYPE),
    DirectionFun = direction_fun(Direction),
    HeuristicFun = heuristic_fun(Direction, StartGrid),
    NeighbourFn = neighbour_fun(DirectionFun, HeuristicFun, ValidFun),
    OpenGrids = push(0, StartGrid, 0, [], new()),
    VisitedGrids = #{StartGrid => true},
    MaxLimit = proplists:get_value(max_limit, Options, ?MAX_LIMIT),
    do_search(EndGrid, NeighbourFn, OpenGrids, VisitedGrids, MaxLimit).

%%=====================================================
%% Internal Function
%%=====================================================
heuristic_fun(6, {X1, Y1}) ->
    Cub1 = evenr_to_cube(X1, Y1),
    fun({X2, Y2}) ->
        Cub2 = evenr_to_cube(X2, Y2),
        cube_distance(Cub1, Cub2)
    end;
heuristic_fun(_, {X1, Y1}) ->
    fun({X2, Y2}) -> erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2) end.

direction_fun(4) ->
    fun(_) -> [{-1, 0}, {0, -1}, {1, 0}, {0, 1}] end;
direction_fun(6) ->
    fun
        (Y) when Y band 1 =:= 0 ->
            [{-1, 0}, {0, 1}, {1, 1}, {1, 0}, {1, -1}, {0, -1}];
        (_) ->
            [{-1, 0}, {-1, 1}, {0, 1}, {1, 0}, {0, -1}, {-1, -1}]
    end;
direction_fun(8) ->
    fun(_) -> [{-1, 0}, {-1, -1}, {0, -1}, {1, -1}, {1, 0}, {1, 1}, {0, 1}, {-1, 1}] end.

evenr_to_cube(Col, Row) ->
    X = Col - ((Row + (Row band 1)) bsr 1),
    Z = Row,
    Y = -X - Z,
    {X, Y, Z}.

cube_distance({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    erlang:max(erlang:abs(X1 - X2), erlang:max(erlang:abs(Y1 - Y2), erlang:abs(Z1 - Z2))).

neighbour_fun(DirectionFun, HeuristicFun, ValidFun) ->
    fun({X, Y} = CGrid, G, Path, OpenGrids, VisitedGrids) ->
        Fun =
            fun({XOffset, YOffset}, {AccOpenGrids, AccVisitedGrids}) ->
                NGrid = {X + XOffset, Y + YOffset},
                case is_unvisited(NGrid, AccVisitedGrids) andalso ValidFun(NGrid) of
                    true ->
                        G1 = G + g(CGrid, NGrid),
                        NewScore = G1 + HeuristicFun(NGrid),
                        NewPath = [NGrid | Path],
                        NewAccVisitedGrids = AccVisitedGrids#{NGrid => true},
                        {push(NewScore, NGrid, G1, NewPath, AccOpenGrids), NewAccVisitedGrids};
                    false ->
                        {AccOpenGrids, AccVisitedGrids}
                end
            end,
        lists:foldl(Fun, {OpenGrids, VisitedGrids}, DirectionFun(Y))
    end.

g({X, _}, {X, _}) ->
    10;
g({_, Y}, {_, Y}) ->
    10;
g(_, _) ->
    14.

is_unvisited(Grid, VisitedGrids) ->
    case VisitedGrids of
        #{Grid := _} ->
            false;
        _ ->
            true
    end.

push(Score, Grid, G, Path, OpenGrids) ->
    insert(Score, {Grid, G, Path}, OpenGrids).

do_search(EndGrid, NeighbourFun, OpenGrids, VisitedGrids, MaxLimit) when MaxLimit > 0 ->
    case take_min(OpenGrids) of
        {{EndGrid, _G, Path}, _NewOpenGrids} ->
            {max, Path};
        {{Grid, G, Path}, NewOpenGrids} ->
            {OpenGrids2, NewVisitedGrids} = NeighbourFun(Grid, G, Path, NewOpenGrids, VisitedGrids),
            do_search(EndGrid, NeighbourFun, OpenGrids2, NewVisitedGrids, MaxLimit - 1);
        empty ->
            none
    end;
do_search(_EndGrid, _NeighbourFun, _OpenGrids, _VisitedGrids, _MaxLimit) ->
    max_limited.

%%======================================
%% pairs_heap implement
%%======================================
new() ->
    {}.

insert(K, V, Heap) ->
    do_merge({K, V, []}, Heap).

take_min({}) ->
    empty;
take_min({_, V, SubHeaps}) ->
    {V, merge_pairs(SubHeaps)}.

do_merge(Heap1, {}) ->
    Heap1;
do_merge({}, Heap2) ->
    Heap2;
do_merge({K1, V1, SubHeap1}, Heap2) when K1 < element(1, Heap2) ->
    {K1, V1, [Heap2 | SubHeap1]};
do_merge(Heap1, {K2, V2, SubHeap2}) ->
    {K2, V2, [Heap1 | SubHeap2]}.

merge_pairs([SH1, SH2 | Rest]) ->
    do_merge(do_merge(SH1, SH2), merge_pairs(Rest));
merge_pairs([SubHeap]) ->
    SubHeap;
merge_pairs([]) ->
    {}.
