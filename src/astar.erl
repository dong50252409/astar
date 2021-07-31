-module(astar).

-compile(inline).

-export([search/4]).
-export([is_unvisited/2]).


-export_type([grid/0, valid_fun/0, visited_grids/0]).

-type grid() :: {integer(), integer()}.
-type result() :: {max, Path :: [grid()]} | none| max_limited.
-type max_limit() :: {max_limit, non_neg_integer()}.
-type option() :: {astar_mod, module()} |max_limit().
-type options() :: [option()].
-type valid_fun() :: fun((CurGrid :: grid()) -> boolean()).
-type visited_grids() :: #{Grid :: grid() => true}.

-callback(get_neighbours(ValidFun :: valid_fun(), CurGrid :: grid(), VisitedGrids :: visited_grids()) -> Neighbours :: [grid()]).
-callback(heuristic(Grid1 :: grid(), Grid2 :: grid()) -> H :: number()).
-callback(distance(Grid1 :: grid(), Grid2 :: grid()) -> G :: number()).


-define(MAX_LIMIT, 16#FFFF).

-spec search(StartGrid, EndGrid, ValidFun, Options) -> Result when
    StartGrid :: grid(), EndGrid :: grid(),
    ValidFun :: valid_fun(), Options :: options(),
    Result :: result().
search(StartGrid, EndGrid, ValidFun, Options) ->
    OpenGrids = insert(0, {StartGrid, 0, []}, new()),
    VisitedGrids = #{StartGrid => -1},
    AStarMod = proplists:get_value(astar_mod, Options, astar_diagonally),
    MaxLimit = proplists:get_value(max_limit, Options, ?MAX_LIMIT),
    do_search(EndGrid, ValidFun, OpenGrids, VisitedGrids, AStarMod, MaxLimit).

%%=====================================================
%% Internal Function
%%=====================================================
do_search(EndGrid, ValidFun, OpenGrids, VisitedGrids, AStarMod, MaxLimit) when MaxLimit > 0 ->
    case take_min(OpenGrids) of
        {{EndGrid, _G, Path}, _NewOpenGrids} ->
            {max, erlang:tl(lists:reverse([EndGrid | Path]))};
        {{Grid, G, Path}, NewOpenGrids} ->
%%            io:format("take_min: Grid:~w G:~w ~n", [Grid, G]),
            Neighbours = AStarMod:get_neighbours(ValidFun, Grid, VisitedGrids),
            {OpenGrids2, NewVisitedGrids} = add_neighbours(EndGrid, Grid, G, [Grid | Path], NewOpenGrids, VisitedGrids, AStarMod, Neighbours),
            do_search(EndGrid, ValidFun, OpenGrids2, NewVisitedGrids, AStarMod, MaxLimit - 1);
        empty ->
            none
    end;
do_search(_EndGrid, _ValidFun, _OpenGrids, _VisitedGrids, _AStarMod, _MaxLimit) ->
    max_limited.

add_neighbours(EndGrid, ParentGrid, G, Path, OpenGrids, VisitedGrids, AStarMod, [NGrid | T]) ->
    G1 = G + AStarMod:distance(ParentGrid, NGrid),
    NewScore = G1 + AStarMod:heuristic(EndGrid, NGrid),
    case VisitedGrids of
        #{NGrid := OldScore} when OldScore =< NewScore ->
            OpenGrids1 = OpenGrids,
            VisitedGrids1 = VisitedGrids;
        _ ->
%%            io:format("add_neighbours: NGrid:~w Socre:~w H:~w G:~w G1:~w~n", [NGrid, NewScore, AStarMod:heuristic(EndGrid, NGrid), G, G1]),
            OpenGrids1 = insert(NewScore, {NGrid, G1, Path}, OpenGrids),
            VisitedGrids1 = VisitedGrids#{NGrid => NewScore}
    end,
    add_neighbours(EndGrid, ParentGrid, G, Path, OpenGrids1, VisitedGrids1, AStarMod, T);
add_neighbours(_EndGrid, _CurGrid, _G, _Path, OpenGrids, VisitedGrids, _AStarMod, []) ->
    {OpenGrids, VisitedGrids}.
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

-spec is_unvisited(Grid :: grid(), VisitedGrids :: visited_grids()) -> boolean().
is_unvisited(Grid, VisitedGrids) ->
    case VisitedGrids of
        #{Grid := -1} ->
            false;
        _ ->
            true
    end.