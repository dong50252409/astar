%%%-------------------------------------------------------------------
%%% @author dy
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 多边形寻路
%%% @end
%%% Created : 29. 7月 2021 16:56
%%%-------------------------------------------------------------------
-module(astar_polygonal).

-behavior(astar).

%% API
-export([get_neighbours/3, heuristic/2, distance/2]).

-spec get_neighbours(ValidFun :: astar:valid_fun(), CurGrid :: astar:grid(), VisitedGrids :: astar:visited_grids()) -> Neighbours :: [astar:grid()].
get_neighbours(ValidFun, CurGrid, VisitedGrids) ->
    Directions = directions(),
    get_neighbours_1(ValidFun, CurGrid, VisitedGrids, Directions).

get_neighbours_1(ValidFun, {X, Y} = CurGrid, VisitedGrids, [{DX, DY} | T]) ->
    NGrid = {X + DX, Y + DY},
    case astar:is_unvisited(NGrid, VisitedGrids) andalso ValidFun(NGrid)
        andalso (DX =:= 0 orelse DY =:= 0 orelse (ValidFun({X + DX, Y}) orelse ValidFun({X, Y + DY}))) of
        true ->
            [NGrid | get_neighbours_1(ValidFun, CurGrid, VisitedGrids, T)];
        false ->
            get_neighbours_1(ValidFun, CurGrid, VisitedGrids, T)
    end;
get_neighbours_1(_ValidFun, _CurGrid, _VisitedGrids, []) ->
    [].

directions() ->
    [{-1, 0}, {-1, -1}, {0, -1}, {1, -1}, {1, 0}, {1, 1}, {0, 1}, {-1, 1}].

-spec heuristic(Grid1 :: astar:grid(), Grid2 :: astar:grid()) -> float().
heuristic(Grid1, Grid2) ->
    astar_heuristic:octile(Grid1, Grid2).

-spec distance(Grid1 :: astar:grid(), Grid2 :: astar:grid()) -> number().
distance({X, _}, {X, _}) ->
    1;
distance({_, Y}, {_, Y}) ->
    1;
distance(_, _) ->
    1.414.