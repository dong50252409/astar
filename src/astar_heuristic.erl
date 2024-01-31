%%%-------------------------------------------------------------------
%%% @author dy
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 启发函数
%%% @end
%%% Created : 29. 7月 2021 16:52
%%%-------------------------------------------------------------------
-module(astar_heuristic).

%% API
-export([
    manhattan/2, euclidean/2, octile/2, chebyshev/2,
    cube_chebyshev/2    % hexagonal
]).

-spec manhattan(Grid1 :: astar:grid(), Grid2 :: astar:grid()) -> integer().
manhattan({X1, Y1}, {X2, Y2}) ->
    erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2).

-spec euclidean(Grid1 :: astar:grid(), Grid2 :: astar:grid()) -> float().
euclidean({X1, Y1}, {X2, Y2}) ->
    DX = erlang:abs(X1 - X2),
    DY = erlang:abs(Y1 - Y2),
    math:sqrt(DX * DX + DY * DY).

-spec octile(Grid1 :: astar:grid(), Grid2 :: astar:grid()) -> float().
octile({X1, Y1}, {X2, Y2}) ->
    F = 0.4142135,
    DX = erlang:abs(X1 - X2),
    DY = erlang:abs(Y1 - Y2),
    F * erlang:min(DX, DY) + erlang:max(DX, DY).

-spec chebyshev(Grid1 :: astar:grid(), Grid2 :: astar:grid()) -> integer().
chebyshev({X1, Y1}, {X2, Y2}) ->
    DX = erlang:abs(X1 - X2),
    DY = erlang:abs(Y1 - Y2),
    erlang:max(DX, DY).

-spec cube_chebyshev(Grid1 :: astar:grid(), Grid2 :: astar:grid()) -> integer().
cube_chebyshev(Grid1, Grid2) ->
    {X1, Y1, Z1} = evenr_to_cube(Grid1),
    {X2, Y2, Z2} = evenr_to_cube(Grid2),
    erlang:max(erlang:abs(X1 - X2), erlang:max(erlang:abs(Y1 - Y2), erlang:abs(Z1 - Z2))).

evenr_to_cube({X, Y}) ->
    DX = X - ((Y + (Y band 1)) bsr 1),
    DZ = Y,
    DY = -X - DZ,
    {DX, DY, DZ}.